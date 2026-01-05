getwd()
setwd("~/Desktop/PredictionChallenge2")


install.packages("ggplot2")

library(rpart)
library(rpart.plot) # Visualization for decision trees
library(ranger)
library(caret)
library(doParallel)
library(corrplot)
library(reshape2)
library(ggplot2)


train <- read.csv("earnings_train.csv")
test  <- read.csv("earnings_test.csv")

head(train)
str(train)
summary(train)
na_counts <- colSums(is.na(train))
na_counts
unique(train$Major)

if (!"Earnings" %in% colnames(train)) {
  stop("Target column 'Earnings' not found in training data. Choose an existing column as target.")
}

colnum <- c("GPA", "Height", "Number_Of_Professional_Connections",
            "Graduation_Year", "Number_Of_Credits", "Number_Of_Parking_Tickets")

for (col in colnum) {
  if (col %in% colnames(train)) {
    train[[col]] <- as.numeric(as.character(train[[col]]))
  }
  if (col %in% colnames(test)) {
    test[[col]] <- as.numeric(as.character(test[[col]]))
  }
}

colcat <- c("Major")
for (col in colcat) {
  if (col %in% colnames(train)) {
    train[[col]] <- as.factor(train[[col]])
  }
  if (col %in% colnames(test)) {
    test[[col]] <- as.factor(test[[col]])
  }
}

# =====================
# 2. feature create 
# =====================

train$Buisness_GPA_Even <- ifelse(train$Major == "Buisness" &
                                    train$Graduation_Year %% 2 == 0,
                                  train$GPA, 0)

train$Buisness_GPA_Odd  <- ifelse(train$Major == "Buisness" &
                                    train$Graduation_Year %% 2 != 0,
                                  train$GPA, 0)

train$Other_Conn_Sq <- ifelse(train$Major == "Other",
                              train$Number_Of_Professional_Connections^2, 0)

train$Conn <- train$Number_Of_Professional_Connections

# =====================
# 3. plot to explore
# =====================

## 3.1 Scatter GPA vs Earnings, facet theo Major
p1 <- ggplot(train, aes(x = GPA, y = Earnings)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Major, scales = "free") +
  theme_minimal() +
  labs(title = "Earnings vs GPA by Major",
       subtitle = "Distinct patterns across majors")
print(p1)

## 3.2 Boxplot Earnings theo Major
p2 <- ggplot(train, aes(x = Major, y = Earnings, fill = Major)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Earnings by Major")
print(p2)

## 3.3 GPA vs Earnings với slope khác nhau theo Major
p3 <- ggplot(train, aes(x = GPA, y = Earnings, color = Major)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Different Slopes of GPA vs Earnings by Major")
print(p3)

## 3.4 Connections vs Earnings across Majors
p4 <- ggplot(train, aes(x = Number_Of_Professional_Connections,
                        y = Earnings, color = Major)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Connections vs Earnings across Majors",
       x = "Connections", y = "Earnings")
print(p4)


# STEM, Humanities, Vocational, Professional: GPA + GPA^2
stem <- subset(train, Major == "STEM")
hum  <- subset(train, Major == "Humanities")
voc  <- subset(train, Major == "Vocational")
prof <- subset(train, Major == "Professional")

stem$GPA2 <- stem$GPA^2
hum$GPA2  <- hum$GPA^2
voc$GPA2  <- voc$GPA^2
prof$GPA2 <- prof$GPA^2

mod_stem <- lm(Earnings ~ GPA + GPA2, data = stem)
mod_hum  <- lm(Earnings ~ GPA + GPA2, data = hum)
mod_voc  <- lm(Earnings ~ GPA + GPA2, data = voc)
mod_prof <- lm(Earnings ~ GPA + GPA2, data = prof)

# Other: LogConn + Conn2
other <- subset(train, Major == "Other")
other$Conn    <- other$Number_Of_Professional_Connections
other$Conn2   <- other$Conn^2
other$LogConn <- log(other$Conn + 1)
mod_other <- lm(Earnings ~ LogConn + Conn2, data = other)

# Business: Conn + Conn2
biz <- subset(train, Major == "Buisness")
biz$Conn  <- biz$Number_Of_Professional_Connections
biz$Conn2 <- biz$Conn^2
mod_bus <- lm(Earnings ~ Conn + Conn2, data = biz)

## 3.6 Diagnostics plots
par(mfrow = c(2, 2))
plot(mod_stem, main = "STEM Model Diagnostics")
plot(mod_hum,  main = "Humanities Model Diagnostics")
plot(mod_voc,  main = "Vocational Model Diagnostics")
plot(mod_prof, main = "Professional Model Diagnostics")

par(mfrow = c(1, 2))
plot(mod_other, main = "Other Model Diagnostics")
plot(mod_bus,   main = "Business Model Diagnostics")

par(mfrow = c(1, 1))

## 3.7 Business: GPA vs Earnings  Even/Odd Year
p5 <- ggplot(biz, aes(x = GPA, y = Earnings,
                      color = factor(Graduation_Year %% 2))) +
  geom_point(size = 2, alpha = .6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Business: GPA vs Earnings by Even/Odd Graduation Year",
       color = "Even/Odd")
print(p5)

## 3.8 Business: Quadratic pattern theo Connections
p6 <- ggplot(biz, aes(x = Conn, y = Earnings)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              color = "red") +
  theme_minimal() +
  labs(title = "Business: Quadratic Pattern in Connections",
       x = "Connections", y = "Earnings")
print(p6)

# =====================
# 4. BIG-LM MODEL
# =====================

model <- lm(Earnings ~ 0 + Major + 
              # Standard GPA slopes for normal majors (interaction term)
              Major:GPA + 
              # Special slope features for Business
              Buisness_GPA_Even + 
              Buisness_GPA_Odd +
              # Special quadratic feature for Other
              Other_Conn_Sq, 
            data = train)

print(summary(model))

train_preds <- predict(model, newdata = train)
mse_train <- mean((train$Earnings - train_preds)^2)
cat("Training MSE (Target is approx 0):", mse_train, "\n")

# =====================
# 5. 5-Fold Cross Validation
# =====================

set.seed(123) 
k <- 5
folds   <- sample(rep(1:k, length.out = nrow(train)))
cv_mses <- numeric(k)

cat("\nRunning 5-Fold Cross Validation...\n")
for (i in 1:k) {
  cv_train <- train[folds != i, ]
  cv_valid <- train[folds == i, ]
  
  cv_model <- lm(Earnings ~ 0 + Major + Major:GPA +
                   Buisness_GPA_Even + Buisness_GPA_Odd +
                   Other_Conn_Sq,
                 data = cv_train)
  
  cv_preds <- predict(cv_model, newdata = cv_valid)
  
  cv_mses[i] <- mean((cv_valid$Earnings - cv_preds)^2)
}

cat("\n5-Fold CV Average MSE:", mean(cv_mses), "\n")

# =====================
# 6. Predict  test + submission
# =====================

test$Major <- as.factor(test$Major)
test$Major <- factor(test$Major, levels = levels(train$Major))

# create feature the same to train
test$Buisness_GPA_Even <- ifelse(test$Major == "Buisness" &
                                   test$Graduation_Year %% 2 == 0,
                                 test$GPA, 0)
test$Buisness_GPA_Odd  <- ifelse(test$Major == "Buisness" &
                                   test$Graduation_Year %% 2 != 0,
                                 test$GPA, 0)

test$Other_Conn_Sq <- ifelse(test$Major == "Other",
                             test$Number_Of_Professional_Connections^2, 0)

# Predict
test_preds <- predict(model, newdata = test)

# submission
submission <- data.frame(
  ID       = 1:nrow(test),
  Earnings = test_preds
)

write.csv(submission, "submission.csv", row.names = FALSE)
