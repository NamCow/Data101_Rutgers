## ============================================
## 1. read dataset and increase number of row
## ============================================

set.seed(2025)
setwd("/Users/lainam/Desktop")  

df_raw <- read.csv("Students_Social_Media_Addiction.csv",
                   stringsAsFactors = FALSE)

N_ROWS <- 10000
idx <- sample(seq_len(nrow(df_raw)), size = N_ROWS, replace = TRUE)
df <- df_raw[idx, ]
rownames(df) <- NULL

df$Student_ID <- seq_len(nrow(df))

## ============================================
## 2. Scramble column 
## ============================================

cols_to_scramble <- c(
  "Country",
  "Avg_Daily_Usage_Hours",
  "Most_Used_Platform",
  "Affects_Academic_Performance", 
  "Sleep_Hours_Per_Night",
  "Mental_Health_Score",
  "Relationship_Status",
  "Conflicts_Over_Social_Media",
  "Addicted_Score"
)

for (nm in cols_to_scramble) {
  if (nm %in% names(df)) {
    df[[nm]] <- sample(df[[nm]], size = nrow(df), replace = FALSE)
  }
}

## ============================================
## 3. Hidden features CORE (6 main features)
## ============================================

Usage_to_Sleep_Ratio <- df$Avg_Daily_Usage_Hours / df$Sleep_Hours_Per_Night

Sleep_Debt        <- pmax(0, 7 - df$Sleep_Hours_Per_Night)        # lack of sleep with 7h
Usage_Over_4      <- pmax(0, df$Avg_Daily_Usage_Hours - 4)        # usage > 4h
Ratio_Excess      <- pmax(0, Usage_to_Sleep_Ratio - 0.8)          # usage/sleep too high
Mental_Struggle   <- pmax(0, 6 - df$Mental_Health_Score)          # low mental health
Conflict_Intensity <- df$Conflicts_Over_Social_Media              # conflict

n <- nrow(df)
risk_score <- rep(0, n)

## ============================================
## 4. RULES THEO ACADEMIC_LEVEL
##    Target conceptual: Social Media Burnout
## ============================================

is_hs <- df$Academic_Level == "High School"
is_ug <- df$Academic_Level == "Undergraduate"
is_gr <- df$Academic_Level == "Graduate"

## 4.1 High School – Sleep_Debt + usage + ratio
risk_score[is_hs] <-
  0.9 * Sleep_Debt[is_hs] +
  0.7 * Usage_Over_4[is_hs] +
  0.4 * Ratio_Excess[is_hs]

## 4.2 Undergraduate – mix sleep + usage + mental + conflict
risk_score[is_ug] <-
  1.0 * Sleep_Debt[is_ug] +
  0.8 * Usage_Over_4[is_ug] +
  0.7 * Ratio_Excess[is_ug] +
  0.9 * Mental_Struggle[is_ug] +
  0.7 * Conflict_Intensity[is_ug]

# Relationship interaction for Undergrad
in_rel_ug <- is_ug & df$Relationship_Status == "In Relationship"
risk_score[in_rel_ug] <- risk_score[in_rel_ug] +
  0.3 * Conflict_Intensity[in_rel_ug]

comp_ug <- is_ug & df$Relationship_Status == "Complicated"
risk_score[comp_ug] <- risk_score[comp_ug] +
  0.6 * Conflict_Intensity[comp_ug] +
  0.3 * Mental_Struggle[comp_ug]

## 4.3 Graduate – weight on mental + Sleep_Debt + usage
risk_score[is_gr] <-
  1.1 * Sleep_Debt[is_gr] +
  1.1 * Mental_Struggle[is_gr] +
  0.8 * Usage_Over_4[is_gr] +
  0.4 * Ratio_Excess[is_gr]

## ============================================
## 5. Platform effects
## ============================================

short_video <- df$Most_Used_Platform %in% c("Instagram", "TikTok")
risk_score[short_video] <- risk_score[short_video] +
  0.4 + 0.1 * Usage_Over_4[short_video]

linked <- df$Most_Used_Platform == "LinkedIn"
risk_score[linked] <- risk_score[linked] - 0.6


## ============================================
## 6. Create noise + 3 level of burnout 
## ============================================

# Chia thành 3 nhóm qua quantiles
q_low  <- as.numeric(quantile(risk_score, probs = 0.33))
q_high <- as.numeric(quantile(risk_score, probs = 0.67))

burnout_det <- ifelse(
  risk_score < q_low, "Low",
  ifelse(risk_score < q_high, "Moderate", "High")
)

burnout_det <- factor(burnout_det,
                      levels = c("Low", "Moderate", "High"))

# Add noise: ~5% 
set.seed(2025)
flip_idx <- sample(seq_len(n), size = round(0.05 * n))

burnout_noisy <- burnout_det

for (i in flip_idx) {
  current <- burnout_det[i]
  if (current == "Low") {
    burnout_noisy[i] <- "Moderate"
  } else if (current == "High") {
    burnout_noisy[i] <- "Moderate"
  } else if (current == "Moderate") {
    if (runif(1) < 0.5) {
      burnout_noisy[i] <- "Low"
    } else {
      burnout_noisy[i] <- "High"
    }
  }
}

df$Social_Media_Burnout <- factor(burnout_noisy,
                                  levels = c("Low", "Moderate", "High"))

## ============================================
## 7. Drop Affects_Academic_Performance 
## ============================================

if ("Affects_Academic_Performance" %in% names(df)) {
  df$Affects_Academic_Performance <- NULL
}

## ============================================
## 8. test (90% / 10%) and save CSV
## ============================================

set.seed(2025)
train_idx <- sample(seq_len(n), size = round(0.9 * n))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

write.csv(train, "students_burnout_train.csv", row.names = FALSE)
write.csv(test,  "students_burnout_test.csv",  row.names = FALSE)




set.seed(2025)
setwd("/Users/lainam/Desktop/hw4") 

train <- read.csv("students_burnout_train.csv", stringsAsFactors = FALSE)
test  <- read.csv("students_burnout_test.csv",  stringsAsFactors = FALSE)

# Chuyển factor
factor_cols <- c(
  "Gender",
  "Academic_Level",
  "Most_Used_Platform",
  "Relationship_Status",
  "Country",
  "Social_Media_Burnout"
)

for (nm in factor_cols) {
  if (nm %in% names(train)) {
    train[[nm]] <- as.factor(train[[nm]])
    test[[nm]]  <- as.factor(test[[nm]])
  }
}

train$Student_ID <- NULL
test$Student_ID  <- NULL

prop.table(table(train$Social_Media_Burnout))


#simple Rpart model
library(rpart)
library(rpart.plot)

tree_baseline <- rpart(
  Social_Media_Burnout ~ Age +
    Avg_Daily_Usage_Hours +
    Sleep_Hours_Per_Night +
    Mental_Health_Score +
    Conflicts_Over_Social_Media +
    Addicted_Score,
  data   = train,
  method = "class",
  control = rpart.control(cp = 0.01, minbucket = 30)
)

rpart.plot(tree_baseline)

pred_train_tree <- predict(tree_baseline, type = "class")
mean(pred_train_tree == train$Social_Media_Burnout)

pred_test_tree <- predict(tree_baseline, newdata = test, type = "class")
mean(pred_test_tree == test$Social_Media_Burnout)

table(Predicted = pred_test_tree,
      Actual    = test$Social_Media_Burnout)






#simple multinom model

library(nnet)

mlogit_baseline <- multinom(
  Social_Media_Burnout ~
    Avg_Daily_Usage_Hours +
    Sleep_Hours_Per_Night +
    Conflicts_Over_Social_Media +
    Mental_Health_Score +
    Academic_Level +
    Gender,
  data = train
)

summary(mlogit_baseline)

pred_train_mlogit <- predict(mlogit_baseline, type = "class")
mean(pred_train_mlogit == train$Social_Media_Burnout)

pred_test_mlogit <- predict(mlogit_baseline, newdata = test, type = "class")
mean(pred_test_mlogit == test$Social_Media_Burnout)

table(Predicted = pred_test_mlogit,
      Actual    = test$Social_Media_Burnout)





#meta random forest with recommend feature 
add_engineered_features <- function(df) {
  df$Sleep_Debt        <- pmax(0, 7 - df$Sleep_Hours_Per_Night)
  df$Usage_Over_4      <- pmax(0, df$Avg_Daily_Usage_Hours - 4)
  df$Ratio             <- df$Avg_Daily_Usage_Hours / df$Sleep_Hours_Per_Night
  df$Ratio_Excess      <- pmax(0, df$Ratio - 0.8)
  df$Mental_Struggle   <- pmax(0, 6 - df$Mental_Health_Score)
  df$Conflict_Intensity <- df$Conflicts_Over_Social_Media
  df
}

train_eng <- add_engineered_features(train)
test_eng  <- add_engineered_features(test)

library(randomForest)

set.seed(2025)
rf_model <- randomForest(
  Social_Media_Burnout ~
    Sleep_Debt + Usage_Over_4 +
    Ratio_Excess + Mental_Struggle +
    Conflict_Intensity +
    Academic_Level + Relationship_Status + Most_Used_Platform,
  data = train_eng,
  ntree = 500,
  mtry  = 4,
  importance = TRUE
)

rf_model

pred_train_rf <- predict(rf_model, type = "class")
mean(pred_train_rf == train_eng$Social_Media_Burnout)

pred_test_rf <- predict(rf_model, newdata = test_eng, type = "class")
mean(pred_test_rf == test_eng$Social_Media_Burnout)

table(Predicted = pred_test_rf,
      Actual    = test_eng$Social_Media_Burnout)

importance(rf_model)
varImpPlot(rf_model)

# Make sure burnout levels are ordered Low < Moderate < High
train_eng$Social_Media_Burnout <- factor(
  train_eng$Social_Media_Burnout,
  levels = c("Low", "Moderate", "High")
)

# Numeric encoding: Low = 1, Moderate = 2, High = 3
train_eng$Burnout_Num <- as.numeric(train_eng$Social_Media_Burnout)

test_eng$Social_Media_Burnout <- factor(
  test_eng$Social_Media_Burnout,
  levels = c("Low", "Moderate", "High")
)
test_eng$Burnout_Num <- as.numeric(test_eng$Social_Media_Burnout)




## ============================
## 2. Sleep_Debt rules
## ============================

# 2.1 Boxplot: Sleep hours vs Burnout
boxplot(Sleep_Hours_Per_Night ~ Social_Media_Burnout,
        data = train_eng,
        main = "Sleep hours vs Social Media Burnout",
        xlab  = "Burnout level",
        ylab  = "Sleep hours per night")

# 2.2 Boxplot: Sleep_Debt vs Burnout (hidden feature)
boxplot(Sleep_Debt ~ Social_Media_Burnout,
        data = train_eng,
        main = "Sleep debt vs Social Media Burnout",
        xlab  = "Burnout level",
        ylab  = "Sleep debt (hours below 7h)")

# 2.3 Mean Sleep_Debt by burnout class
aggregate(Sleep_Debt ~ Social_Media_Burnout, data = train_eng, mean)

# 2.4 Sleep_Debt vs Burnout by Academic_Level (scatter + regression)
par(mfrow = c(1, 3))

for (lev in levels(train_eng$Academic_Level)) {
  sub <- subset(train_eng, Academic_Level == lev)
  
  plot(sub$Sleep_Debt, sub$Burnout_Num,
       pch = 16,
       xlab = "Sleep_Debt (hours below 7h)",
       ylab = "Burnout level (Low=1, Moderate=2, High=3)",
       main = paste("Sleep_Debt vs Burnout -", lev))
  
  abline(lm(Burnout_Num ~ Sleep_Debt, data = sub),
         col = "blue", lwd = 2)
}

par(mfrow = c(1, 1))



## ============================
## 3. Usage_Over_4 rules
## ============================

# 3.1 Raw usage vs Burnout
boxplot(Avg_Daily_Usage_Hours ~ Social_Media_Burnout,
        data = train_eng,
        main = "Daily social media usage vs Burnout",
        xlab  = "Burnout level",
        ylab  = "Avg daily usage (hours)")

# 3.2 Excess usage above 4 hours vs Burnout
boxplot(Usage_Over_4 ~ Social_Media_Burnout,
        data = train_eng,
        main = "Usage above 4 hours vs Burnout",
        xlab  = "Burnout level",
        ylab  = "Hours above 4h/day")

# 3.3 Mean Usage_Over_4 by burnout class
aggregate(Usage_Over_4 ~ Social_Media_Burnout, data = train_eng, mean)

## ============================
## 4. Ratio_Excess rules
## ============================

# 4.1 Scatter: Raw ratio vs Burnout (jittered)
par(mfrow = c(1, 2)) 
plot(train_eng$Ratio,
     jitter(train_eng$Burnout_Num, amount = 0.1),
     col  = as.numeric(train_eng$Social_Media_Burnout),
     pch  = 16,
     xlab = "Usage/Sleep ratio",
     ylab = "Burnout level (jittered)",
     main = "Usage-to-sleep ratio vs Burnout")
legend("topleft",
       legend = levels(train_eng$Social_Media_Burnout),
       col = 1:3, pch = 16)

# 4.2 Boxplot: Ratio_Excess vs Burnout
boxplot(Ratio_Excess ~ Social_Media_Burnout,
        data = train_eng,
        main = "Excess usage/sleep ratio vs Burnout",
        xlab  = "Burnout level",
        ylab  = "Ratio_Excess (above 0.8)")

# 4.3 Mean Ratio_Excess by burnout class
aggregate(Ratio_Excess ~ Social_Media_Burnout, data = train_eng, mean)



## ============================
## 5. Mental_Struggle rules
## ============================

# 5.1 Boxplot: Mental_Health_Score vs Burnout
par(mfrow = c(1, 2)) 
boxplot(Mental_Health_Score ~ Social_Media_Burnout,
        data = train_eng,
        main = "Mental health score vs Burnout",
        xlab  = "Burnout level",
        ylab  = "Mental health score (higher = better)")

# 5.2 Boxplot: Mental_Struggle vs Burnout
boxplot(Mental_Struggle ~ Social_Media_Burnout,
        data = train_eng,
        main = "Mental Struggle vs Burnout",
        xlab  = "Burnout level",
        ylab  = "Mental_Struggle (6 - score, truncated)")

# 5.3 Mean Mental_Struggle by burnout class
aggregate(Mental_Struggle ~ Social_Media_Burnout, data = train_eng, mean)


## ============================
## 6. Conflict + Relationship rules (Undergrad)
## ============================
par(mfrow = c(1, 3)) 

ug <- subset(train_eng, Academic_Level == "Undergraduate")

# 6.1 Boxplot: conflicts vs Burnout (undergrads only)
boxplot(Conflict_Intensity ~ Social_Media_Burnout,
        data = ug,
        main = "Conflicts vs Burnout (Undergraduates)",
        xlab  = "Burnout level",
        ylab  = "Conflict Intensity")

# 6.2 Mean conflict by burnout and relationship status
aggregate(Conflict_Intensity ~ Social_Media_Burnout + Relationship_Status,
          data = ug, mean)

# 6.3 Stacked bar: Burnout distribution by relationship status (Undergrad)
tab_rel_ug  <- table(ug$Relationship_Status, ug$Social_Media_Burnout)
prop_rel_ug <- prop.table(tab_rel_ug, margin = 1)

barplot(t(prop_rel_ug),
        beside = FALSE,
        col = c("skyblue","orange","red"),
        legend.text = TRUE,
        main = "Burnout distribution by relationship status (Undergrad)",
        xlab = "Relationship status",
        ylab = "Proportion")

# 6.4 Interaction visualization: conflicts vs burnout, colored by relationship status
plot(ug$Conflict_Intensity, ug$Burnout_Num,
     col  = as.numeric(ug$Relationship_Status),
     pch  = 16,
     xlab = "Conflict_Intensity",
     ylab = "Burnout level (1=Low,3=High)",
     main = "Conflict vs Burnout by Relationship (Undergrad)")
legend("topleft",
       legend = levels(ug$Relationship_Status),
       col = 1:length(levels(ug$Relationship_Status)),
       pch = 16)



## ============================
## 7. Platform rules
## ============================
par(mfrow = c(1, 2)) 

tab_plat  <- table(train_eng$Most_Used_Platform, train_eng$Social_Media_Burnout)
prop_plat <- prop.table(tab_plat, margin = 1)

barplot(t(prop_plat),
        beside = FALSE,
        col = c("skyblue","orange","red"),
        legend.text = TRUE,
        main = "Burnout distribution by most used platform",
        xlab = "Most used platform",
        ylab = "Proportion")

# Optional: focus only on Instagram/TikTok/LinkedIn
sub_plat <- subset(train_eng,
                   Most_Used_Platform %in% c("Instagram", "TikTok", "LinkedIn"))

tab_sub  <- table(sub_plat$Most_Used_Platform, sub_plat$Social_Media_Burnout)
prop_sub <- prop.table(tab_sub, margin = 1)

barplot(t(prop_sub),
        beside = FALSE,
        col = c("skyblue","orange","red"),
        legend.text = TRUE,
        main = "Burnout by Platform (Instagram/TikTok vs LinkedIn)",
        xlab = "Platform",
        ylab = "Proportion")

## ============================
## 8. Academic level–specific patterns
## ============================

# 8.1 Burnout composition by Academic_Level
tab_acad  <- table(train_eng$Academic_Level, train_eng$Social_Media_Burnout)
prop_acad <- prop.table(tab_acad, margin = 1)

barplot(t(prop_acad),
        beside = FALSE,
        col = c("skyblue","orange","red"),
        legend.text = TRUE,
        main = "Burnout distribution by Academic Level",
        xlab = "Academic level",
        ylab = "Proportion")


