install.packages("arules")
install.packages("devtools")
install.packages("rpart.plot")

devtools::install_github("janish-parikh/ZTest")


library(arules)
library(devtools)
library(HypothesisTesting)
library(rpart)
library(rpart.plot)


## ================================
## 1.1 Load data & basic cleaning
## ================================

setwd("/Users/lainam/Desktop")

social <- read.csv("Students_Social_Media_Addiction.csv",
                   stringsAsFactors = FALSE)

nrow(social)

head(social)
str(social)
names(social)


## ================================
## 1.2 Định nghĩa kiểu biến
## ================================

# turn into factor
social$Gender                        <- factor(social$Gender)
social$Academic_Level                <- factor(social$Academic_Level)
social$Country                       <- factor(social$Country)
social$Most_Used_Platform            <- factor(social$Most_Used_Platform)
social$Affects_Academic_Performance  <- factor(social$Affects_Academic_Performance)
social$Relationship_Status           <- factor(social$Relationship_Status)

numeric_vars <- names(social)[sapply(social, is.numeric)]
categorical_vars <- names(social)[sapply(social, is.factor)]

numeric_vars
categorical_vars


## ================================
## 1.3 use function table(), summary(), subset(), tapply()
## ================================

# 1) summary() for numerical
summary(social$Age)
summary(social$Avg_Daily_Usage_Hours)
summary(social$Sleep_Hours_Per_Night)
summary(social$Addicted_Score)

# 2) table() for categorical
table(social$Gender)
table(social$Academic_Level)
table(social$Most_Used_Platform)
table(social$Affects_Academic_Performance)

# 3) subset():  Undergraduate
undergrads <- subset(social, Academic_Level == "Undergraduate")
head(undergrads)

# subset(): high score for  Addicted_Score ≥ 8 
high_addiction <- subset(social, Addicted_Score >= 8)
head(high_addiction)

# 4) tapply():average due to Academic_Level
tapply(social$Addicted_Score, social$Academic_Level, mean)

# tapply(): hour of using social media on Platform
tapply(social$Avg_Daily_Usage_Hours, social$Most_Used_Platform, mean)


## ================================
## 1.4 Create new variable (data transformation)
## ================================

## a) Addiction_Level
# - Low  : Addicted_Score <= 4
# - Moderate: 5–7
# - High : >= 8

social$Addiction_Level <- cut(
  social$Addicted_Score,
  breaks = c(-Inf, 4, 7, Inf),
  labels = c("Low", "Moderate", "High"),
  right = TRUE
)

table(social$Addiction_Level, useNA = "ifany")

## b) High_Addiction

social$High_Addiction <- ifelse(social$Addicted_Score >= 8, 1, 0)

## c) Sleep_Deprived: lack of sleep (< 7h)
social$Sleep_Deprived <- ifelse(social$Sleep_Hours_Per_Night < 7, 1, 0)

table(social$Sleep_Deprived)

## d) Heavy_User:using social media much (>= 5 hr/day)
social$Heavy_User <- ifelse(social$Avg_Daily_Usage_Hours >= 5, 1, 0)

table(social$Heavy_User)

## e) Usage_to_Sleep_Ratio: Avg_Daily_Usage_Hours/Sleep_Hours_Per_Night
social$Usage_to_Sleep_Ratio <- social$Avg_Daily_Usage_Hours / social$Sleep_Hours_Per_Night
summary(social$Usage_to_Sleep_Ratio)

## f) Affects_Binary: Yes/No → 1/0 
social$Affects_Binary <- ifelse(social$Affects_Academic_Performance == "Yes", 1, 0)

table(social$Affects_Academic_Performance, social$Affects_Binary)


## g) High_Risk_Study_Impact:
##    if High_Addiction = 1 AND Affects_Academic_Performance == "Yes"
social$High_Risk_Study_Impact <- ifelse(
  social$High_Addiction == 1 & social$Affects_Academic_Performance == "Yes",
  1, 0
)

table(social$High_Risk_Study_Impact)


## ================================
## 1.5 Summarize dataset 
## ================================

nrow(social)        
length(numeric_vars)
length(categorical_vars)


## ================================
## 2. Exploratory Data Analysis
## ================================

social$Addiction_Level <- cut(
  social$Addicted_Score,
  breaks = c(-Inf, 4, 7, Inf),
  labels = c("Low", "Moderate", "High"),
  right = TRUE
)

## 2.1 Histogram of Addicted_Score
hist(
  social$Addicted_Score,
  breaks = 8,
  main = "Distribution of Social Media Addiction Scores",
  xlab = "Addicted Score",
  ylab = "Number of Students",
  col  = "lightblue",
  border = "white"
)

## 2.2 Boxplot: Addicted_Score by Academic_Level
boxplot(
  Addicted_Score ~ Academic_Level,
  data = social,
  main = "Addiction Scores by Academic Level",
  xlab = "Academic Level",
  ylab = "Addicted Score",
  col  = "lightgreen"
)

## 2.3 Barplot: counts of Most_Used_Platform
platform_counts <- table(social$Most_Used_Platform)

barplot(
  platform_counts,
  main = "Most Used Social Media Platforms",
  xlab = "Platform",
  ylab = "Number of Students",
  las  = 2,           
  col  = "lightcoral"
)

## 2.4 Scatterplot: Avg_Daily_Usage_Hours vs Sleep_Hours_Per_Night
plot(
  social$Avg_Daily_Usage_Hours,
  social$Sleep_Hours_Per_Night,
  main = "Daily Social Media Use vs Sleep Duration",
  xlab = "Avg Daily Usage (hours)",
  ylab = "Sleep Hours per Night",
  pch  = 19,
  col  = "darkgray"
)

# add linear line 
abline(
  lm(Sleep_Hours_Per_Night ~ Avg_Daily_Usage_Hours, data = social),
  col = "red",
  lwd = 2
)

## ================================
## 3. Fooled by Data: Randomness
## ================================

set.seed(123)

## 3.1 Real pattern: Addicted_Score by Gender

# Số lượng theo giới
table(social$Gender)

# Mean Addicted_Score on Gender
real_means_gender <- tapply(
  social$Addicted_Score,
  social$Gender,
  mean,
  na.rm = TRUE
)
real_means_gender

# Boxplot: Addiction on Gender (pattern "real" )
boxplot(
  Addicted_Score ~ Gender,
  data = social,
  main = "Addiction Scores by Gender",
  xlab = "Gender",
  ylab = "Addicted Score",
  col  = c("lightpink", "lightblue")
)

## 3.2 Observed difference (Female - Male)

m_F <- mean(social$Addicted_Score[social$Gender == "Female"], na.rm = TRUE)
m_M <- mean(social$Addicted_Score[social$Gender == "Male"],   na.rm = TRUE)

obs_diff <- m_F - m_M   
m_F
m_M
obs_diff   

## 3.3 Permutation: shuffle  Gender difference of random

n_sim <- 5000
diff_sim <- numeric(n_sim)

for (i in 1:n_sim) {
  #permutation with 
  gender_perm <- sample(social$Gender)
  
  # Chênh lệch mean(Female) - mean(Male) dưới nhãn random
  diff_sim[i] <- mean(
    social$Addicted_Score[gender_perm == "Female"],
    na.rm = TRUE
  ) - mean(
    social$Addicted_Score[gender_perm == "Male"],
    na.rm = TRUE
  )
}

# Histogram: 
hist(
  diff_sim,
  breaks = 40,
  main = "Permutation Distribution of Gender Difference in Addiction",
  xlab = "mean(Female) - mean(Male)",
  col  = "lightyellow",
  border = "white"
)


# Permutation p-value (two-sided)
p_perm_gender <- mean(abs(diff_sim) >= abs(obs_diff))
p_perm_gender



## 4. CLT & 95% CI – Instagram Usage
## ================================

z_score <- 1.96

# Subset: Instagram users
ins <- subset(social,
              Most_Used_Platform == "Instagram" &
                !is.na(Avg_Daily_Usage_Hours))

# Sample size, mean, and standard deviation
n_inst <- nrow(ins)
m_inst <- mean(ins$Avg_Daily_Usage_Hours)
s_inst <- sd(ins$Avg_Daily_Usage_Hours)

# Standard error of the mean (SEM)
sem_inst <- s_inst / sqrt(n_inst)

# Margin of Error and CI
MOE_inst <- z_score * sem_inst
LCL_inst <- m_inst - MOE_inst
UCL_inst <- m_inst + MOE_inst

round(c(n = n_inst,
        mean = m_inst,
        sd   = s_inst,
        SEM  = sem_inst,
        MOE  = MOE_inst,
        LCL  = LCL_inst,
        UCL  = UCL_inst), 3)


## ================================
## 5. Hypothesis Testing – Undergrad vs Graduate
## ================================


# Means for Undergraduate vs Graduate
m_U <- mean(social$Addicted_Score[social$Academic_Level == "Undergraduate"],
            na.rm = TRUE)
m_G <- mean(social$Addicted_Score[social$Academic_Level == "Graduate"],
            na.rm = TRUE)
obs_diff_UG <- m_U - m_G
obs_diff_UG

# 1) Z-test (one-tailed): test U - G > 0
p_z <- z_test_from_data(social,
                        "Academic_Level",
                        "Addicted_Score",
                        "Graduate",      # sub1
                        "Undergraduate") # sub2
p_z

# 2) Permutation test (one-tailed, N>=10000) 
p_perm_UG <- permutation_test(social,
                              "Academic_Level",
                              "Addicted_Score",
                              10000,
                              "Graduate",
                              "Undergraduate")
p_perm_UG

cat("Permutation p(one-tailed) UG:", p_perm_UG, "\n")


## ================================
## 6. Chi-Square – Academic_Level x Affects_Academic_Performance
## ================================

cont_ac <- table(social$Academic_Level,
                 social$Affects_Academic_Performance)
cont_ac

chisq_ac <- chisq.test(cont_ac)
chisq_ac

# Expected vs Observed
chisq_ac$expected
chisq_ac$observed


## ================================
## 7. Multiple Hypothesis Testing:
##    Bonferroni across platforms
## ================================

# 7.1 Mean Addicted_Score theo platform
means_platform <- tapply(
  social$Addicted_Score,
  social$Most_Used_Platform,
  mean,
  na.rm = TRUE
)
means_platform

#  platform & number of pairs
n_platform <- sum(!is.na(means_platform))          
m_platform <- n_platform * (n_platform - 1) / 2    

alpha_bonf_platform <- 0.05 / m_platform           
cat("Number of platforms =", n_platform, "\n",
    "Number of comparisons (all pairs) =", m_platform, "\n",
    "Bonferroni alpha (FWER 0.05) =", alpha_bonf_platform, "\n")

# 7.2 choose 10 pairs  TikTok vs famous platform 
# (tên phải đúng với table(social$Most_Used_Platform))
platform_pairs <- rbind(
  c("Instagram", "TikTok"),
  c("Facebook", "TikTok"),
  c("YouTube", "TikTok"),
  c("WhatsApp", "TikTok"),
  c("Twitter", "TikTok"),
  c("Snapchat", "TikTok"),
  c("WeChat", "TikTok"),
  c("LINE", "TikTok"),
  c("LinkedIn", "TikTok"),
  c("VKontakte", "TikTok")
)

platform_pairs

# 7.3  permutation_test for each
multi_tests <- data.frame(
  platform1 = platform_pairs[, 1],
  platform2 = platform_pairs[, 2],
  p_perm    = NA_real_
)

for (i in 1:nrow(multi_tests)) {
  multi_tests$p_perm[i] <- permutation_test(
    social,
    "Most_Used_Platform",
    "Addicted_Score",
    10000,
    multi_tests$platform1[i],
    multi_tests$platform2[i]
  )
}

multi_tests

# 7.4 Mark significant before & after Bonferroni
multi_tests$significant_raw  <- multi_tests$p_perm < 0.05
multi_tests$significant_bonf <- multi_tests$p_perm < alpha_bonf_platform

multi_tests



## ================================
## 8. Bayesian Reasoning – TikTok & Academic Impact
## ================================

# Belief (D): Affects_Academic_Performance == "Yes"
# Observation (O): Most_Used_Platform == "TikTok"

# Step 1: Prior Probability and Odds
n_affects <- sum(social$Affects_Academic_Performance == "Yes", na.rm = TRUE)
n_total   <- sum(!is.na(social$Affects_Academic_Performance))

PriorP    <- n_affects / n_total
PriorOdds <- PriorP / (1 - PriorP)
cat("Prior Probability:", round(PriorP, 3), "\n")
cat("Prior Odds:", round(PriorOdds, 3), "\n")

# Step 2: Observation (Most_Used_Platform == "TikTok")
obs_tiktok <- social$Most_Used_Platform == "TikTok"

# Step 3: True Positive and False Positive
TP <- sum(social$Most_Used_Platform == "TikTok" &
            social$Affects_Academic_Performance == "Yes",
          na.rm = TRUE) /
  sum(social$Affects_Academic_Performance == "Yes", na.rm = TRUE)
cat("True Positive:", round(TP, 3), "\n")

FP <- sum(social$Most_Used_Platform == "TikTok" &
            social$Affects_Academic_Performance == "No",
          na.rm = TRUE) /
  sum(social$Affects_Academic_Performance == "No", na.rm = TRUE)
cat("False Positive:", round(FP, 3), "\n")

cat("TP:", round(TP, 3), " FP:", round(FP, 3), "\n")

# Step 4: Likelihood Ratio and Posterior
LR       <- TP / FP
PostOdds <- LR * PriorOdds
PostP    <- PostOdds / (1 + PostOdds)

cat("Likelihood Ratio:", round(LR, 2), "\n")
cat("Posterior Probability (Affects | TikTok):", round(PostP * 100, 2), "%\n")

## ================================
## 9. Prediction Model – Decision Tree
## ================================


## 9.1 Prepair data for model

# Biến mục tiêu: High_Addiction (0/1) -> factor Yes/No
social$High_Addiction_F <- factor(
  social$High_Addiction,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# choose feature for predict 
model_vars <- c(
  "High_Addiction_F",          # target
  "Gender",
  "Academic_Level",
  "Most_Used_Platform",
  "Avg_Daily_Usage_Hours",
  "Sleep_Hours_Per_Night",
  "Affects_Academic_Performance",
  "Usage_to_Sleep_Ratio",
  "Sleep_Deprived",
  "Heavy_User"
)

model_df <- social[, model_vars]

model_df <- model_df[complete.cases(model_df), ]

nrow(model_df)

## 9.2 Split train / test (70% / 30%)

set.seed(123)  
n <- nrow(model_df)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

train_data <- model_df[train_idx, ]
test_data  <- model_df[-train_idx, ]

## 9.3  (classification tree)

tree_model <- rpart(
  High_Addiction_F ~ Gender +
    Academic_Level +
    Most_Used_Platform +
    Avg_Daily_Usage_Hours +
    Sleep_Hours_Per_Night +
    Affects_Academic_Performance +
    Usage_to_Sleep_Ratio +
    Sleep_Deprived +
    Heavy_User,
  data   = train_data,
  method = "class",
  control = rpart.control(
    minsplit = 20,
    cp       = 0.01    
  )
)

## 9.4 Accuracy on train & test

# predict on train
train_pred <- predict(tree_model, type = "class")
train_acc  <- mean(train_pred == train_data$High_Addiction_F)

# predict on test
test_pred <- predict(tree_model, newdata = test_data, type = "class")
test_acc  <- mean(test_pred == test_data$High_Addiction_F)

train_acc
test_acc

## 9.5 (plot(tree))

rpart.plot(tree_model)
text(tree_model, use.n = TRUE, all = TRUE, cex = 0.7)

## ================================
## 10. Association Rules & Lift
## ================================



## 10.1 prepair data for apriori

assoc_df <- data.frame(
  High_Addiction = social$High_Addiction == 1,
  Heavy_User     = social$Heavy_User == 1,
  Sleep_Deprived = social$Sleep_Deprived == 1,
  TikTok_User    = social$Most_Used_Platform == "TikTok",
  Instagram_User = social$Most_Used_Platform == "Instagram",
  WhatsApp_User  = social$Most_Used_Platform == "WhatsApp",
  AffectsYes     = social$Affects_Academic_Performance == "Yes"
)

# NA -> FALSE 
assoc_df[is.na(assoc_df)] <- FALSE

# Turn into transactions for arules
trans <- as(assoc_df, "transactions")
summary(trans)   

## 10.2 Run apriori to find rules with lift > 1

rules_all <- apriori(
  trans,
  parameter = list(
    supp   = 0.05,  
    conf   = 0.60,  # confidence ≥ 60%
    minlen = 2,
    maxlen = 3
  )
)

# Filter rules with lift > 1
rules_lift <- subset(rules_all, lift > 1)

# Show strong rule with lift
inspect(head(sort(rules_lift, by = "lift"), 15))


## 10.3 Calculate support / confidence / lift for particular rule (base R)

n <- nrow(social)

rule_metrics <- function(lhs_condition, rhs_condition) {
  both <- lhs_condition & rhs_condition
  support    <- sum(both) / n
  confidence <- sum(both) / sum(lhs_condition)
  base_rate  <- sum(rhs_condition) / n
  lift       <- confidence / base_rate
  c(Support = support, Confidence = confidence, Lift = lift)
}

# Rule 1: Heavy_User & TikTok_User -> High_Addiction
r1 <- rule_metrics(
  social$Heavy_User == 1 & social$Most_Used_Platform == "TikTok",
  social$High_Addiction == 1
)

# Rule 2: Heavy_User & Sleep_Deprived -> High_Addiction
r2 <- rule_metrics(
  social$Heavy_User == 1 & social$Sleep_Deprived == 1,
  social$High_Addiction == 1
)

# Rule 3: TikTok_User -> High_Addiction
r3 <- rule_metrics(
  social$Most_Used_Platform == "TikTok",
  social$High_Addiction == 1
)

# Rule 4: High_Addiction -> AffectsYes
r4 <- rule_metrics(
  social$High_Addiction == 1,
  social$Affects_Academic_Performance == "Yes"
)

r1; r2; r3; r4

