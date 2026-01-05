install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

#Hypothesis testing 
df <- read.csv("/Users/lainam/Desktop/Students_Social_Media_Addiction.csv")

#' Null Hypothesis : The average value of Addicted_Score is the same when Academic_Level =
#' 'Undergraduate' and when Academic_Level = 'Graduate'
#'
#' Alternative Hypothesis : The average value of Addicted_Score is greater when 
#' Academic_Level = 'Undergraduate' than when Academic_Level = 'Graduate'
m_U <- mean(df$Addicted_Score[df$Academic_Level=="Undergraduate"], na.rm=TRUE)
m_G <- mean(df$Addicted_Score[df$Academic_Level=="Graduate"],     na.rm=TRUE)
obs_diff <- m_U - m_G
obs_diff

# 3) Z-test (one-tailed): test U - G > 0 ⇒ sub1="Graduate", sub2="Undergraduate"
p_z <- z_test_from_data(df, "Academic_Level", "Addicted_Score",
                        "Graduate", "Undergraduate")

# 4) Permutation test (one-tailed, N>=10000) 

p_perm <- permutation_test(df, "Academic_Level", "Addicted_Score",
                           10000, "Graduate", "Undergraduate")

cat(p_perm, "\n")






#Confidence Interval for Avg_Daily_Usage_Hours of student most used Instagram 
z_score <- 1.96

# 2) Subset: Instagram users
ins <- subset(df, Most_Used_Platform == "Instagram" & !is.na(Avg_Daily_Usage_Hours))

# 3) Sample size, mean, and standard deviation
n  <- nrow(ins)
m  <- mean(ins$Avg_Daily_Usage_Hours)
s  <- sd(ins$Avg_Daily_Usage_Hours)

# 4) Standard error of the mean (SEM)

sem <- s / (n^(1/2))
# 5) Margin of Error and CI
MOE <- z_score * sem
LCL <- m - MOE
UCL <- m + MOE

round(c(n = n, mean = m, sd = s, SEM = sem, MOE = MOE, LCL = LCL, UCL = UCL), 3)



#Chi-Square 
# Academic_Level vs Affects_Academic_Performance
# Academic_Level: Undergraduate, Graduate, High School
#Affects_Academic_Performance: Boolean for their affection of academic performance ( Yes/ No)

cont_ac <- table(df$Academic_Level, df$Affects_Academic_Performance)
cont_ac

chisq_ac <- chisq.test(cont_ac)
chisq_ac

# Interpretation:
# H0: Academic_Level and Affects_Academic_Performance are independent
# H1: They are associated
# Decision: if p-value < 0.05 → reject H0 (association exists)

chisq_ac$expected
chisq_ac$observed





# === Bayesian: D = (Affects = "Yes"), O = (Platform = TikTok) ===
# Belief (D): Affects_Academic_Performance == "Yes"
# Observation (O): Most_Used_Platform == "TikTok"

# Step 1: Prior Probability and Odds
n_affects <- sum(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
n_total   <- sum(!is.na(df$Affects_Academic_Performance))
PriorP <- n_affects / n_total
#PriorP <- mean(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
PriorOdds <- PriorP / (1 - PriorP)
cat("Prior Probability:", round(PriorP, 3), "\n")
cat("Prior Odds:", round(PriorOdds, 3), "\n")

# Step 2: Observation (Most_Used_Platform == "TikTok")
obs <- df$Most_Used_Platform == "TikTok"

# Step 3: True Positive and False Positive
TP <- sum(df$Most_Used_Platform == "TikTok" & df$Affects_Academic_Performance == "Yes", na.rm = TRUE) / 
  sum(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
cat("True Positive:", round(TP, 3), "\n")

FP <- sum(df$Most_Used_Platform == "TikTok" & df$Affects_Academic_Performance == "No", na.rm = TRUE) / 
  sum(df$Affects_Academic_Performance == "No", na.rm = TRUE)
cat("False Positive:", round(FP, 3), "\n")

cat("TP:", round(TP, 3), " FP:", round(FP, 3), "\n")

# Step 4: Likelihood Ratio and Posterior
LR <- TP / FP
PostOdds <- LR * PriorOdds
cat(PostOdds, "\n")
PostP <- PostOdds / (1 + PostOdds)
cat("Likelihood Ratio:", round(LR, 2), "\n")
cat("Posterior Probability (Affects | TikTok):", round(PostP * 100, 2), "%\n")

