install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)

df <- read.csv("/Users/lainam/Desktop/Students_Social_Media_Addiction.csv")

m_U <- mean(df$Addicted_Score[df$Academic_Level=="Undergraduate"], na.rm=TRUE)
m_G <- mean(df$Addicted_Score[df$Academic_Level=="Graduate"],     na.rm=TRUE)
obs_diff <- m_U - m_G
obs_diff

p_z <- z_test_from_data(df, "Academic_Level", "Addicted_Score",
                        "Graduate", "Undergraduate")


p_perm <- permutation_test(df, "Academic_Level", "Addicted_Score",
                           10000, "Graduate", "Undergraduate")

cat(p_perm)






z_score <- 1.96

ins <- subset(df, Most_Used_Platform == "Instagram" & !is.na(Avg_Daily_Usage_Hours))

n  <- nrow(ins)
m  <- mean(ins$Avg_Daily_Usage_Hours)
s  <- sd(ins$Avg_Daily_Usage_Hours)


sem <- s / (n^(1/2))
MOE <- z_score * sem
LCL <- m - MOE
UCL <- m + MOE

round(c(n = n, mean = m, sd = s, SEM = sem, MOE = MOE, LCL = LCL, UCL = UCL), 3)





cont_ac <- table(df$Academic_Level, df$Affects_Academic_Performance)
cont_ac

chisq_ac <- chisq.test(cont_ac)
chisq_ac



chisq_ac$expected
chisq_ac$observed






n_affects <- sum(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
n_total   <- sum(!is.na(df$Affects_Academic_Performance))
PriorP <- n_affects / n_total

PriorOdds <- PriorP / (1 - PriorP)
cat("Prior Probability:", round(PriorP, 3), "\n")
cat("Prior Odds:", round(PriorOdds, 3), "\n")

obs <- df$Most_Used_Platform == "TikTok"

TP <- sum(df$Most_Used_Platform == "TikTok" & df$Affects_Academic_Performance == "Yes", na.rm = TRUE) / 
  sum(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
cat("True Positive:", round(TP, 3), "\n")

FP <- sum(df$Most_Used_Platform == "TikTok" & df$Affects_Academic_Performance == "No", na.rm = TRUE) / 
  sum(df$Affects_Academic_Performance == "No", na.rm = TRUE)
cat("False Positive:", round(FP, 3), "\n")

cat("TP:", round(TP, 3), " FP:", round(FP, 3), "\n")

LR <- TP / FP
PostOdds <- LR * PriorOdds
cat(PostOdds)
PostP <- PostOdds / (1 + PostOdds)
cat("Likelihood Ratio:", round(LR, 2), "\n")
cat("Posterior Probability (Affects | TikTok):", round(PostP * 100, 2), "%\n")








z_score <- 1.96

ins <- subset(df, Most_Used_Platform == "Instagram" & !is.na(Avg_Daily_Usage_Hours))

n  <- nrow(ins)
m  <- mean(ins$Avg_Daily_Usage_Hours)
s  <- sd(ins$Avg_Daily_Usage_Hours)


sem <- s / (n^(1/2))
MOE <- z_score * sem
LCL <- m - MOE
UCL <- m + MOE

round(c(n = n, mean = m, sd = s, SEM = sem, MOE = MOE, LCL = LCL, UCL = UCL), 3)





cont_ac <- table(df$Academic_Level, df$Affects_Academic_Performance)
cont_ac

chisq_ac <- chisq.test(cont_ac)
chisq_ac



chisq_ac$expected
chisq_ac$observed







n_affects <- sum(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
n_total   <- sum(!is.na(df$Affects_Academic_Performance))
PriorP <- n_affects / n_total
PriorOdds <- PriorP / (1 - PriorP)
cat("Prior Probability:", round(PriorP, 3), "\n")
cat("Prior Odds:", round(PriorOdds, 3), "\n")

obs <- df$Most_Used_Platform == "TikTok"

TP <- sum(df$Most_Used_Platform == "TikTok" & df$Affects_Academic_Performance == "Yes", na.rm = TRUE) / 
  sum(df$Affects_Academic_Performance == "Yes", na.rm = TRUE)
cat("True Positive:", round(TP, 3), "\n")

FP <- sum(df$Most_Used_Platform == "TikTok" & df$Affects_Academic_Performance == "No", na.rm = TRUE) / 
  sum(df$Affects_Academic_Performance == "No", na.rm = TRUE)
cat("False Positive:", round(FP, 3), "\n")

cat("TP:", round(TP, 3), " FP:", round(FP, 3), "\n")

LR <- TP / FP

PostOdds <- LR * PriorOdds
cat(PostOdds, "\n")
PostP <- PostOdds / (1 + PostOdds)
cat("Likelihood Ratio:", round(LR, 2), "\n")
cat("Posterior Probability (Affects | TikTok):", round(PostP * 100, 2), "%\n")

