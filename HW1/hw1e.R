# 0) Load data
dataset <- read.csv("Desktop/Students_Social_Media_Addiction.csv")

#1) Prepare helper buckets
# (for clean group comparisons / plots)
dataset$Addiction_Level <- cut(dataset$Addicted_Score,
                               breaks = c(0, 3, 6, 10),
                               labels = c("Low", "Medium", "High"))

dataset$Usage_Level <- cut(dataset$Avg_Daily_Usage_Hours,
                           breaks = c(1.5, 3, 5, 7, 8.5),
                           labels = c("Light", "Moderate", "Heavy", "Extreme"))

dataset$Mental_Health_Cat <- cut(dataset$Mental_Health_Score,
                                 breaks = c(4, 5, 7, 9),
                                 labels = c("Poor", "Fair", "Good"))

#2) Basic explore:
nrow(dataset)        
ncol(dataset)       
names(dataset)    
str(dataset) 

summary(dataset$Avg_Daily_Usage_Hours)
summary(dataset$Sleep_Hours_Per_Night)
summary(dataset$Mental_Health_Score)
summary(dataset$Addicted_Score)


table(dataset$Gender)
table(dataset$Academic_Level)
table(dataset$Most_Used_Platform)
table(dataset$Addiction_Level)
table(dataset$Usage_Level)
table(dataset$Mental_Health_Cat)

# 3) Relation of atrribute 




#Number 1
plot(dataset$Avg_Daily_Usage_Hours, dataset$Sleep_Hours_Per_Night,
     main = "Usage Hours vs Sleep Hours",
     xlab = "Avg Daily Usage Hours", ylab = "Sleep Hours per Night",
     pch = 19, col = "blue")
abline(lm(Sleep_Hours_Per_Night ~ Avg_Daily_Usage_Hours, data = dataset),
       col = "red", lwd = 2)
# Students use SM 4–5h/day but sleep => 8h
outlier_mid_usage_high_sleep <- subset(dataset, 
                                       Avg_Daily_Usage_Hours >= 4.5 & Avg_Daily_Usage_Hours <= 5.5 &
                                         Sleep_Hours_Per_Night >= 8)

outlier_mid_usage_high_sleep[, c("Gender","Academic_Level","Most_Used_Platform",
                                 "Addicted_Score","Conflicts_Over_Social_Media",
                                 "Mental_Health_Score","Relationship_Status")]

# Students use SM 4–6h/day but sleep <= 5h
outlier_low_sleep <- subset(dataset, 
                            Avg_Daily_Usage_Hours >= 4.5 & Avg_Daily_Usage_Hours <= 5.5 &
                              Sleep_Hours_Per_Night <= 5)
outlier_low_sleep[, c("Gender", "Academic_Level", "Most_Used_Platform", 
                      "Addicted_Score", "Conflicts_Over_Social_Media", 
                      "Mental_Health_Score", "Relationship_Status")]




#Number 2 in
#Mean of usage on addiction level
tapply(dataset$Avg_Daily_Usage_Hours, dataset$Addiction_Level, mean)
#Mean of conflict on addiction level
tapply(dataset$Conflicts_Over_Social_Media, dataset$Addiction_Level, mean)

# scatter plot Conflict vs Avg Usage Hours -> Conflict high but usage hour ~ >3
plot(dataset$Conflicts_Over_Social_Media, dataset$Avg_Daily_Usage_Hours,
     main = "Conflict vs Usage Hours",
     xlab = "Conflicts", ylab = "Usage Hours",
     pch = 19, col = "darkgreen")
# scatter plot Avg usage hour vs Addicted Score
plot(dataset$Avg_Daily_Usage_Hours, dataset$Addicted_Score,
     main = "Usage Hours vs Addiction",
     xlab = "Usage Hours", ylab = "Addicted Score",
     pch = 19, col = "purple")

# box plot for Addiction Level vs Avg usage hour -> There are lot of student with usage time low ( 3-4 hrs) but high addiction level 
boxplot(Avg_Daily_Usage_Hours ~ Addiction_Level, 
        data = dataset,
        main = "Addiction Level vs Avg usage hour",
        xlab = "Addiction Level", ylab = "Avg Daily Usage Hours",
        col = c("lightgreen", "yellow", "red"))

# (E) Scatter: Addiction high but usage hour low, color on the conflict >3
plot(dataset$Avg_Daily_Usage_Hours, dataset$Addicted_Score,
     pch = 19,
     col = ifelse(dataset$Conflicts_Over_Social_Media >= 3, "red", "grey"),
     main = "The impact of conflict hour",
     xlab = "Avg Daily Usage Hours",
     ylab = "Addicted Score")
legend("topleft", legend = c("Conflicts >= 3", "Conflicts <3"), 
       col = c("red", "grey"), pch = 19)

#high addiction level have high conflict level 
boxplot(Conflicts_Over_Social_Media ~ Addiction_Level, 
        data = dataset,
        col = c("lightgreen", "yellow", "red"),
        main = "Conflicts Over Social Media vs Addiction Level",
        xlab = "Addiction Level",
        ylab = "Conflicts")

#Most kind of platform lead to conflict 
conflict_high <- subset(dataset, Conflicts_Over_Social_Media >= 3)
table(conflict_high$Most_Used_Platform)

barplot(table(conflict_high$Most_Used_Platform),
        main = "Most Used Platform (Conflict >=3)",
        xlab = "Platform", ylab = "Quantity",
        col = "red")

#Number 3 in
#Most_Used_Platform vs Addiction
tapply(dataset$Addicted_Score, dataset$Most_Used_Platform, mean)
boxplot(Addicted_Score ~ Most_Used_Platform, data = dataset,
        main = "Addiction Score due to Platform",
        xlab = "Platform", ylab = "Addiction Score",
        col = rainbow(length(unique(dataset$Most_Used_Platform))),
        las = 2) 

#social media most affect mental health 
tapply(dataset$Mental_Health_Score, dataset$Most_Used_Platform, mean)


# More Addiction, less mental health 
boxplot(Mental_Health_Score ~ Addiction_Level, data = dataset,
        main = "Mental Health vs Addiction Level",
        xlab = "Addiction Level", ylab = "Mental Health Score",
        col = c("lightgreen","yellow","red"))

plot(dataset$Addicted_Score, dataset$Mental_Health_Score,
     main = "Mental Health vs Addiction Level",
     xlab = "Addiction Level", ylab = "Mental Health Score",
     pch = 19, col = "blue")
abline(lm(dataset$Mental_Health_Score ~ dataset$Addicted_Score), col = "red", lwd = 2)

#highschool addiction
#Addicted score based on Academic Level 
boxplot(Addicted_Score ~ Academic_Level, data = dataset,
        main = "Addiction Score based on Academic Level",
        xlab = "Academic Level", ylab = "Addiction Score",
        col = c("lightblue","lightgreen","orange"))
tapply(dataset$Addicted_Score, dataset$Academic_Level, mean)
#highschool
hs <- subset(dataset, Academic_Level == "High School")

# Boxplot
boxplot(Addicted_Score ~ Most_Used_Platform, data = hs,
        main = "Addiction Score based on Platform (High. School)",
        xlab = "Platform", ylab = "Addiction Score",
        col = rainbow(length(unique(hs$Most_Used_Platform))),
        las = 2)

boxplot(Avg_Daily_Usage_Hours ~ Academic_Level, data = dataset,
        main = "Usage Hours vs Academic Level",
        xlab = "Academic Level", ylab = "Avg Daily Usage Hours",
        col = c("lightblue","lightgreen","orange"))
#-> highschool have more usage time than the others

boxplot(Conflicts_Over_Social_Media ~ Academic_Level, data = dataset,
        main = "Conflict vs Academic Level",
        xlab = "Academic Level", ylab = "Conflicts_Over_Social_Media",
        col = c("lightblue","lightgreen","orange"))
#-> highschool have more conflict than the others

# Cross-tab between Addiction Level and Academic Performance
table_addiction_perf <- table(dataset$Addiction_Level, dataset$Affects_Academic_Performance)
table_addiction_perf
prop.table(table_addiction_perf, margin = 1)






#Number 4
# Addicticted score based on Relationship 
boxplot(Addicted_Score ~ Relationship_Status, 
        data = dataset,
        main = "Addiction Score theo Relationship Status",
        xlab = "Relationship Status",
        ylab = "Addicted Score",
        col = c("skyblue", "pink", "orange"))
tapply(dataset$Addicted_Score, dataset$Relationship_Status, mean)

table_gender_perf <- table(dataset$Gender, dataset$Affects_Academic_Performance)
table_gender_perf

barplot(prop.table(table_gender_perf, margin = 1)[, "Yes"],
        beside = TRUE,
        col = c("pink", "skyblue"),
        main = "Proportion of genders affected by Social Media",
        ylab = "Proportion",
        ylim = c(0,1))
#Mental of female vs male in using social media 
tapply(rel$Mental_Health_Score, rel$Gender, mean)

barplot(tapply(rel$Conflicts_Over_Social_Media, rel$Gender, mean),
        main = "Average Conflicts by Gender (In Relationship)",
        xlab = "Gender", ylab = "Mean Conflicts",
        col = c("pink","skyblue"))

#usage time of female and male in relationship
boxplot(Avg_Daily_Usage_Hours ~ Gender, data = rel,
        main = "Usage Hours (In Relationship)",
        xlab = "Gender", ylab = "Avg Daily Usage Hours",
        col = c("pink","skyblue"))