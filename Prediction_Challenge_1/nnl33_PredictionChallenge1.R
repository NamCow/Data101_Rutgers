install.packages("rpart")
install.packages("rpart.plot")
install.packages("ModelMetrics")
install.packages("devtools")
install.packages("bestNormalize")
install.packages("randomForest")   
library(randomForest)
#devtools::install_github("devanshagr/CrossValidation")
library(CrossValidation)

setwd("/Users/lainam/Desktop/PredictionChallenge1")
list.files()
library(rpart)
library(rpart.plot)
library(dplyr)
library(CrossValidation)
library(bestNormalize)

train <- read.csv("CarsTrainNew.csv")
test <- read.csv("CarsTestNew+Truncated.csv")

clean_data <- function(df) {
  df <- df %>%
    mutate(across(where(is.character) & !matches("Deal"),
                  ~ tolower(trimws(.)))) %>%
    mutate(across(!where(is.character), as.numeric))
  return(df)
}


train <- clean_data(train)
test  <- clean_data(test)
train$Deal <- factor(train$Deal)

train$Deal

summary(train)

train |> summarise(across(everything(), ~ sum(is.na(.x))))


str(train)

str(train)
colnames(train)
colSums(is.na(test))


#Calculate Skewness 
skewness <- function(x){ 
  x <- x[is.finite(x)]            
  if(length(x)<3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  mean(((x - m) / s)^3)           # skewness
}

# choose numeric
num_cols <- names(train)[sapply(train, is.numeric)]

#  skewness each column
skew_df <- data.frame(
  var = num_cols, 
  skew = sapply(train[num_cols], skewness)
) %>% arrange(desc(abs(skew)))

print(skew_df)


train$log_price <- log(pmax(train$Price, 1))
train$log_mileage <- log(pmax(train$Mileage, 0) + 1)

train$ratio_pm <- train$log_price - train$log_mileage

# (1) Diff Price and Benchmark
train$DiffLogPriceBench <- train$log_price - train$ValueBenchmark


# (2) ABS for DiffLogPriceBench
train$AbsDiffLogPriceBench <- abs(train$DiffLogPriceBench)

# (6) Interaction  benchmark and log_mileage
train$VB_x_Mileage <- train$ValueBenchmark * train$Mileage

#(7) Overprice
train$OverPrice_xM <- train$AbsDiffLogPriceBench * train$Mileage


train$LogP_xM <- train$log_price * train$Mileage

#train$MileageBand <- cut(train$Mileage,
#                        breaks = c(0, 60000, 120000, 200000, 300000),
#                         labels = c("Low","Mid","High","VeryHigh"),
#                         include.lowest = TRUE)



# --- Boxplot for Deal ---
par(mfrow=c(2,3))
hist(train$Price, main="Histogram Price", xlab="Price")
hist(train$log_price, main="Histogram log(Price)", xlab="log(Price)")
hist(train$Mileage, main="Histogram Mileage", xlab="Mileage")
hist(train$log_mileage, main="Histogram log(Mileage)", xlab="log(Mileage)")
hist(train$ratio_pm, main="Histogram log", xlab="log(ratio_pm)")
par(mfrow=c(1,1))

# --- Scatter ---
par(mfrow=c(2,2))
plot(train$Mileage, train$Price, xlab="Mileage", ylab="Price",
     main="Mileage vs Price")
plot(train$log_mileage, train$log_price, xlab="log(Mileage)", ylab="log(Price)",
     main="log(Mileage) vs log(Price)")
par(mfrow=c(1,1))

# --- Mosaic: Make x Deal, Location x Deal ---
topMakes <- names(sort(table(train$Make), decreasing=TRUE)[1:10])
tmp <- droplevels(train[train$Make %in% topMakes, c("Make","Deal")])
mosaicplot(table(tmp$Make, tmp$Deal), color=TRUE, las=2,
           main="Mosaic: Make x Deal")


topLocations <- names(sort(table(train$Location), decreasing=TRUE)[1:10])
tmp <- droplevels(train[train$Location %in% topLocations, c("Location","Deal")])

mosaicplot(table(tmp$Location, tmp$Deal), 
           color=c("lightblue", "lightcoral"),  # Màu tùy chỉnh
           las=2,
           main="Mosaic: Location x Deal",
           xlab="Location",
           ylab="Deal")



unique(train$Model)
unique(train$Location)

table(train$Make)
table(test$Make)


table(train$Model)
table(test$Model)


table(train$Location)
table(test$Location)


# Top 10 highest price
head(train[order(-train$Price), c("Make", "Model", "Price", "Mileage", "Location","Deal")], 10)

aggregate(Price ~ Make, data = train, mean, na.rm = TRUE) |>
  transform(Price = round(Price, 0)) 

#down of price
aggregate(Price ~ Make, data = train, mean, na.rm = TRUE)[order(-aggregate(Price ~ Make, data = train, mean, na.rm = TRUE)$Price), ]


table(train$Make, train$Deal)


#scatter
plot(train$Mileage, train$Price,
     main = "Price vs Mileage",
     xlab = "Mileage",
     ylab = "Price",
     col = "blue",
     pch = 19)

#log
plot(train$log_mileage, train$log_price,
     main = "Price vs Mileage",
     xlab = "Mileage",
     ylab = "Price",
     col = "blue",
     pch = 19)


plot(train$ValueBenchmark, train$Price,
     main = "Price vs ValueBenchmark",
     xlab = "ValueBenchmark",
     ylab = "Price",
     col = "darkgreen",
     pch = 19)

#log
plot(train$ValueBenchmark, train$log_price,
     main = "Price vs ValueBenchmark",
     xlab = "ValueBenchmark",
     ylab = "Price",
     col = "darkgreen",
     pch = 19)


plot(train$ValueBenchmark, train$Mileage,
     main = "Mileage vs ValueBenchmark",
     xlab = "ValueBenchmark",
     ylab = "Mileage",
     col = "orange",
     pch = 19)


plot(train$ValueBenchmark, train$log_mileage,
     main = "Log Mileage vs  ValueBenchmark",
     xlab = "ValueBenchmark",
     ylab = "Mileage",
     col = "orange",
     pch = 19)

#boxplot
boxplot(Price ~ Deal, data = train,
        main = "Price on Deal",
        xlab = "Deal",
        ylab = "Price",
        col = "lightblue")
#log
boxplot(log_price ~ Deal, data = train,
        main = "Price on Deal",
        xlab = "Deal",
        ylab = "Price",
        col = "lightblue")

boxplot(Mileage ~ Deal, data = train,
        main = "Mileage on Deal",
        xlab = "Deal",
        ylab = "Mileage",
        col = "lightgreen")
#log 
boxplot(log_mileage ~ Deal, data = train,
        main = "Mileage on Deal",
        xlab = "Deal",
        ylab = "Mileage",
        col = "lightgreen")


boxplot(ValueBenchmark ~ Deal, data = train,
        main = "ValueBenchmark on Deal",
        xlab = "Deal",
        ylab = "ValueBenchmark",
        col = "lightpink")

#Barplot 
barplot(table(train$Deal),
        main = "Num of Car on Deal",
        xlab = "Deal",
        ylab = "Num",
        col = "skyblue")



cv_df <- train[, c("Deal", "Price", "Mileage", "ValueBenchmark", "ratio_pm", "log_price", "AbsDiffLogPriceBench")]
head(cv_df)
tree_deal <- rpart(
  Deal ~ log_price + Mileage + ValueBenchmark + ratio_pm+ AbsDiffLogPriceBench,
  data   = cv_df,
  method = "class",
  control = rpart.control(
    xval = 0,
    maxdepth = 30,
    minsplit = 7
  )
)
rpart.plot(tree_deal, main = "Decision tree: Deal ~ Features")

train_pred_class <- predict(tree_deal, newdata = cv_df, type = "class")  
table(train$Deal, train_pred_class)
mean(train$Deal == train_pred_class, na.rm=TRUE)


cv_output <-CrossValidation::cross_validate(cv_df, tree_deal, 10, 0.8)


cv_output[[1]]
cv_output[[2]]






#random forest 

rf_df <- train[, c("Deal","Mileage","AbsDiffLogPriceBench", "LogP_xM", "VB_x_Mileage","OverPrice_xM")]

#rf_df <- train[, c("Deal","Price", "Mileage", "ValueBenchmark","log_price", "log_mileage", "ratio_pm","AbsDiffLogPriceBench","VB_x_LogMileage","MileageBand", "LogP_x_LogM")]

#rf_df <- train[, c("Deal", "ValueBenchmark","log_price","Mileage","AbsDiffLogPriceBench","ratio_pm", "LogP_xM","VB_x_Mileage","OverPrice_xM" )]

#rf_df <- train[, c("Deal","Mileage","AbsDiffLogPriceBench", "LogP_xM" )]



colnames(train)



set.seed(101)

p <- ncol(rf_df) - 1   
mtry_default <- floor(sqrt(p))

rf_basic <- randomForest(
  Deal ~ .,
  data      = rf_df,
  ntree     = 500,        
  mtry      = mtry_default,
  nodesize  = 5,          
  importance = TRUE
)

print(rf_basic)   # OOB error & confusion matrix

# OOB accuracy 
oob_err  <- rf_basic$err.rate[rf_basic$ntree, "OOB"]
oob_acc  <- 1 - oob_err
oob_acc


# Train accuracy 
rf_train_pred <- predict(rf_basic, newdata = rf_df, type = "class")
rf_train_acc  <- mean(rf_train_pred == rf_df$Deal)
rf_train_acc



#Train final model
best_mtry    <- 2   
best_nodesize <- 10 

set.seed(101)
rf_final <- randomForest(
  Deal ~ .,
  data      = rf_df,
  ntree     = 800,          # final model 
  mtry      = best_mtry,
  nodesize  = best_nodesize,
  importance = TRUE
)

print(rf_final)
oob_err_final <- rf_final$err.rate[rf_final$ntree, "OOB"]
oob_acc_final <- 1 - oob_err_final
oob_acc_final

rf_final_pred_train <- predict(rf_final, rf_df, type = "class")
rf_final_train_acc  <- mean(rf_final_pred_train == rf_df$Deal)
rf_final_train_acc

# Print importance of features
importance(rf_final)
varImpPlot(rf_final, main = "Random Forest - Variable Importance")


#CV
set.seed(101)
K <- 10
n <- nrow(rf_df)

# create k fold 
folds <- sample(rep(1:K, length.out = n))

cv_acc <- numeric(K)

for (k in 1:K) {

  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)
  
  rf_k <- randomForest(
    Deal ~ .,
    data      = rf_df[train_idx, ],
    ntree     = 800,
    mtry      = best_mtry,     
    nodesize  = best_nodesize  
  )
  
  pred_k <- predict(rf_k, newdata = rf_df[test_idx, ], type = "class")
  cv_acc[k] <- mean(pred_k == rf_df$Deal[test_idx])
}

cv_acc          # accuracy for fold
mean(cv_acc)    # CV accuracy mean
var(cv_acc)   




# FEATURE ENGINEERING for TEST 

test$log_price    <- log(pmax(test$Price, 1))
test$log_mileage  <- log(pmax(test$Mileage, 0) + 1)

test$ratio_pm <- test$log_price - test$log_mileage

test$DiffLogPriceBench    <- test$log_price - test$ValueBenchmark
test$AbsDiffLogPriceBench <- abs(test$DiffLogPriceBench)

test$VB_x_Mileage <- test$ValueBenchmark * test$Mileage
test$OverPrice_xM <- test$AbsDiffLogPriceBench * test$Mileage

test$LogP_xM <- test$log_price * test$Mileage



rf_df <- train[, c("Deal","Mileage","AbsDiffLogPriceBench", "LogP_xM", "VB_x_Mileage","OverPrice_xM")]


test_rf_df <- test[, c("Mileage", "AbsDiffLogPriceBench", 
                       "LogP_xM", "VB_x_Mileage", "OverPrice_xM")]
test_pred <- predict(rf_final, 
                     newdata = test_rf_df, 
                     type = "class")
test_pred

sum(is.na(test_pred))

colnames(test)


#Submit

submission <- data.frame(
  ID   = test$id,         #from test
  Deal = as.character(test_pred)
)

write.csv(submission,
          file = "submission.csv",
          row.names = FALSE)


#Create data test file
test_original <- read.csv("CarsTestNew+Truncated.csv")
test_original <- clean_data(test_original)

# add predict column
test_original$Deal <- as.character(test_pred)

# creat new file
write.csv(
  test_original,
  file = "test_with_predictions.csv",
  row.names = FALSE
)



