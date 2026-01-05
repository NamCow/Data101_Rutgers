# Clear workspace
rm(list = ls())


# ============================================================================
# 1. SETUP & LIBRARIES
# ============================================================================
library(randomForest)
library(ggplot2)
library(dplyr)
library(scales)

# Set working directory
setwd("/Users/lainam/Desktop/PredictionChallenge3")

# Set random seed for reproducibility
set.seed(42)

# ============================================================================
# 2. LOAD DATA
# ============================================================================
cat("Loading data...\n")

training_data <- read.csv('Prediction3Train.csv')
test_data <- read.csv('Prediction3Test-truncated.csv')
university_locations <- read.csv('Location.csv')

cat("Training samples:", nrow(training_data), "\n")
cat("Test samples:", nrow(test_data), "\n")

# ============================================================================
# 3. DEFINE CLASSIFICATION RULES
# ============================================================================

# Blue states (Democratic-leaning) - Tuned list based on data analysis
# Note: AZ and WI excluded as they behave like Red states in this dataset
BLUE_STATES <- c('CA', 'CT', 'IL', 'MA', 'MD', 'NM', 'NY', 'OR', 'VA', 'WA')

# STEM majors
STEM_MAJORS <- c('Computer Science', 'Physics', 'Mathematics', 
                 'Data Science', 'Mechanical Engineering')

# ============================================================================
# 4. FEATURE ENGINEERING FUNCTION
# ============================================================================

create_features <- function(data, location_data) {
  # Merge with location data
  data_merged <- merge(data, location_data, by = 'University', all.x = TRUE)
  
  # Create features
  data_merged <- data_merged %>%
    mutate(
      # Political alignment: 1 = Blue State, 0 = Red State
      is_blue_state = ifelse(State %in% BLUE_STATES, 1, 0),
      
      # Major category: 1 = STEM, 0 = Humanities
      is_stem_major = ifelse(Major %in% STEM_MAJORS, 1, 0),
      
      # Political fit: Measures alignment between state politics and major
      # Fit = 1 when (Blue State & Humanities) OR (Red State & STEM)
      # This captures the hidden hiring pattern
      political_fit = ifelse(is_blue_state != is_stem_major, 1, 0),
      
      # Labels for visualization
      state_type = ifelse(is_blue_state == 1, "Blue State", "Red State"),
      major_type = ifelse(is_stem_major == 1, "STEM", "Humanities")
    )
  
  return(data_merged)
}

# ============================================================================
# 5. PREPARE DATASETS
# ============================================================================
cat("\nPreparing features...\n")

training_set <- create_features(training_data, university_locations)
test_set <- create_features(test_data, university_locations)

# Define feature columns for modeling
MODEL_FEATURES <- c('GPA', 'is_blue_state', 'is_stem_major', 'political_fit')

# ============================================================================
# 6. EXPLORATORY DATA ANALYSIS & VISUALIZATIONS
# ============================================================================
cat("\nGenerating visualizations...\n")

# ---- Plot 1: Hiring Rate by State Type and Major Type ----
hiring_by_group <- training_set %>%
  group_by(state_type, major_type) %>%
  summarise(
    hiring_rate = mean(Hired),
    count = n(),
    .groups = 'drop'
  )

plot1 <- ggplot(hiring_by_group, aes(x = major_type, y = hiring_rate, 
                                     fill = state_type)) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  geom_text(aes(label = paste0(round(hiring_rate * 100, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("Blue State" = "#4575b4", 
                               "Red State" = "#d73027")) +
  labs(
    title = "Hiring Rate by State Politics and Major Type",
    subtitle = "Clear pattern: Blue states prefer Humanities, Red states prefer STEM",
    x = "Major Type",
    y = "Hiring Rate",
    fill = "State Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

# ---- Plot 2: GPA Distribution by Hiring Status ----
plot2 <- ggplot(training_set, aes(x = factor(Hired), y = GPA, 
                                  fill = factor(Hired))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  scale_fill_manual(values = c("0" = "#fc8d62", "1" = "#66c2a5"),
                    labels = c("Not Hired", "Hired")) +
  labs(
    title = "GPA Distribution by Hiring Outcome",
    x = "Hiring Status",
    y = "GPA",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

# ---- Plot 3: Political Fit Impact ----
hiring_by_fit <- training_set %>%
  mutate(fit_label = ifelse(political_fit == 1, 
                            "Good Fit", 
                            "Poor Fit")) %>%
  group_by(fit_label) %>%
  summarise(
    hiring_rate = mean(Hired),
    count = n(),
    .groups = 'drop'
  )

plot3 <- ggplot(hiring_by_fit, aes(x = fit_label, y = hiring_rate, 
                                   fill = fit_label)) +
  geom_bar(stat = 'identity', alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(hiring_rate * 100, 1), "%\n(n=", 
                               count, ")")), 
            vjust = 1.5, color = "white", size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("Good Fit" = "#1b9e77", 
                               "Poor Fit" = "#d95f02")) +
  labs(
    title = "Impact of Political Fit on Hiring",
    subtitle = "Political Fit = (Blue State & Humanities) OR (Red State & STEM)",
    x = "Political Fit",
    y = "Hiring Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# ---- Plot 4: Feature Distribution Heatmap ----
feature_summary <- training_set %>%
  group_by(state_type, major_type) %>%
  summarise(
    avg_gpa = mean(GPA),
    hiring_rate = mean(Hired),
    .groups = 'drop'
  )

plot4 <- ggplot(feature_summary, aes(x = major_type, y = state_type, 
                                     fill = hiring_rate)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(hiring_rate * 100, 0), "%\n",
                               "GPA: ", round(avg_gpa, 2))), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee090", high = "#1a9850",
                       midpoint = 0.5, labels = percent) +
  labs(
    title = "Hiring Rate Heatmap",
    subtitle = "Shows hiring rate and average GPA by state and major type",
    x = "Major Type",
    y = "State Type",
    fill = "Hiring Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid = element_blank()
  )

# Display all plots
print(plot1)
print(plot2)
print(plot3)
print(plot4)

# ============================================================================
# 7. TRAIN RANDOM FOREST MODEL
# ============================================================================
cat("\nTraining Random Forest model...\n")

# Prepare training matrices
X_train <- training_set[, MODEL_FEATURES]
y_train <- as.factor(training_set$Hired)

# Train model with 1000 trees for stability
rf_model <- randomForest(
  x = X_train, 
  y = y_train, 
  ntree = 1000, 
  importance = TRUE,
  do.trace = 100  # Show progress every 100 trees
)

# ============================================================================
# 8. MODEL EVALUATION
# ============================================================================
cat("\n" , rep("=", 60), "\n", sep = "")
cat("MODEL PERFORMANCE\n")
cat(rep("=", 60), "\n", sep = "")

# Out-of-Bag accuracy
oob_accuracy <- 1 - rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
cat("Out-of-Bag Accuracy:", round(oob_accuracy * 100, 2), "%\n")

# Feature importance
cat("\nFeature Importance:\n")
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>%
  arrange(desc(MeanDecreaseGini))

print(importance_df)

# ---- Plot 5: Feature Importance ----
plot5 <- ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), 
                                   y = MeanDecreaseGini, fill = Feature)) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "Based on Mean Decrease in Gini impurity",
    x = "Feature",
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(plot5)

# ============================================================================
# 9. GENERATE PREDICTIONS
# ============================================================================
cat("\nGenerating predictions...\n")

X_test <- test_set[, MODEL_FEATURES]
predictions <- predict(rf_model, X_test)

# Create submission file
submission <- data.frame(
  ID = test_set$ID, 
  Hired = as.integer(as.character(predictions))
)

# Sort by ID to ensure correct order
submission <- submission %>% arrange(ID)

# Save predictions
output_file <- 'submission.csv'
write.csv(submission, output_file, row.names = FALSE)

cat("\n" , rep("=", 60), "\n", sep = "")
cat("PREDICTION COMPLETE\n")
cat(rep("=", 60), "\n", sep = "")
cat("Predictions saved to:", output_file, "\n")
cat("Total predictions:", nrow(submission), "\n")
cat("Predicted hires:", sum(submission$Hired), "\n")
cat("Predicted rejection:", sum(1 - submission$Hired), "\n")


