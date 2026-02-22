1. Pre----
  
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(factoextra)
library(caret)
library(class)
library(kknn)
library(ROCR)
library(pROC)
library(readr)
Indian_Liver_Patient_Dataset <- read_csv("~/Documents/Indian Liver Patient Dataset (ILPD).csv")
liver_data <- Indian_Liver_Patient_Dataset
# Add column names
colnames(liver_data) <- c("Age", "Gender", "Total_Bilirubin", "Direct_Bilirubin", 
                        "Alkaline_Phosphotase", "Alamine_Aminotransferase", 
                        "Aspartate_Aminotransferase", "Total_Proteins", 
                        "Albumin", "Albumin_Globulin_Ratio", "Liver_Disease")

2. Data cleaning----
  # check for missing values
missing_summary <- colSums(is.na(liver_data))
print(missing_summary)

  # Replace NA with meidan
median_ag_ratio <- median(liver_data$Albumin_Globulin_Ratio, na.rm = TRUE)
liver_data$Albumin_Globulin_Ratio[is.na(liver_data$Albumin_Globulin_Ratio)] <- median_ag_ratio
print(colSums(is.na(liver_data)))

  # Convert variables
liver_data$Gender <- factor(liver_data$Gender, levels = c("Female", "Male"))
liver_data$Gender <- as.integer(liver_data$Gender) - 1

 # Keep numeric version for EDA, create factor version for modeling
liver_data$Liver_Disease_Numeric <- liver_data$Liver_Disease
liver_data$Liver_Disease <- factor(liver_data$Liver_Disease, 
                                   levels = c(1, 2),
                                   labels = c("Patient", "Non-Patient"))

3. Exploratory data anlaysis-----
summary_stats <- summary(liver_data)
print(summary_stats)

 # 1.Distribution of target variable
dist_table <- table(liver_data$Liver_Disease)
dist_percent <- prop.table(dist_table) * 100

# Pie chart 
png("chart1_target_distribution.png", width = 800, height = 600)
par(mfrow = c(1, 2))

pie(dist_table, 
    labels = paste0(names(dist_table), "\n", round(dist_percent, 1), "%"),
    col = c("lightcoral", "lightblue"),
    main = "Liver Disease Distribution",
    cex.main = 1.5)

# Bar chart with counts
barplot(dist_table, 
        col = c("lightcoral", "lightblue"),
        main = "Patient vs Non-Patient Counts",
        ylab = "Number of Patients",
        ylim = c(0, 450),
        cex.main = 1.5,
        cex.names = 1.2)
text(x = c(0.7, 1.9), y = dist_table + 20, labels = dist_table, cex = 1.5)

par(mfrow = c(1, 1))
dev.off()

cat("\n=== CHART 1: Target Variable Distribution ===\n")
cat("Patients:", dist_table["Patient"], "(", round(dist_percent["Patient"], 1), "%)\n")
cat("Non-Patients:", dist_table["Non-Patient"], "(", round(dist_percent["Non-Patient"], 1), "%)\n")

 # 2. Boxplots
png("chart2_key_indicators.png", width = 1000, height = 800)

# Select the 4 most important medical indicators
p1 <- ggplot(liver_data, aes(x = Liver_Disease, y = Total_Bilirubin, fill = Liver_Disease)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Total Bilirubin", 
       subtitle = "Higher in liver patients",
       x = "", y = "mg/dL") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("lightcoral", "lightblue"))

p2 <- ggplot(liver_data, aes(x = Liver_Disease, y = Albumin, fill = Liver_Disease)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Albumin", 
       subtitle = "Lower in liver patients",
       x = "", y = "g/dL") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("lightcoral", "lightblue"))

p3 <- ggplot(liver_data, aes(x = Liver_Disease, y = Alkaline_Phosphotase, fill = Liver_Disease)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Alkaline Phosphotase", 
       subtitle = "Higher in liver patients",
       x = "", y = "U/L") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("lightcoral", "lightblue"))

p4 <- ggplot(liver_data, aes(x = Liver_Disease, y = Aspartate_Aminotransferase, fill = Liver_Disease)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "AST (Aspartate Aminotransferase)", 
       subtitle = "Higher in liver patients",
       x = "", y = "U/L") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("lightcoral", "lightblue"))

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
             top = "Key Liver Function Indicators: Patient vs Non-Patient")

dev.off()

 # 3. Age distribution
png("chart3_age_distribution.png", width = 900, height = 600)

age_plot <- ggplot(liver_data, aes(x = Age, fill = Liver_Disease)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20, color = "white") +
  labs(title = "Age Distribution: Liver Disease Risk Increases with Age",
       subtitle = "Patients show higher proportion in older age groups",
       x = "Age (years)", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("Patient" = "lightcoral", "Non-Patient" = "lightblue"))

print(age_plot)
dev.off()

 # 4. PCA for dimension reduction
numeric_cols <- c("Age", "Total_Bilirubin", "Direct_Bilirubin", 
                  "Alkaline_Phosphotase", "Alamine_Aminotransferase", 
                  "Aspartate_Aminotransferase", "Total_Proteins", 
                  "Albumin", "Albumin_Globulin_Ratio")

pca_data <- liver_data[, numeric_cols]
pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)
cat("\nPCA Summary - Variance Explained:\n")
pca_summary <- summary(pca_result)
print(pca_summary$importance[, 1:5])
# Scree plot

png("pca_scree_plot.png", width = 800, height = 600)
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(var_explained[1:10], type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot: Variance Explained by Each PC",
     col = "blue", pch = 19)
abline(h = 0.1, col = "red", lty = 2)
text(2, 0.15, "First 3 PCs explain 63% of variance", col = "darkgreen")
dev.off()

# Biplot - show which variables contribute most to PC1 and PC2
png("pca_biplot.png", width = 900, height = 700)
fviz_pca_biplot(pca_result, 
                col.ind = liver_data$Liver_Disease,
                palette = c("red", "blue"),
                addEllipses = TRUE,
                ellipse.level = 0.95,
                title = "PCA Biplot: Variable Contributions",
                repel = TRUE) +
  theme_minimal()
dev.off()

# Get top contributing variables to PC1 (not necessarily most important for prediction)
pc1_loadings <- abs(pca_result$rotation[, 1])
top_pc1_vars <- sort(pc1_loadings, decreasing = TRUE)[1:5]
cat("\nTop variables contributing to PC1:\n")
print(round(top_pc1_vars, 3))

  # 5. K-means

pca_scores <- as.data.frame(pca_result$x[, 1:5])

# Find optimal number of clusters using elbow method
set.seed(123)
wss <- numeric(10)
for(k in 1:10) {
  kmeans_temp <- kmeans(pca_scores, centers = k, nstart = 25)
  wss[k] <- kmeans_temp$tot.withinss
}

# Elbow plot
png("kmeans_elbow.png", width = 800, height = 600)
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "Within-cluster Sum of Squares",
     main = "Elbow Method for Optimal k",
     col = "blue", pch = 19, lwd = 2)
abline(v = 2, col = "red", lty = 2, lwd = 2)
text(3, max(wss) * 0.9, "Elbow at k=2", col = "red")
dev.off()

# Perform K-means with k=2 (based on elbow)
set.seed(123)
kmeans_result <- kmeans(pca_scores, centers = 2, nstart = 25)

# Add cluster assignments to data
liver_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
png("kmeans_clusters.png", width = 900, height = 700)
cluster_plot <- fviz_cluster(kmeans_result, data = pca_scores,
                             geom = "point",
                             ellipse.type = "norm",
                             palette = c("lightcoral", "lightblue"),
                             main = "K-means Clustering Results",
                             subtitle = "2 Natural Clusters Found in Patient Data") +
  theme_minimal()
print(cluster_plot)
dev.off()

# Check cluster composition by actual disease status
cluster_disease <- table(liver_data$Cluster, liver_data$Liver_Disease)
cat("\nCluster vs Actual Disease Status:\n")
print(cluster_disease)

# Calculate agreement
agreement <- sum(diag(cluster_disease)) / sum(cluster_disease)
cat("\nCluster-disease agreement:", round(agreement, 3))

  # Data partitioning
set.seed(123)
train_index <- createDataPartition(liver_data$Liver_Disease, p = 0.7, list = FALSE)
train_data <- liver_data[train_index, ]
test_data <- liver_data[-train_index, ]

cat("\nTraining set size:", nrow(train_data))
cat("\n  Patients:", table(train_data$Liver_Disease)["Patient"])
cat("\n  Non-Patients:", table(train_data$Liver_Disease)["Non-Patient"])
cat("\n\nTesting set size:", nrow(test_data))
cat("\n  Patients:", table(test_data$Liver_Disease)["Patient"])
cat("\n  Non-Patients:", table(test_data$Liver_Disease)["Non-Patient"])

  # Simplified SMOTE
simplified_smote <- function(data, target_col = "Liver_Disease") {
# Separate majority and minority classes
patients <- data[data[[target_col]] == "Patient", ]
non_patients <- data[data[[target_col]] == "Non-Patient", ]
  
cat("\nOriginal class sizes:")
cat("\n  Patients:", nrow(patients))
cat("\n  Non-Patients:", nrow(non_patients))
# Check if oversampling is needed
if(nrow(patients) > nrow(non_patients) * 1.2) {  # 20% imbalance threshold
  cat("\n\nData is imbalanced. Performing oversampling...")
    
# Oversample minority class (non-patients) to match majority
set.seed(123)
minority_oversampled <- non_patients[sample(1:nrow(non_patients), 
                                                nrow(patients), 
                                                replace = TRUE), ]
    
# Combine
balanced_data <- rbind(patients, minority_oversampled)

cat("\nBalanced class sizes:")
cat("\n  Patients:", sum(balanced_data[[target_col]] == "Patient"))
cat("\n  Non-Patients:", sum(balanced_data[[target_col]] == "Non-Patient"))

return(balanced_data)
} else {
  cat("\nData is already balanced. No oversampling needed.")
  return(data)
}
}

# Apply simplified SMOTE
train_balanced <- simplified_smote(train_data)

# Verify balancing
cat("\n\nFinal balanced training set distribution:\n")
print(table(train_balanced$Liver_Disease))

 # 6. Model1 - Logistic regression
# Train on balanced data
lr_model <- glm(Liver_Disease ~ Age + Gender + Total_Bilirubin + 
                  Direct_Bilirubin + Alkaline_Phosphotase + 
                  Alamine_Aminotransferase + Aspartate_Aminotransferase + 
                  Total_Proteins + Albumin + Albumin_Globulin_Ratio, 
                data = train_balanced, family = binomial)

# Model summary
cat("\nModel Summary (significant predictors marked with *):\n")
lr_summary <- summary(lr_model)
print(lr_summary$coefficients[which(lr_summary$coefficients[, 4] < 0.05), ])

# Predictions
lr_prob <- predict(lr_model, test_data, type = "response")
lr_pred <- ifelse(lr_prob > 0.5, "Patient", "Non-Patient")
lr_pred <- factor(lr_pred, levels = c("Patient", "Non-Patient"))

# Confusion Matrix
lr_cm <- confusionMatrix(lr_pred, test_data$Liver_Disease)
cat("\nConfusion Matrix:\n")
print(lr_cm$table)
cat("\nAccuracy:", round(lr_cm$overall["Accuracy"], 3))
cat("\nSensitivity (detect patients):", round(lr_cm$byClass["Sensitivity"], 3))
cat("\nSpecificity (detect non-patients):", round(lr_cm$byClass["Specificity"], 3))
cat("\nPrecision:", round(lr_cm$byClass["Precision"], 3))
cat("\nF1 Score:", round(lr_cm$byClass["F1"], 3))

# Odds Ratios
cat("\n\nOdds Ratios (risk factors):\n")
odds_ratios <- exp(coef(lr_model))
odds_ratios <- sort(odds_ratios, decreasing = TRUE)
print(round(odds_ratios, 2))

  # 7. Model2- KNN
knn_cols <- c("Age", "Total_Bilirubin", "Direct_Bilirubin", 
              "Alkaline_Phosphotase", "Alamine_Aminotransferase", 
              "Aspartate_Aminotransferase", "Total_Proteins", 
              "Albumin", "Albumin_Globulin_Ratio")

# Normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Prepare training data (using balanced set)
train_knn <- train_balanced[, knn_cols]
train_knn_norm <- as.data.frame(lapply(train_knn, normalize))

# Prepare test data
test_knn <- test_data[, knn_cols]
test_knn_norm <- as.data.frame(lapply(test_knn, normalize))

# Find optimal k using cross-validation
set.seed(123)
ctrl <- trainControl(method = "cv", number = 5)
knn_tune <- train(x = train_knn_norm, 
                  y = train_balanced$Liver_Disease,
                  method = "knn",
                  tuneGrid = expand.grid(k = seq(3, 21, 2)),
                  trControl = ctrl)
best_k <- knn_tune$bestTune$k
cat("\nOptimal k value:", best_k)

# Plot k tuning
png("knn_tuning.png", width = 800, height = 600)
plot(knn_tune, main = paste("KNN Cross-Validation Results (Best k =", best_k, ")"))
dev.off()

# Apply KNN with best k
knn_pred <- knn(train = train_knn_norm, 
                test = test_knn_norm,
                cl = train_balanced$Liver_Disease,
                k = best_k)

# Confusion Matrix
knn_cm <- confusionMatrix(knn_pred, test_data$Liver_Disease)
cat("\n\nConfusion Matrix:\n")
print(knn_cm$table)
cat("\nAccuracy:", round(knn_cm$overall["Accuracy"], 3))
cat("\nSensitivity:", round(knn_cm$byClass["Sensitivity"], 3))
cat("\nSpecificity:", round(knn_cm$byClass["Specificity"], 3))
cat("\nPrecision:", round(knn_cm$byClass["Precision"], 3))
cat("\nF1 Score:", round(knn_cm$byClass["F1"], 3))

  # 9. Model comparison
model_comparison <- data.frame(
  Model = c("Logistic Regression", "K-Nearest Neighbours"),
  Accuracy = c(round(lr_cm$overall["Accuracy"], 3), 
               round(knn_cm$overall["Accuracy"], 3)),
  Sensitivity = c(round(lr_cm$byClass["Sensitivity"], 3), 
                  round(knn_cm$byClass["Sensitivity"], 3)),
  Specificity = c(round(lr_cm$byClass["Specificity"], 3), 
                  round(knn_cm$byClass["Specificity"], 3)),
  Precision = c(round(lr_cm$byClass["Precision"], 3), 
                round(knn_cm$byClass["Precision"], 3)),
  F1_Score = c(round(lr_cm$byClass["F1"], 3), 
               round(knn_cm$byClass["F1"], 3))
)

cat("\nModel Performance Comparison:\n")
print(model_comparison)

png("model_comparison.png", width = 1000, height = 600)
comparison_long <- model_comparison %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

comparison_plot <- ggplot(comparison_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Value), position = position_dodge(0.7), 
            vjust = -0.3, size = 4) +
  facet_wrap(~Metric, nrow = 1) +
  labs(title = "Model Performance Comparison",
       subtitle = paste("Logistic Regression vs KNN (k =", best_k, ") with SMOTE"),
       y = "Score") +
  ylim(0, 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 0, size = 10),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none")
print(comparison_plot)
dev.off()

  # 10. ROC curve comparison
png("roc_comparison.png", width = 900, height = 700)

# ROC for Logistic Regression
lr_roc <- roc(test_data$Liver_Disease, lr_prob, 
              levels = c("Non-Patient", "Patient"))

# Get KNN probabilities using kknn
knn_prob_model <- kknn(Liver_Disease ~ ., 
                       train = train_balanced[, c(knn_cols, "Liver_Disease")],
                       test = test_data[, knn_cols],
                       k = best_k, kernel = "rectangular")
knn_prob <- knn_prob_model$prob[, "Patient"]

# ROC for KNN
knn_roc <- roc(test_data$Liver_Disease, knn_prob, 
               levels = c("Non-Patient", "Patient"))

# Plot
plot(lr_roc, col = "blue", lwd = 2, 
     main = "ROC Curves: Logistic Regression vs KNN")
plot(knn_roc, col = "red", lwd = 2, add = TRUE)
legend("bottomright", 
       legend = c(paste("Logistic Regression (AUC =", round(auc(lr_roc), 3), ")"),
                  paste("KNN (AUC =", round(auc(knn_roc), 3), ")")),
       col = c("blue", "red"), lwd = 2)
dev.off()

4. Summary and recommondations
# Determine best model based on F1 score
if(lr_cm$byClass["F1"] > knn_cm$byClass["F1"]) {
  best_model <- "Logistic Regression"
  best_f1 <- lr_cm$byClass["F1"]
  best_sens <- lr_cm$byClass["Sensitivity"]
  best_spec <- lr_cm$byClass["Specificity"]
  best_auc <- auc(lr_roc)
} else {
  best_model <- "K-Nearest Neighbours"
  best_f1 <- knn_cm$byClass["F1"]
  best_sens <- knn_cm$byClass["Sensitivity"]
  best_spec <- knn_cm$byClass["Specificity"]
  best_auc <- auc(knn_roc)
}

cat("\n=== EXECUTIVE SUMMARY ===\n")
cat("\nBusiness Problem:")
cat("\n  - Early detection of liver disease patients")
cat("\n  - Data: 583 patients with 10 clinical measurements")
cat("\n  - Challenge: 71% patients, 29% non-patients (imbalanced)")

cat("\n\nData Mining Approach:")
cat("\n  1. PCA: Reduced dimensions, first 3 PCs explain", 
    round(sum(var_explained[1:3]) * 100, 1), "% of variance")
cat("\n  2. K-means: Found 2 natural clusters with", 
    round(agreement * 100, 1), "% agreement to actual labels")
cat("\n  3. SMOTE: Balanced training data (", 
    table(train_balanced$Liver_Disease)["Patient"], "vs",
    table(train_balanced$Liver_Disease)["Non-Patient"], ")")
cat("\n  4. Models: Logistic Regression vs KNN")

cat("\n\nResults:")
cat("\n  - Best Model:", best_model)
cat("\n  - Accuracy:", round(model_comparison[model_comparison$Model == best_model, "Accuracy"], 3))
cat("\n  - Sensitivity (detect patients):", round(best_sens, 3))
cat("\n  - Specificity (detect non-patients):", round(best_spec, 3))
cat("\n  - F1 Score:", round(best_f1, 3))
cat("\n  - AUC:", round(best_auc, 3))


cat("\n\n=== RECOMMENDATIONS ===\n")
cat("\n1. Model Deployment:")
cat("\n   - Use", best_model, "for clinical decision support")
cat("\n   - Focus on top 3 features:")

cat("\n\n2. Clinical Interpretation:")
cat("\n   - High Total Bilirubin and Low Albumin are strongest indicators")
cat("\n   - Age is important risk factor")

cat("\n\n3. Future Improvements:")
cat("\n   - Collect more non-patient samples")
cat("\n   - Include lifestyle factors and family history")
cat("\n   - Validate on external datasets")

cat("\n\n4. Limitations:")
cat("\n   - Specificity still moderate (", round(best_spec, 3), ")")
cat("\n   - May need adjustment for different populations")

# Save all results
write.csv(model_comparison, "model_comparison_results.csv", row.names = FALSE)

cat("\n\n Analysis complete! Generated files:\n")
cat("- chart1_target_distribution.png\n")
cat("- chart2_key_indicators.png\n")
cat("- pca_scree_plot.png\n")
cat("- pca_biplot.png\n")
cat("- kmeans_elbow.png\n")
cat("- kmeans_clusters.png\n")
cat("- knn_tuning.png\n")
cat("- model_comparison.png\n")
cat("- roc_comparison.png\n")
cat("- feature_importance.png\n")
cat("- model_comparison_results.csv\n")
cat("- feature_importance.csv\n")





