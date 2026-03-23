# --------------------------------------------------
# Farmer Distress Early Warning System
# Model Training Script
# --------------------------------------------------

# Load required libraries
library(randomForest)
library(dplyr)

# --------------------------------------------------
# Load Dataset
# --------------------------------------------------

data <- read.csv("data/tamilnadu_farmer_distress_dataset.csv")

# Convert district to factor
data$District <- as.factor(data$District)

# --------------------------------------------------
# Normalization function
# --------------------------------------------------

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

# --------------------------------------------------
# Create Risk Indices
# --------------------------------------------------

# Climate risk
data$Rainfall_Risk <- normalize(-data$Rainfall_Deviation_pct)
data$Temp_Risk <- normalize(data$Temperature_Anomaly_C)

data$CRI <- 0.6 * data$Rainfall_Risk +
  0.4 * data$Temp_Risk

# Crop stress
data$Paddy_Risk <- normalize(-data$Paddy_Yield_t_ha)
data$Sugarcane_Risk <- normalize(-data$Sugarcane_Yield_t_ha)
data$Groundnut_Risk <- normalize(-data$Groundnut_Yield_t_ha)
data$Maize_Risk <- normalize(-data$Maize_Yield_t_ha)

data$CSI <- (data$Paddy_Risk +
               data$Sugarcane_Risk +
               data$Groundnut_Risk +
               data$Maize_Risk) / 4

# Market risk
data$Paddy_Price_Risk <- normalize(-data$Paddy_Price_Rs_qtl)
data$Sugarcane_Price_Risk <- normalize(-data$Sugarcane_Price_Rs_ton)
data$Groundnut_Price_Risk <- normalize(-data$Groundnut_Price_Rs_qtl)
data$Maize_Price_Risk <- normalize(-data$Maize_Price_Rs_qtl)

data$MRI <- (data$Paddy_Price_Risk +
               data$Sugarcane_Price_Risk +
               data$Groundnut_Price_Risk +
               data$Maize_Price_Risk) / 4

# Final risk score
data$Distress_Risk_Score <- 0.4 * data$CRI +
  0.35 * data$CSI +
  0.25 * data$MRI

# --------------------------------------------------
# Train Random Forest Model
# --------------------------------------------------

rf_model <- randomForest(
  as.factor(Distress_Label) ~ CRI + CSI + MRI + Distress_Risk_Score,
  data = data,
  ntree = 200
)

print(rf_model)

# --------------------------------------------------
# Save Model
# --------------------------------------------------

saveRDS(rf_model, "model/farmer_distress_rf_model.rds")

cat("Model saved successfully in models folder\n")