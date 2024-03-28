getwd()
# Adjust the file path and options as necessary
setwd("C:/Users/parmi/cmpt318/cybersecurity_final/")

DataDf <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")

#DataDf <- read.table("household_power_consumption.txt", header = T, sep = ",")
# I think you need to install packages before here 

library(ggplot2)
library(dplyr)
library(zoo)

# Applying linear interpolation for each column  

df <- DataDf # Create a copy of the data to store interpolated values
#interpolate missing values in each column
df <- df %>%
  mutate(Global_active_power = na.approx(Global_active_power))
df <- df %>%
  mutate(Global_reactive_power = na.approx(Global_reactive_power))
df <- df %>%
  mutate(Voltage  = na.approx(Voltage ))
df <- df %>%
  mutate(Global_intensity = na.approx(Global_intensity))
df <- df %>%
  mutate(Sub_metering_1  = na.approx(Sub_metering_1 ))
df <- df %>%
  mutate(Sub_metering_2  = na.approx(Sub_metering_2 ))
df <- df %>%
  mutate(Sub_metering_3  = na.approx(Sub_metering_3 ))
#view updated data frame
#checking and comparing the filled in values
df[6841,]
DataDf[6841,]
###############################################
# Calculation for Z score 
# For multiple columns/features, calculate the Z score for each feature and the Anomalies of each feature
dataf <- df
features_to_calculate <- c("Global_active_power","Global_reactive_power", "Voltage","Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3") 
for(feature_name in features_to_calculate) {
  # Create a column in table dataf that lists all the Z-scores for the corresponding feature 
  z_score_features_name <- paste(feature_name, "Z_score", sep = "_")
  # Calculate the Z-score of each feature and save it into the corresponding feature Z_score
  dataf[[z_score_features_name]] <- (dataf[[feature_name]] - mean(dataf[[feature_name]])) / sd(dataf[[feature_name]])
  
  # Assuming features_to_calculate contains the names of the original columns/features
  # Now, each column/feature has an associated 'anomaly' column indicating point anomalies
  # In the table dataf  TRUE = point anomaly (outliar)
  # In the table dataf  FALSE = not a point anomaly (outliar)
  anomaly_feature_name <- paste(feature_name, "anomaly", sep = "_")
  dataf[[anomaly_feature_name]] <- abs(dataf[[z_score_features_name]]) > 3
}
################################################
# Calculate the percentage of anomalies for each feature
percentage <- sapply(features_to_calculate, function(feature_name) {
  anomaly_feature_name <- paste(feature_name, "anomaly", sep = "_")
  percentage <- mean(dataf[[anomaly_feature_name]] == TRUE) * 100
  return(percentage)
})

# Print the percentages for each feature
print(percentage)

# Calculate overall percentage of anomalies across all features
overall_anomaly_flags <- dataf[, grep("anomaly$", names(dataf))]
dataf$any_anomaly <- rowSums(overall_anomaly_flags == TRUE) > 0
overall_percentage <- mean(dataf$any_anomaly) * 100

# Print the overall percentage of anomalies in the dataset
paste("Standardization---The overall anomaly percentage using Z-Score: ",overall_percentage)
