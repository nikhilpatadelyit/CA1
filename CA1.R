# Importing the Dataset into a DF
heart_data <- read.csv("heart.csv", na="")

# Display the first six entries from the DF
head(heart_data)

# Structure of DF
str(heart_data)

# Verifying that it is a DF
class(heart_data)

# Displaying the number of rows and columns
nrow(heart_data)
ncol(heart_data)

# Display the summary of the data
summary(heart_data)

# Rename the column names
colnames(heart_data)
name <- c("Age","Sex", "Chest_pain", "Resting_BP", "Cholestoral", "Fasting_BS", 
          "Resting_ECG", "Max_heartrate", "Excercise_angina", "Oldpeak", "Slope", "Num_major_vessel", 
          "Thall", "Target")
names(heart_data) <- name

# Display the DF with column-names
colnames(heart_data)

# Displaying the summary
summary(heart_data)

# Changing the columns values to their corresponding values needed
# used function (ifelse)
heart_data$Sex <- ifelse((heart_data$Sex == 1), 'Male', 'Female')
heart_data$Excercise_angina <- ifelse((heart_data$Excercise_angina == 1), 'Yes', 'No')
heart_data$Fasting_BS <- ifelse((heart_data$Fasting_BS == 1), 'True', 'False')
heart_data$Target <- ifelse((heart_data$Target == 1), 'More chance of heart attack', 'Less chance of heart attack')

# Display the DF
head(heart_data)

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(heart_data))

# Fetching the required columns for processing &
# storing them to a new DF
new_heart_data <- heart_data[, c(1,2,3,4,5,6,7,8,9,12,14)]

# Display the structure of DF
str(new_heart_data)

# Displays the chances of people getting a heart-attack &
# not getting the heart-attack
table(new_heart_data$Target)

#Imporitng the libraries
#library(ggplot)
#library(dplyr)
#library(caTools)
#library(gridExtra)
#library(data.table)
#library(tidyr)

# Analysing the chances of heart-attack with the sex-gender
table(new_heart_data$Target, new_heart_data$Sex)

table(new_heart_data$Age)











