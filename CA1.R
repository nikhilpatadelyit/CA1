# Analyzing and dealing with the data related to the "HEART"
# for visualizing different variables related to functioning & working 
# of the patients heart and get them analyzed & awareness of their "HEART"

# Importing & Installing the required packages & libraries

# Importing the dataset into a DF
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

# Display the column-names of DF
colnames(heart_data)

# Displaying the summary
summary(heart_data)

# To check if any NA data present
incomplete_data <- heart_data[!complete.cases(heart_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)

# visualize the missing data
# install.packages("VIM")
library(VIM)
missing_values <- aggr(heart_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)
# No missing data present in the DF

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(heart_data))

# Fetching the required columns for processing &
# storing them to a new DF
new_heart_data <- heart_data[, c(1,2,3,4,5,6,7,8,9,12,14)]

# Display the structure of DF
str(new_heart_data)

# Display the column-names of DF
colnames(new_heart_data)

# Installing the library 'psych'
# install.packages("psych")
library(psych)

pairs.panels(new_heart_data, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jitered
             factor = 2, # Jitering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals


############# Question 1:
# Peoples at different ages can get a heart-diseases
############# 
# H0 = More chance of getting a heart-attack between the age(29 to 77)
# H1 = Less chance of getting a heart-attack between the age(29 to 77)

# Convert the Target variable to
# a categorical dichotomous variable with appropriate labels
# 0 = Less chance of getting HA 1 = More chance of getting HA
new_heart_data$Target <- factor(new_heart_data$Target, 
                                labels = c("Less chance of getting HA", 
                                           "More chance of getting HA"))

# Displaying the structure of DF
str(new_heart_data)

# Displays the value of chance people getting a heart-attack &
# not getting the heart-attack
table(new_heart_data$Target)

# Analyse the Age of the patients
table(new_heart_data$Age)

# Analysing the chances of heart-attack according to there Ages
table(new_heart_data$Age, new_heart_data$Target)

# Attach the DF with the function
attach(new_heart_data)

# Plot the graph to analyze the specified attributes
plot(Target, Age, pch = 9, col = "LightBlue", 
     main = "Comaprison of Target with Age", 
     xlab = "Target", ylab = "Age (Years)")

# We can split the dichotomous variable into 
# 2 different visualization & then examine the data
# Importing the library 'lattice'
library("lattice")

# Visualizing the variables
histogram(~Age | Target, 
          data = new_heart_data, 
          main = "Distribution of age with the chance of HA", 
          xlab = "AGE", ylab = "Count of people getting HA")

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(Age, Target, median)

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(Age)

# Add a line that represents the ND
qqline(Age, col = "red")
# Assuming that AGE variable is ND

# Visualizing the Q-Q-Plot for ND variable
with(new_heart_data,
     {qqnorm(Age, 
             main ="Normal Q-Q-Plot of Age", 
             xlab = "Theoritical Quantities", 
             ylab = "Sample Quantities")
             qqline(Age)
     })

# Comparing the two variables
with(new_heart_data, 
     qqplot(Age[Target == "More chance of getting HA"], 
            Age[Target == "Less chance of getting HA"], 
            main = "Comparing 2 samples of Heart Data", 
            xlab = "Age Target = More chance", 
            ylab = "Age Target = Less chance"))

# formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a ND
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_heart_data$Age)
normality_test$p.value
# p.value = 0.00579
# Here p-values tells us the chance that the sample comes from ND
# We observed that p-value is < than 0.05, 
# The AGE var is not ND

# This test does not work on a dichotomous variable
with(new_heart_data, tapply(Age, Target, shapiro.test))

# Results show
# Less chance of getting HA = p-value = 0.002 - It is not ND
# More chance of getting HA = p-value = 0.121 - It is ND

# After consulting the chart, I am aiming
# a dependent var(Age)
# with a independent categorical var(Target)
# Format wilcox.test(dependent var ~ independent var)
wilcox.test(Age~Target)
# cut-off = 0.05
# p-value = 3.439e-05 equals to (0.0003)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the chance of patient getting HA between the 
# age(29 to 77) is less

# Answer for Question 1:
# Thus the chance of patient getting a HA 
# between the age(29 to 77) is more.


############## Question 2:
# Comparing the ratio of Gender having a chance to get the heart-attack
############## 
# H0 = Males have more chance to get a HA then female
# H1 = Males do not have more chance of getting a HA then female

# Convert the Sex variable to
# a categorical dichotomous variable with appropriate labels
# 0 = Female and 1 = Male
new_heart_data$Sex <- factor(new_heart_data$Sex, 
                             labels = c("FeMale", 
                                        "Male"))

# Structure of the DF
str(new_heart_data)

# Analyze the gender of the patients
table(new_heart_data$Sex)

# Analyzing the chances of heart-attack with the sex-gender
table(new_heart_data$Target, new_heart_data$Sex)

# Plot the graph to analyze the specified attributes
plot(Target, Sex, pch = 9, col = "LightBlue", 
     main = "Comaprison of Gender with Target", 
     xlab = "Target", ylab = "Gender")

# Visualizing the variables
histogram(~Sex | Target, 
          data = new_heart_data, 
          main = "Distribution of the gender with HA", 
          xlab = "SEX", ylab = "Count of people getting HA") 

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(Sex)

# Add the line to show if data is ND
qqline(Sex, col = "red")
# The gender(sex) field is not normally distributed

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(Sex, Target, median)

# Applying the chi-square statistic with the function
# it can be applied as both are the categorical variables
chisq <- chisq.test(new_heart_data$Sex, new_heart_data$Target)
chisq

# Observed count values for the hypothesis
chisq$observed

# Expected count of the values for the hypothesis
round(chisq$expected)

# Visualize the pearsons residuals
round(chisq$residuals)

# Print the p.value
chisq$p.value

# cut-off = 0.05
# p-value = 1.876e-06 equals to (0.0018)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the chance of males getting HA is less as compared to female

# Answer to Question 2:
# Thus the chance of male patient getting HA 
# is more then with female.


########### Question 3:
# Regular exercise of may reduced chest pain and 
# can have less chances of HA
########### 
# H0 = Exercise will reduced the chest pain
# H1 = Exercise will not reduced the chest pain

# Analyzing the count with different types of chest pain data
table(new_heart_data$Chest_pain)

# Analyzing the exercise data with the count
table(new_heart_data$Excercise_angina)

# Analyzing the combination of the exercise and chest pain
table(new_heart_data$Excercise_angina, new_heart_data$Chest_pain)

# Plot the graph to analyze the specified attributes
plot(Excercise_angina, Chest_pain, pch = 9, col = "LightBlue", 
     main = "Comparison of the exercise data v/s chest pain", 
     xlab = "Exercise status", ylab = "Chest Pain Type")

# Convert the Exercise_Angina variable to
# a categorical dichotomous variable with appropriate labels
# 0 = No and 1 = Yes
new_heart_data$Excercise_angina <- factor(new_heart_data$Excercise_angina, 
                                          labels = c("No", "Yes"))

# Structure of the DF
str(new_heart_data)

# Converting the Chest pain variable to a factor &
# categorizing it to different levels
new_heart_data$Chest_pain <- as.factor(new_heart_data$Chest_pain)

# Renaming the levels of the Chest Pain as
# Value 0 = Typical Angina
# Value 1 = Atypical Angina
# Value 2 = Non-Anginal Pain
# Value 3 = Asymptomatic
levels(new_heart_data$Chest_pain) = c("Typical Angina", 
                                      "Atypical Angina", 
                                      "Non-Anginal Pain", 
                                      "Asymptomatic")


# Structure of the DF
str(new_heart_data)

# Analyzing the data after converting it to desired attributes
table(new_heart_data$Chest_pain)
table(new_heart_data$Excercise_angina)

# Analyzing the combination of the exercise and chest pain
table(new_heart_data$Excercise_angina, new_heart_data$Chest_pain)

# Visualizing the variables
histogram(~Chest_pain | Excercise_angina, 
          data = new_heart_data, 
          main = "Distribution of the Exercise v/s Chest Pain", 
          xlab = "Chest Pain Type", ylab = "Daily Exercise Result") 

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(Chest_pain, Excercise_angina, median)

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(Chest_pain)

# Add the line to show if data is ND
qqline(Chest_pain, col = "red")
# The chest pain variable is not normally distributed

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(Excercise_angina)

# Add the line to show if data is ND
qqline(Excercise_angina, col = "red")
# It is not normally distributed

# Applying the chi-square statistic with the function
# it can be applied as both are the categorical variables
chisq <- chisq.test(new_heart_data$Chest_pain, new_heart_data$Excercise_angina)
chisq

# Observed count values for the hypothesis
chisq$observed

# Expected count of the values for the hypothesis
round(chisq$expected)

# Visualize the pearsons residuals
round(chisq$residuals)

# Print the p.value
chisq$p.value

# cut-off = 0.05
# p-value = 1.577331e-14 equals to (0.00000000015)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the daily exercise will not reduce the chest pain and
# higher the chance of HA

# Answer to Question 3:
# Thus the exercise will reduced the chest pain and
# less chance of HA

########### Question 4:
# Cholesterol level can block/damage the blood vessel & 
# the cholesterol levels are correlated with blood arteries
########### 
# H0 = Cholesterol level has an effect on blood arteries
# H1 = Cholesterol level has no effect on blood arteries

# Analyze the data with specified attributes
table(new_heart_data$Num_major_vessel)
table(new_heart_data$Cholestoral)

# Comparing the analysis of the specified attributes
table(new_heart_data$Num_major_vessel, new_heart_data$Cholestoral)

# Convert the Num_major_vessel variable to
# a categorical nominal variable with appropriate levels
new_heart_data$Num_major_vessel <- as.factor(new_heart_data$Num_major_vessel)

# Renaming the levels of the Chest Pain as
# 0 = No Blocks
# 1 = 1 Block
# 2 = 2 Block
# 3 = 3 Block
# 4 = 4 Block
levels(new_heart_data$Num_major_vessel) = c("No Blocks", 
                                            "1 Block", 
                                            "2 Blocks", 
                                            "3 Blocks", 
                                            "4 Blocks")

# Show the vessel levels
levels(new_heart_data$Num_major_vessel)

# Structure of the DF
str(new_heart_data)

# Plot the graph to analyze the specified attributes
plot(Cholestoral, Num_major_vessel, pch = 9, col = 'lightblue', 
     main = "Comparison of cholesterol level v/s arteries", 
     xlab = "Cholesterol level(mg/dl)", ylab = "Arteries Blocked")

# Visualizing the variables with the histogram
histogram(~Cholestoral | Num_major_vessel, 
          data = new_heart_data, 
          main = "Distribution of cholesterol v/s arteries", 
          xlab = "Cholesterol level(mg/dl)", ylab = "Arteries Blocked")

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is ND or not
qqnorm(Cholestoral)
# Add line that represent the ND
qqline(Cholestoral, col = "red")
# Assuming the data for Cholesterol is ND

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is ND or not
qqnorm(Num_major_vessel)

# Add line that represent the ND
qqline(Num_major_vessel, col = "red")
# The blood vessel data is not ND

# formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a ND
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_heart_data$Cholestoral)
normality_test$p.value
# p.value = 5.364848e-09 = 0.0000005
# Here p-values tells us the chance that the sample comes from ND
# We observed that p-value is < than 0.05, 
# The cholesterol var is not ND

# As we have one continuous and other categorical we will be using
# Kruskal-Wallis test for the better computation of the data
kruskal.test(Cholestoral ~ Num_major_vessel, data = new_heart_data)
# p-value = 0.04

# function pairwise.wilcox.test() to calculate pairwise comparisons between
# arteries levels
pairwise.wilcox.test(new_heart_data$Cholestoral, new_heart_data$Num_major_vessel, 
                     p.adjust.method = "BH")

# The pairwise comparison shows that the levels are significantly different

# p-value is = 0.04 which is less than the cut-off = 0.05
# Thus the result interprets that there is significant differences between
# the blood vessels

# p-value < 0.05 = Reject H0
# p-value < 0.05 so this indicates the 
# Null (H0) hypothesis is rejected
# therefore this indicates that
# Cholesterol level has no significance effect with the blood arteries

# Answer to Question 4:
# Thus the cholesterol level affects major arteries & 
# have significance difference between them

# Install package and import the library for "tidyverse"
# install.packages("tidyverse")
library(tidyverse)

# Install package and import the library for "ggpubr"
# install.packages("ggpubr")
library(ggpubr)

# Installing and importing library "gplots"
# install.packages("gplots")
library(gplots)

############### Question 5:
# Patients sugar level can cause the heart disease which can
# further lead to cause the heart attack
# Sugar and heart attack have significance correlation
###############
# (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
# H0 = Increase in sugar level can cause heart failure
# H1 = Increase in sugar level cannot led to heart failure

# Analyze the data with specified attributes
table(new_heart_data$Fasting_BS)
table(new_heart_data$Target)

# Comparing the analysis of the specified attributes
table(new_heart_data$Fasting_BS, new_heart_data$Target)

# Convert the Fasting_BS variable to
# a categorical dichotomous variable with appropriate labels
# (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
new_heart_data$Fasting_BS <- factor(new_heart_data$Fasting_BS, 
                                          labels = c("False", "True"))

# Structure of the DF after the conversion
str(new_heart_data)

# Visualizing the data
plot(Fasting_BS, Target, pch = 21, col = "LightBlue", 
     main = "Comparison of the Sugar level v/s Heart Attack", 
     xlab = "Sugar Level", ylab = "Heart Attack Status")


# Analysing the distribution of the variables
histogram(~Fasting_BS | Target, 
          data = new_heart_data, 
          main = "Distribution of Sugar Level v/s Heart Attack", 
          xlab = "Sugar Level Status(mg/dl)", ylab = "Count of people getting HA") 

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(Fasting_BS, Target, median)

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(Fasting_BS)

# Add the line to show if data is ND
qqline(Fasting_BS, col = "red")
# The fasting sugar variable is not normally distributed

# Applying the chi-square statistic with the function
# it can be applied as both are the categorical variables
chisq <- chisq.test(new_heart_data$Fasting_BS, new_heart_data$Target)
chisq
# p-value = 0.74

# Observed count values for the hypothesis
chisq$observed

# Expected count of the values for the hypothesis
round(chisq$expected)

# Visualize the pearsons residuals
round(chisq$residuals)

# Print the p.value
chisq$p.value

# cut-off = 0.05
# p-value = 0.74
# p-value > 0.05 then we, Accept H0

# p-value > 0.05 so this indicates that the
# Null (H0) hypothesis is accepted
# therefore this indicates that
# the increase in sugar level can cause the chances of heart attack

# Answer to Question 5:
# Thus the chance of getting heart attack is less 
# if the sugar level is not increased

################# Question 6:
# Blood pressure level can also cause the heart disease which may
# lead to heart attack
#################
# normal range of BP = 120
# abnormal range of BP > 120
# H0 = Rise in blood pressure can cause HA
# H1 = Rise in blood pressure cannot cause HA

# Analyze the data with specified attributes
table(new_heart_data$Resting_BP)
table(new_heart_data$Target)

# Comparing the analysis of the specified attributes
table(new_heart_data$Resting_BP, new_heart_data$Target)

# Plot the graph to analyze the specified attributes
plot(Target, Resting_BP, pch = 9, col = "LightBlue", 
     main = "Comparison of the pressure v/s Heart Attack", 
     xlab = "Blood Pressure", ylab = "Heart Attack Status")

# Analyzing the distribution of the variables
histogram(~Resting_BP | Target, 
          data = new_heart_data, 
          main = "Distribution of Blood Pressure v/s Heart Attack", 
          xlab = "Blood Pressure(mm Hg)", ylab = "Count of people getting HA")

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is ND or not
qqnorm(Resting_BP)

# Add line that represent the ND
qqline(Resting_BP, col = "red")
# Assume it is ND

# Comparing the two variables
with(new_heart_data, 
     qqplot(Resting_BP[Target == "More chance of getting HA"], 
            Resting_BP[Target == "Less chance of getting HA"], 
            main = "Comparing 2 samples of Heart Data", 
            xlab = "High Pressure Target = More chance of HA", 
            ylab = "Low Pressure Target = Less chance of HA"))

# Formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a ND
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_heart_data$Resting_BP)
normality_test$p.value
# p.value =1.45e-06 = 0.00001
# It is not ND

# This test does not work on a dichotomous variable
with(new_heart_data, tapply(Resting_BP, Target, shapiro.test))

# Results show
# Less chance of HA with low pressure = p-value = 0.000008 - it is not ND
# More chance of HA with high pressure = p-value = 0.011 - it is not ND

# After consulting the chart, I am aiming
# for a dependent var(Blood Pressure)
# with a independent categorical var(Target)
# Format for the test is: wilcox.test(dependent var ~ independent var)
wilcox.test(Resting_BP~Target)
# p-value = 0.03

# cut-off = 0.05
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the rise in blood pressure cannot cause HA

# Answer for Question 6:
# Thus higher the blood pressure high are the chances of getting HA
# Hence rise is blood pressure can cause HA

########### Question 7:
# Blood pressure levels will cause the changes with respect to
# heart rate in patients
# Do blood pressure have an affect on heart rate
###########

#H0: Does blood pressure affects heart rate
#H1: Does blood pressure not affects heart rate

# Plot the graph to analyze the specified attributes
plot(Resting_BP, Max_heartrate, pch = 19, col ="lightblue", 
     main = "Comaprison of blood pressure with heart rate", 
     xlab = "Blood Pressure (mm Hg)", ylab = "Heart Rate")


# Visualizing the variables separately using hist function
# for blood pressure
hist(Resting_BP, col = "red", main = "Distributation of blood pressure", 
     xlab = "Blood Pressure (mm Hg)")
# for heart rate
hist(Max_heartrate, col = "red", main = "Distribution of heart rate", 
     xlab = "Heart Rate")

# Visual analysis of the data
histogram(~Resting_BP | Max_heartrate,
          data = cars,
          main = "Distributation of blood pressure v/s heart rate",
          xlab = "Blood Pressure (mm Hg)" ,ylab = "Heart Rate")

# Quantile-quantile plot (Q-Q-Plot) allows us to check
# if the data is ND or not
# Adding the line that represent the ND of data using
# function qqplot() & qqline()
with(new_heart_data,
     {qqnorm(Resting_BP, 
             main ="Normal Q-Q-Plot of blood pressure", 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles")
             qqline(Resting_BP)
     })

with(cars,
     {qqnorm(Max_heartrate, 
             main ="Normal Q-Q-Plot of heart rate", 
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles")
             qqline(Max_heartrate)
     })

# Examine the linear correlation between both vars
with(new_heart_data, qqplot(Resting_BP, Max_heartrate))

# Testing the linearity of the variables
# We can run the formal test of normality provided through 
# the widely used shapiro-wilks test
normality_test <- shapiro.test(new_heart_data$Resting_BP)
normality_test$p.value
# For this variable p-value = 0.000014

normality_test <- shapiro.test(new_heart_data$Max_heartrate)
normality_test$p.value
# For this variable p-value = 0.000014

# p-values tells us the chance that the sample comes from ND
# If p-value < 0.05 then the variable is not ND
# If p-value < 0.05 then the variable is ND
shapiro.test(new_heart_data$Resting_BP)
# 0.00014 < 0.05
# Then resting_bs is not ND
shapiro.test(new_heart_data$Max_heartrate)
# 0.00066 < 0.05
# Then Max_Heart_Rate is not ND

# If both the variables are not ND 
# will use "spearman" correlation method test
# dependent var = Heart Rate
# independent var = Resting Blood Pressure
cor.test(Resting_BP, Max_heartrate, 
         method = "spearman")
# Spearman correlation = -0.04
# p-value = 0.48
# cut-off = 0.05
# Thus p-value > 0.05
# we will Accept (H0)
# Thus there is no significant correlation between
# blood pressure and heart rate

# Answer to question 7:
# Thus we can state that blood pressure does not affects heart rate

# Saving the modified file of the data worked on
write.csv(new_heart_data, file = "new_heart_data.csv")

