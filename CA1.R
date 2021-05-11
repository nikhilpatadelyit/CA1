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

# Display the column-names of DF
colnames(heart_data)

# Displaying the summary
summary(heart_data)

# To check if any data present
incomplete_data <- heart_data[!complete.cases(heart_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)

# visualize the missing data
install.packages("VIM")
library(VIM)
missing_values <- aggr(heart_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)
# No missing data present in the DF

############ DO NOT RUN FROM HERE #################
# Changing the columns values to their corresponding values needed
# used function (ifelse)
heart_data$Sex <- ifelse((heart_data$Sex == 1), 'Male', 'Female')
heart_data$Excercise_angina <- ifelse((heart_data$Excercise_angina == 1), 'Yes', 'No')
heart_data$Fasting_BS <- ifelse((heart_data$Fasting_BS == 1), 'True', 'False')
heart_data$Target <- ifelse((heart_data$Target == 1), 'More chance of heart attack', 'Less chance of heart attack')
# Display the DF
head(heart_data)
############ DO NOT RUN UNTIL HERE ###############

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(heart_data))

# Fetching the required columns for processing &
# storing them to a new DF
new_heart_data <- heart_data[, c(1,2,3,4,5,6,7,8,9,12,14)]

# Display the structure of DF
str(new_heart_data)

# Installing the library 'psych'
install.packages("psych")
library(psych)

pairs.panels(new_heart_data, 
             smooth = FALSE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered
             factor = 2, # Jittering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals


# Question 1:
# Peoples at different ages can get a heart-diseases
# H0 = Chance of getting a heart-disease between the age(29 to 77)
# H1 = Chance of not getting a heart-disease between the age(29 to 77)

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

with(new_heart_data, 
     qqplot(Age[Target == "More chance of getting HA"], 
            Age[Target == "Less chance of getting HA"], 
            main = "Comparing 2 samples of Target Heart Data", 
            xlab = "Age Target = More chance", 
            ylab = "Age Target = Less chance"))

# formal test of normality
# Shapiro-Wilks Test
# p-value tells us the chances that the sample comes from a ND
# If p.value > 0.05 then it is normally distributed
normality_test <- shapiro.test(new_heart_data$Age)
normality_test$p.value
# p.value = 0.00579

# This test does not work on a dichotomous variable
with(new_heart_data, tapply(Age, Target, shapiro.test))

# Results show
# Less chance of getting HA = p-value = 0.002 - ND
# More chance of getting HA = p-value = 0.121 - ND

# After consulting the chart, I am aiming
# a dependent var(Age)
# with a independent categorical var(Target)
# Format wilcox.test(dependent var ~ independent var)
wilcox.test(Age~Target)
# cut-off = 0.05
# p-value < 3.439e-05 equals to (0.002)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the chance of patient getting HA between the age(29 to 77) is less

# Answer for Question 1:
# Thus the chance of patient getting a HA between the age(29 to 77) is more.



# Question 2:
# Comparing the ratio of Gender having a chance o get the heart-attack
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

# Quantile-Quantile plot (Q-Q-Plot) allows us to check
# if the data is normally distributed or not
qqnorm(Sex)

# Add the line to show if data is ND
qqline(Sex, col = "red")
# The gender(sex) field is not normally distributed

# Visualizing the variables
histogram(~Sex | Target, 
          data = new_heart_data, 
          main = "Distribution of the gender with HA", 
          xlab = "SEX", ylab = "Count of people getting HA") 

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
# p-value < 1.876e-06 equals to (0.022)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the chance of female patient getting HA is less compared to male

# Answer to Question 2:
# Thus the chance of male patient getting HA is more with th female.


# Question 3:
# Regular exercise of may reduced to the chest pain and can have less chances of HA
# H0 = Exercise will reduced the chest pain & less chance of HA
# H1 = Exercise will not reduced the chest pain & can get HA

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


# Analyzing the data after converting it to desired attributes
table(new_heart_data$Chest_pain)
table(new_heart_data$Excercise_angina)


# Visualizing the variables
histogram(~Chest_pain | Excercise_angina, 
          data = new_heart_data, 
          main = "Distribution of the Exercise v/s Chest Pain", 
          xlab = "Chest Pain Type", ylab = "Daily Exercise Result") 

# Visual analysis seems to indicate that the 
# data is Normally Distributed
# summarizing it below
tapply(Chest_pain, Excercise_angina, median)

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
# p-value < 1.577331e-14 equals to (0.0016)
# p-value < 0.05 then we, Reject the H0

# p-value < 0.05 so this indicates that the
# Null (H0) hypothesis is rejected
# therefore this indicates that
# the daily exercise is required to reduce the chest pain and lower the risk of HA

# Answer to Question 3:
# Thus the chance of getting a high risk of chest pain which may lead to HA
# is more if daily exercise is not concerned










