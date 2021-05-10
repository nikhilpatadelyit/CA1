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

# Question 1:
# Peoples at different ages can get a heart-diseases
# H0 = Chance of getting a heart-disease between the age(29 to 77)
# H1 = Chance of not getting a heart-disease between the age(29 to 77)

# Convert the Target variable to
# a categorical dichotomous variable with appropriate labels
new_heart_data$Target <- factor(new_heart_data$Target, 
                                labels = c("Less chance of getting HA", 
                                           "More chance of getting HA"))

# Displaying the structure of DF
str(new_heart_data)

# Installing the library 'psych'
install.packages("psych")
library(psych)

pairs.panels(new_heart_data, 
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = TRUE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered
             factor = 2, # Jittering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals

# Attach the DF with the function
attach(new_heart_data)

# Plot the graph to analyze the specified attributes
plot(Target, Age, pch = 9, col = "LightBlue")

# We can split the dichotomous variable into 
# 2 different visualization & then examine the data
# Importing the library 'lattice'
library("lattice")

# Visualizing the variables
histogram(~Age | Target, 
          data = new_heart_data, 
          main = "Distribution of heart data", 
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


# We can add normality line to the plot
# to help evaluate normality
#with(new_heart_data, {
 # qqnorm(Age[Target == "More Chance of getting HA"], 
  #       main = "Heart Disease")
  #qqline(Age[Target == "More chance of getting HA"])})

#with(new_heart_data, {
 # qqnorm(Age[Target == "Less chance of getting HA"], 
  #       main = "No Heart Disease")
  #qqline(Age[Target == "Less chance of getting HA"])})

# Question 2:





