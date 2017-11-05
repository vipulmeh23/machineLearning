# Include Paths Setting the working directory
work_dir <- "~/Documents/ISTE-780/Labs/Lab-1/lab1/"
setwd(work_dir)

# Loading Data
college <- read.csv("/Users/vipulmehra/Documents/ISTE-780/Labs/Lab-1/lab1/College.csv")

# Summary of the dataset
summary(college)

# Trying fix and adding a rownames column with name of each university recorded
rownames(college)=college[,1]
fix(college)

# Eliminating first column from the dataset where names are stored
college=college[,-1]
fix(college)

# Getting numerical summary of the variables in dataset
summary(college)

# Getting scatterplot of the first 10 variables of the data
pairs(college[,1:10])

# Plotting box plot of Outstate vs Private
boxplot(college$Outstate ~ college$Private, col = c("Grey", "Red"), main = "Outstate versus Private", xlab = "Private", ylab = "Outstate")

# Creating qualitative variable namely Elite by binning Top10perc variable
Elite <-  rep("No",nrow(college))
Elite[college$Top10perc > 50] <-  "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
fix(college)

# Using the summary() function to see how many elite universities there are. 
# And plotting boxplot to produce side-by-side boxplots of Outstate versus Elite.

# Printing summary
summary(college$Elite)

# Printing the boxplot of Outstate versus Elite
boxplot(college$Outstate ~ college$Elite, col = c("Grey", "Red"), main = "Outstate versus Elite", xlab = "Elite", ylab = "Outstate")

# Printing the Histograms of few of the quantitative variables with different values of break or with different binning

# Dividing the window
par(mfcol = c(2,6))

# Quantitative Variables with 5 bins
hist(college$Accept, breaks = 5, freq = TRUE, col = "Grey", main = "Histogram Bins of 5", 
     xlab = "Accept", ylab = "Value")
hist(college$Enroll, breaks = 5, freq = TRUE, col = "Grey", main = "Histogram Bins of 5", 
     xlab = "Enroll", ylab = "Value")
hist(college$Top10perc, breaks = 5, freq = TRUE, col = "Grey", main = "Histogram Bins of 5", 
     xlab = "Top10perc", ylab = "Value")
hist(college$Top25perc, breaks = 5, freq = TRUE, col = "Grey", main = "Histogram Bins of 5", 
     xlab = "Top25perc", ylab = "Value")

# Quantitative Variables with 10 bins
hist(college$Accept, breaks = 10, freq = TRUE, col = "Red", main = "Histogram Bins of 10", 
     xlab = "Accept", ylab = "Value")
hist(college$Enroll, breaks = 10, freq = TRUE, col = "Red", main = "Histogram Bins of 10", 
     xlab = "Enroll", ylab = "Value")
hist(college$Top10perc, breaks = 10, freq = TRUE, col = "Red", main = "Histogram Bins of 10", 
     xlab = "Top10perc", ylab = "Value")
hist(college$Top25perc, breaks = 10, freq = TRUE, col = "Red", main = "Histogram Bins of 10", 
     xlab = "Top25perc", ylab = "Value")

# Quantitative Variables with 15 bins
hist(college$Accept, breaks = 15, freq = TRUE, col = "Black", border = "White", main = "Histogram Bins of 15", 
     xlab = "Accept", ylab = "Value")
hist(college$Enroll, breaks = 15, freq = TRUE, col = "Black", border = "White", main = "Histogram Bins of 15", 
     xlab = "Enroll", ylab = "Value")
hist(college$Top10perc, breaks = 15, freq = TRUE, col = "Black", border = "White", main = "Histogram Bins of 15", 
     xlab = "Top10perc", ylab = "Value")
hist(college$Top25perc, breaks = 15, freq = TRUE, col = "Black", border = "White", main = "Histogram Bins of 15", 
     xlab = "Top25perc", ylab = "Value")

# Summary to find qualitative (categorical) and quantitative predictors
summary(college)

# Finding Range, Mean and Standard deviation of Enroll

# Range of Enroll: Finds the min and max value and stores it in a numeric vector
range <- range(college$Enroll)
# We will take the difference of the max and the min value to get the range
diff(range)
# Find mean of Enroll
mean(college$Enroll)
# Find standard deviation of Enroll
sd(college$Enroll)

# Removing rows and finding Range, Mean, Standard deviation

# Removing 100th to 200th observations from the college dataframe and storing it as a college_new_df dataframe
college_new_df <- college[-(100:200), ]
# Range of Enroll: Finds the min and max value and stores it in a numeric vector
range_new_df <- range(college_new_df$Enroll)
# We will take the difference of the max and the min value to get the range
diff(range_new_df)
# Find mean of Enroll in the college_new_df dataframe
mean(college_new_df$Enroll)
# Find standard deviation of Enroll in the college_new_df dataframe
sd(college_new_df$Enroll)

# Alternate method without creating new dataframe

# #Removing 100th to 200th observations from the college dataframe and storing it as a college_new_df dataframe
# college[-(100:200), ]
# fix(college)
# #Range of Enroll: Finds the min and max value and stores it in a numeric vector
# range <- range(college$Enroll)
# #We will take the difference of the max and the min value to get the range
# diff(range)
# #Find mean of Enroll in the college_new_df dataframe
# mean(college$Enroll)
# #Find standard deviation of Enroll in the college_new_df dataframe
# sd(college$Enroll)

