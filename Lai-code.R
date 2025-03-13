#Presentation link:https://youtu.be/JAAYWde9SGw

##################################
#Question 1 - Understand the Data
##################################

data <- read.table("RedWine.txt")
data

the.data <- as.matrix(read.table("RedWine.txt"))

set.seed(223330692) # using your student ID number for reproducible sampling with the seed function

sample.data <- the.data[sample(1:1599, 500), c(1:6)]

# Set the column names for the sample data
colnames(sample.data) <- c("Citric Acid", "Chlorides", "Total Sulfur Dioxide", "pH", "Alcohol", "Quality")

#hhjhjhjkkhkh
# Create scatter plots for each predictor variable against the variable of interest (Quality)
par(mfrow = c(1, 1))

# Assuming Citric Acid is in column 1 and Quality is in column 6
plot(sample.data[, 1], sample.data[, 6], 
     xlab = "Citric Acid", ylab = "Quality (Y)", 
     main = "Scatter Plot of Citric Acid vs Quality", 
     pch=16, col="blue")

# Optionally add grid lines
grid()

# Optionally add a regression line
abline(lm(sample.data[, 6] ~ sample.data[, 1]), col="red")


# Scatter plot for Chlorides vs Quality
plot(sample.data[, 2], sample.data[, 6], 
     xlab = "Chlorides", ylab = "Quality (Y)", 
     main = "Scatter Plot of Chlorides vs Quality", pch=16)
# Optionally add grid lines
grid()

# Optionally add a regression line
abline(lm(sample.data[, 6] ~ sample.data[, 2]), col="red")

# Scatter plot for Total Sulfur Dioxide vs Quality
plot(sample.data[, 3], sample.data[, 6], 
     xlab = "Total Sulfur Dioxide", ylab = "Quality (Y)", 
     main = "Scatter Plot of Total Sulfur Dioxide vs Quality", pch=16)
# Optionally add grid lines
grid()

# Optionally add a regression line
abline(lm(sample.data[, 6] ~ sample.data[, 3]), col="red")     

# Scatter plot for pH vs Quality
plot(sample.data[, 4], sample.data[, 6], 
     xlab = "pH", ylab = "Quality (Y)", 
     main = "Scatter Plot of pH vs Quality", pch=16)
# Optionally add grid lines
grid()

# Optionally add a regression line
abline(lm(sample.data[, 6] ~ sample.data[, 4]), col="red")

# Scatter plot for Alcohol vs Quality
plot(sample.data[, 5], sample.data[, 6], 
     xlab = "Alcohol", ylab = "Quality (Y)", 
     main = "Scatter Plot of Alcohol vs Quality", pch=16)
# Optionally add grid lines
grid()

# Optionally add a regression line
abline(lm(sample.data[, 6] ~ sample.data[, 5]), col="red")
     
# Reset the plotting area to default
par(mfrow = c(1, 1))

# Create 6 histograms for each X variable and Y
par(lwd = 3)
# Histogram for Citric Acid
hist(sample.data[,1], 
     main = "Histogram of Citric Acid", 
     xlab = "Citric Acid", 
     col = "red", border = "black", lwd = 3 ,cex.axis = 1.5,  # This increases the size of the axis labels
     cex.lab = 1.5)

# Histogram for Chlorides
hist(sample.data[,2], 
     main = "Histogram of Chlorides", 
     xlab = "Chlorides", 
     col = "red", border = "black", lwd = 3, cex.axis = 1.5,  # This increases the size of the axis labels
     cex.lab = 1.5)

# Histogram for Total Sulfur Dioxide
hist(sample.data[,3], 
     main = "Histogram of Total Sulfur Dioxide", 
     xlab = "Total Sulfur Dioxide", 
     col = "lightpink", border = "black", lwd = 3, cex.axis = 1.5,  # This increases the size of the axis labels
     cex.lab = 1.5)


# Histogram for pH
hist(sample.data[,4], 
     main = "Histogram of pH", 
     xlab = "pH", 
     col = "lightpink", border = "black", lwd = 3, cex.axis = 1.5,  # This increases the size of the axis labels
     cex.lab = 1.5)


# Histogram for Alcohol
hist(sample.data[,5], 
     main = "Histogram of Alcohol", 
     xlab = "Alcohol", 
     col = "deeppink", border = "black", lwd = 3, cex.axis = 1.5,  # This increases the size of the axis labels
     cex.lab = 1.5)

# Histogram for Quality
hist(sample.data[,6], 
     main = "Histogram of Quality", 
     xlab = "Quality", 
     col = "deeppink", border = "black", lwd = 3, cex.axis = 1.5,  # This increases the size of the axis labels
     cex.lab = 1.5)

par(mfrow = c(1, 1))


#based on the scatter plot, it is very messy and a lot of data is right skew & not Normally distributed in histogram. this may cause by outlier
# and lets check on outlier. based on the box plot, we identify some outlier hence we will proceed to remove outlier next

# Function to identify outliers
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Apply the function to each column
outliers <- apply(sample.data, 2, identify_outliers)

# Count the number of outliers in each column
outlier_counts <- colSums(outliers)
print(outlier_counts)



# Define a vector of colors, one for each boxplot
colors <- c("blue", "green", "brown", "purple", "red", "grey")
par(lwd = 1) 
par(mfrow = c(2, 3))
for(i in 1:6) {
  boxplot(sample.data[,i], 
          main=paste(colnames(sample.data)[i], "\n", outlier_counts[i], "outliers"),
          ylab="Value", 
          col=colors[i], 
          border="black",
          outline=TRUE)  # This will show the outliers as individual points
}
par(mfrow = c(1, 1))

# Create a new dataset without outliers, all the data with outlier is remove to ensure consistency
sample_data_clean <- sample.data[!rowSums(outliers), ]

# After removing outlier the dimension of new dataset is n=421, indicate that 15.8% of data is remove which is still acceptable
print(dim(sample_data_clean))

#Histogram after outlier is remove
#the histograms show significant improvements especially for chloride,pH, total sulfur dioxide, though some skewness remains in Citric Acid and Alcohol while 
#Quality now has a clearer bimodal pattern, peaking at levels 5 and 6."

par(lwd = 2) 
par(mfrow = c(2, 6))
for(i in 1:6) {
  # Original data
  hist(sample.data[,i], 
       main=paste("Original", colnames(sample.data)[i]),
       xlab="Value", 
       col=colors[i], 
       border="black")
  
  # Clean data
  hist(sample_data_clean[,i], 
       main=paste("Clean", colnames(sample.data)[i]),
       xlab="Value", 
       col=colors[i], 
       border="black")
}

#Removing outliers results in tighter clustering in scatter plots.Relationships between variables and quality become more apparent, though still weak.
#Scatter plots show weak positive correlations for Citric Acid and Alcohol with quality, and weak negative correlations for Chloride and Total Sulfur Dioxide.

par(mfrow = c(2, 5))

# Plot scatter plots
for(i in 1:5) {  # We only need 5 scatter plots (X variables vs Quality)
  # Original data scatter plot
  plot(sample.data[,i], sample.data[,6], pch=16,
       main=paste("Original", colnames(sample.data)[i], "vs Quality"),
       xlab=colnames(sample.data)[i], 
       ylab="Quality")
  
  # Clean data scatter plot
  plot(sample_data_clean[,i], sample_data_clean[,6], pch=16, 
       main=paste("Clean", colnames(sample.data)[i], "vs Quality"),
       xlab=colnames(sample.data)[i], 
       ylab="Quality")
}

# Reset the plotting area to default
par(mfrow = c(1, 1))

install.packages("moments")  # Install the moments package
library(moments)  # Load the moments package

# Calculate skewness for each variable
for(i in 1:ncol((sample_data_clean))) {
  print(paste("Skewness of", colnames(sample_data_clean)[i], ":", 
              skewness(sample_data_clean[,i])))
}

#Total Sulfur Dioxide is not chosen for question2 due to extremely high skewness & negative correlation

# Calculate skewness for each variable after outlier is remove
for(i in 1:5) {
  print(paste("Correlation of", colnames(sample_data_clean)[i], "with Quality:",
              cor(sample_data_clean[,i], sample_data_clean[,6])))
}
################################
#Question 2 - Transform the Data
################################

# Choose any four X variables and Y
I <- c(1, 2, 4, 5, 6)  # Adjust indices if needed

# Obtain a 400 by 5 matrix
data.transformed <- sample_data_clean[, I]

# Finding the correlations
cor(data.transformed[, 1], data.transformed[, 5])
cor(data.transformed[, 2], data.transformed[, 5])  # WEAK Negative coefficient -0.17 THUS NEGOTIATION NOT PERFORM
cor(data.transformed[, 3], data.transformed[, 5])  # WEAK Negative coefficient -0.10 THUS NEGOTIATION NOT PERFORM
cor(data.transformed[, 4], data.transformed[, 5])

summary(data.transformed)

#negation transformation is not recommended for chlorides and pH
#the weak correlations and lack of clear negative trends in the visualizations indicate that a negation 
#transformation would not improve the relationships between pH & chlorides variables and wine quality. 

skewness <- function(x) {
  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
}

minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

unit.z <- function(x){
  0.15*((x-mean(x))/sd(x)) + 0.5
}

# New function for power transformation
power_transform <- function(x, p) {
  sign(x) * abs(x)^p
}

#Skewness > 0 indicates right skew (tail on right side)
#Skewness < 0 indicates left skew (tail on left side)
#Skewness ≈ 0 indicates roughly symmetric distribution


# Calculate skewness for each variable
for(i in 1:ncol(data.transformed)) {
  print(paste("Skewness of", colnames(data.transformed)[i], ":", 
              skewness(data.transformed[,i])))
}

#Based on the result, we can see that most of our variable are right skew, where alcohol
#has the most right skewed followed by citric acid,chlorides, pH. Hence, transformation is
#applied to decrease skewness.

data_before_transform <- data.transformed

# Apply transformations to reduce skewness, goal is to minimize skewness close to 0

#since skewness > 0.3 = moderate skewed thus moderate transformation is applied
data.transformed[,1] <- data.transformed[,1]^(0.75)

#since skewness < 0.3 = nearly normal distributed thus weak transformation is applied
data.transformed[,2] <- sqrt(data.transformed[,2])
data.transformed[,3] <- sqrt(data.transformed[,3]) 

#since skewness > 0.5 = highly skewed thus reciprocal (Strongest transformation is applied)
data.transformed[,4] <- 1/(data.transformed[,4])^3

z <- data.transformed

#correlation after perform transformation
for(i in 1:4) {
  print(paste("Correlation of", colnames(data.transformed)[i], "with Quality:",
              cor(data.transformed[,i], data.transformed[,5])))
}

#negotiation transformation
data.transformed[,4] = max(data.transformed[,4]) + min(data.transformed[,4]) - data.transformed[,4]

z1 <- data.transformed

# Recalculate skewness
for(i in 1:ncol(data.transformed)) {
  print(paste("New skewness of", colnames(data.transformed)[i], ":", 
              skewness(data.transformed[,i])))
}

# Normalize transformed variables
for(i in 1:5) {
  data.transformed[,i] <- minmax(data.transformed[,i])
}

z2 <- data.transformed

# Check correlations with Quality after transformation is perform
for(i in 1:4) {
  print(paste("Correlation of", colnames(data.transformed)[i], "with Quality:",
              cor(data.transformed[,i], data.transformed[,5])))
}

#no use z score normalization, because not all data is N.D (skewness = 0, kurtosis = 3)
library(moments)
for(i in 1:ncol(data.transformed)) {
  skew_val <- skewness(data.transformed[,i])
  kurt_val <- kurtosis(data.transformed[,i])
  print(paste("Skewness of", colnames(data.transformed)[i], ":", skew_val))
  print(paste("Kurtosis of", colnames(data.transformed)[i], ":", kurt_val))
}

# Create histograms for transformed data

par(mfrow = c(3,4))
for(i in 1:5) {
  # Clean data
  hist(data_before_transform[,i], 
       main=paste("Clean", colnames(data_before_transform)[i]),
       xlab="Value", 
       col=colors[i], 
       border="black")
  
  hist(data.transformed[,i], 
       main=paste("Transformed", colnames(data.transformed)[i]),
       xlab="Value", 
       col=colors[i], 
       border="black")
}

# Reset plotting area
par(mfrow=c(1,1))

# Create scatter plots of transformed variables against Quality
#The scatter plots following transformation indicate that the relationships between the variables and wine quality were preserved and slightly clarified

par(mfrow=c(2,4))
for (i in 1:4) {  # Assuming columns 1-4 are predictors and 5 is the target
  plot(data_before_transform[,i], data_before_transform[,5],
       xlab=colnames(data_before_transform)[i],
       ylab="Quality",col="black", pch=16, lwd = 3, font.axis = 2,
       main=paste("Scatter Plot:", colnames(data.transformed)[i], "vs Quality"))
  
  plot(data.transformed[,i], data.transformed[,5],
       xlab=colnames(data.transformed)[i],
       ylab="Quality",col="black", pch=16, lwd = 3, font.axis = 2,
       main=paste("Transformed scatter Plot:", colnames(data.transformed)[i], "vs Quality"))
}
par(mfrow=c(1,1))


# Save this transformed data to a text file
write.table(data.transformed, "KAHYAN-transformed.txt") 

##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("KAHYAN-transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(data.transformed_copy)

# Get weights for Power Mean p=0.5 with fit.QAM()
fit.QAM(data.transformed_copy,output.1="PMoutput1.txt",stats.1="PMstats1.txt", g=PM05,g.inv = invPM05)

# Get weights for Power Mean p=2 with fit.QAM()
fit.QAM(data.transformed_copy,output.1="QMoutput1.txt",stats.1="QMstats1.txt",g=QM,g.inv = invQM) 

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data.transformed_copy,"OWAoutput1.txt","OWAstats1.txt") # OWA# Get weights for Weighted Arithmetic Mean with fit.QAM() 

#QM MODEL IS CHOOSEN DUE TO LOW ERROR AND HIGH CORRELATION
#######################################
#Question 4 - Use Model for Prediction
#######################################

# New input data
new_input <- c(0.9, 0.65, 38, 2.53, 7.1)

# Choose the same four X variables as in Q2: citric acid, chlorides, pH, alcohol
new_input_for_transform <- new_input[c(1, 2, 4, 5)]

# Ensure new_table includes the correct columns
new_table <- data_before_transform

# Create new row by adding a placeholder for the quality variable (e.g., NA)
new_input_for_transform <- c(new_input_for_transform, NA)

# Bind the new input to the table
new_table_with_data <- rbind(new_table, new_input_for_transform)

# Check the new table with data to ensure the new row was added correctly
print(dim(new_table_with_data))

# transforming the four variables in the same way as in question 2 
# Transform the input (citric acid, chlorides, pH, alcohol)
new_table_with_data[,1] <- new_table_with_data[,1]^(0.75)  # citric acid
new_table_with_data[,2] <- sqrt(new_table_with_data[,2])  # chlorides
new_table_with_data[,3] <- sqrt(new_table_with_data[,3])  # pH
new_table_with_data[,4] <- 1/(new_table_with_data[,4])^3   # alcohol

# Apply negation transformation for alcohol
new_table_with_data[,4] <- max(new_table_with_data[,4]) + min(new_table_with_data[,4]) - new_table_with_data[,4]

# Normalize the transformed input using min and max from the transformed dataset
for(i in 1:5) {
  new_table_with_data[,i] <- minmax(new_table_with_data[,i])
}

# Extract the input for prediction
input_for_prediction <- new_table_with_data[422, 1:4]


## applying the transformed variables to the best model selected from Q3 for Y prediction
qm_weights <- c(0.176214723452287, 0.099782523886134, 0.0835052439443727, 0.640497508717206)

# Using the QAM function
normalized_prediction <- QAM(input_for_prediction, qm_weights, g=QM, g.inv=invQM)

# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
original_min <- min(data_before_transform[,5])  
original_max <- max(data_before_transform[,5])
normalized_prediction <- max(0, min(1, normalized_prediction))
original_scale_prediction <- normalized_prediction * (original_max - original_min) + original_min

# Round to get the final integer prediction
final_prediction <- round(original_scale_prediction)

print(paste("Predicted wine quality:", final_prediction))
