# Simple Linear Regression

# Importing the dataset
dataset <- read.csv("Salary_Data.csv")

# Spliting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
trainingSet <- subset(dataset, split == TRUE)
testSet <- subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training Set
regressor <- lm(formula = Salary ~ YearsExperience,
                data = trainingSet)
