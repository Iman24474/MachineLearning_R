# Importing data
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# Preparing data
wbcd <- wbcd[,-1] # Drop ids

# Factoring target feature
wbcd$diagnosis <- factor(wbcd$diagnosis, 
                         levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# Proportions
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# Transformation - Normalizing numeric data
normalize <- function(x)
{
  return((x - min(x)) / (max(x) - min(x)))
}

# Applying the normalize() function to the numeric features in our dataframe.
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# Creating training and test datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# Diagnosis labels for train and test 
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# Training a model on the data (KNN Algorithm)
library(class)
wbcd_test_pred <- knn(train = wbcd_train, 
         test = wbcd_test,
         cl = wbcd_train_labels,
         k = 21)

# Evaluating model performance
library(gmodels)
CrossTable(x = wbcd_test_labels,
           y = wbcd_test_pred,
           prop.chisq = FALSE)
