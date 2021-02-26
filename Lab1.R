# Data Preprocessing

# Importing the dataset
dataset = read.csv('/Users/rahulc/Documents/Machine learning/Machine Learning/[Tutsgalaxy.com] - Machine Learning A-ZG„¢ Hands-On Python & R In Data Science/2.  Data preproce/Data.csv')

# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)
dataset
