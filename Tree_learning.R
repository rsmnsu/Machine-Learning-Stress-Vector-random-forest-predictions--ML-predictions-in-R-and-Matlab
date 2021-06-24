rm (list = ls(all=TRUE))
graphics.off()
install.packages("readxl")
#library(readxl)
CTO <- read_excel("C:/Users/Mihan/Dropbox/DataCamp/R/Machine Learning in R/CTO.xlsx")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("Matrix")
library(rpart)
library(rpart.plot)
library(Matrix)

n<-nrow(CTO)
n_train<-round(0.80*n)
set.seed(123)
train_indices<-sample(1:n, n_train)
TrainCTO<-CTO[train_indices,]
TestCTO<-CTO[-train_indices,]
ModelCTO<-rpart(formula= USA~.,data=TrainCTO, method="class")
class_prediction<- predict(object= ModelCTO, newdata= TestCTO, type="class")
confusionMatrix(data = class_prediction,       
                reference = TestCTO$USA)
ModelCTO1 <- rpart(formula = USA ~ ., 
                       data = TrainCTO, 
                       method = "class",
                       parms = list(split = "gini"))
pred1 <- predict(object = ModelCTO1, 
                 newdata = TestCTO,
                 type = "class")    
set.seed(1)
assignment <- sample(1:3, size = nrow(CTO), prob = c(0.7,0.15,0.15), replace = TRUE)
# Create a train, validation and tests from the original data frame 
CTO_train <- CTO[assignment == 1, ]    # subset the grade data frame to training indices only
CTO_valid <- CTO[assignment == 2, ]  # subset the grade data frame to validation indices only
CTO_test <- CTO[assignment == 3, ]   # subset the grade data frame to test indices only 
# Train the model
CTO_model <- rpart(formula = USA ~ ., 
                     data = CTO_train, 
                     method = "anova")

# Look at the model output                      
print(CTO_model)


# Plot the tree model
rpart.plot(x = CTO_model, yesno = 2, type = 0, extra = 0)
# Generate predictions on a test set
pred <- predict(object = CTO_model,   # model object 
                    newdata = CTO_test)  # test dataset
 # Compute the RMSE
#install.packages("Metrics")
library(Metrics)
rmse(actual = CTO_test$USA, 
         predicted = pred)
#install.packages("caret", dependencies=c("Depends", "Imports",
#"LinkingTo", "Suggests", "Enhances"))

# Plot the "CP Table"
plotcp(CTO_model)

# Print the "CP Table"
print(CTO_model$cptable)

# Retreive optimal cp value based on cross-validated error
opt_index <- which.min(CTO_model$cptable[, "xerror"])
cp_opt <- CTO_model$cptable[opt_index, "CP"]

# Prune the model (to optimized cp value)
CTO_model_opt <- prune(tree = CTO_model, 
                             cp = cp_opt)
# Plot the optimized model
rpart.plot(x = CTO_model_opt, yesno = 2, type = 0, extra = 0)


#grid search
# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 4, 1)
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)

# Number of potential models in the grid
num_models <- nrow(hyper_grid)

# Create an empty list to store models
CTO_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models){
  
  # Get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # Train a model and store in the list
 CTO_models[[i]] <- rpart(formula = USA ~ ., 
                             data = CTO_train, 
                             method = "anova",
                             minsplit = minsplit,
                             maxdepth = maxdepth)

}
# Number of potential models in the grid
num_models <- length(CTO_models)

# Create an empty vector to store RMSE values
rmse_values <- c()

# Write a loop over the models to compute validation RMSE
for (i in 1:num_models) {
  
  # Retreive the i^th model from the list
  model <- CTO_models[[i]]
  
  # Generate predictions on grade_valid 
  pred <- predict(object = model,
                  newdata = CTO_valid)
  
  # Compute validation RMSE and add to the 
  rmse_values[i] <- rmse(actual = CTO_valid$USA, 
                         predicted = pred)
}

# Identify the model with smallest validation set RMSE
best_model <- CTO_models[[which.min(rmse_values)]]

# Print the model paramters of the best model
best_model$control

# Compute test set RMSE on best_model
pred <- predict(object = best_model,
                newdata = CTO_test)
rmse(actual = CTO_test$USA, 
     predicted = pred)

#install.packages("ipred")
library(ipred)
library(Metrics)
# Bagging is a randomized model, so let's set a seed (123) for reproducibility
set.seed(123)

# Train a bagged model
CTO_model1 <- bagging(formula = USA ~ ., 
                        data = CTO_train,
                        coob = TRUE)

# Print the model
print(CTO_model1)

# Generate predicted classes using the model object
class_prediction <- predict(object = CTO_model1,    
                            newdata = CTO_test,  
                            type = "class")  # return classification labels

# Print the predicted classes
print(class_prediction)

# Calculate the confusion matrix for the test set
confusionMatrix(data = anova_prediction,       
                reference = CTO_test$USA) 


# Generate predictions on the test set
pred <- predict(object = CTO_model1,
                newdata = CTO_test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)

# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
auc(actual = CTO_test$USA, 
    predicted = pred)  
