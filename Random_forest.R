rm (list = ls(all=TRUE))
graphics.off()
#install.packages("readxl")
library(readxl)
CTO <- read_excel("C:/Users/Mihan/Dropbox/DataCamp/R/Machine Learning in R/CTO.xlsx")
RN <- read_excel("C:/Users/Mihan/Dropbox/Mardi Meetings/Meeting 22/Testing/Important_codes/Possible_Studies/3rdProposal/RA1.xlsx")
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

n<-nrow(RN)
n_train<-round(0.80*n)
set.seed(123)
train_RN<-sample(1:n, n_train)
RN_Train<-RN[train_RN,]
RN_Test<-RN[-train_RN,]
  
set.seed(1)
assignment <- sample(1:3, size = nrow(CTO), prob = c(0.7,0.15,0.15), replace = TRUE)
# Create a train, validation and tests from the original data frame 
CTO_train <- CTO[assignment == 1, ]    # subset the grade data frame to training indices only
CTO_valid <- CTO[assignment == 2, ]  # subset the grade data frame to validation indices only
CTO_test <- CTO[assignment == 3, ]   # subset the grade data frame to test indices only 
#install.packages("randomForest")
library(randomForest)
# Train a Random Forest RN
set.seed(1)  # for reproducibility
RN_model <- randomForest(formula = USA ~ ., 
                             data = RN_Train)

# Print the model output                             
print(RN_model)

# Train a Random Forest CTO
set.seed(1)  # for reproducibility
CTO_model <- randomForest(formula = USA ~ ., 
                         data = CTO_train)

# Print the model output                             
print(CTO_model)

err<-RN_model$err.rate
head(err)
err1<-CTO_model$err.rate
head(err1)


# Execute the tuning process
set.seed(1)              
res <- tuneRF(x = subset(CTO_train, select = -USA),
              y = CTO_train$USA,
              ntreeTry = 500)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)


# Execute the tuning process
set.seed(1)              
res <- tuneRF(x = subset(RN_Train, select = -USA),
              y = RN_Train$USA,
              ntreeTry = 500)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

# If you just want to return the best RF model (rather than results)
# you can set `doBest = TRUE` in `tuneRF()` to return the best RF model
# instead of a set performance matrix.


# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(RN_Train) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(RN_Train) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = USA ~ ., 
                        data = RN_Train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])





