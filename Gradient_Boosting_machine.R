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
RN_train<-RN[train_RN,]
RN_test<-RN[-train_RN,]
  
set.seed(1)
assignment <- sample(1:3, size = nrow(CTO), prob = c(0.7,0.15,0.15), replace = TRUE)
# Create a train, validation and tests from the original data frame 
CTO_train <- CTO[assignment == 1, ]    # subset the grade data frame to training indices only
CTO_valid <- CTO[assignment == 2, ]  # subset the grade data frame to validation indices only
CTO_test <- CTO[assignment == 3, ]   # subset the grade data frame to test indices only 

install.packages("gbm")
library(gbm)

# Train a 10000-tree GBM model
set.seed(1)

RN_model <- gbm(formula = USA ~ ., 
                    data = RN_train,
                    n.trees = 10000)

# Print the model object                    
print(RN_model)

# summary() prints variable importance
summary(RN_model)


CTO_model <- gbm(formula = USA ~ ., 
                data = TrainCTO,
                n.trees = 10000)

# Print the model object                    
print(CTO_model)

# summary() prints variable importance
summary(CTO_model)

# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = CTO_model, 
                          method = "OOB", 
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)
CTO_model_cv <- gbm(formula = USA ~ ., 
                       data = TrainCTO,
                       n.trees = 10000,
                       cv.folds = 2)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = CTO_model_cv, 
                         method = "cv")

# Compare the estimates                         
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_oob number of trees
preds1 <- predict(object = CTO_model, 
                  newdata = TestCTO,
                  n.trees = ntree_opt_oob, n.ahead=250)

# Generate predictions on the test set using ntree_opt_cv number of trees
preds2 <- predict(object = CTO_model, 
                  newdata = TestCTO,
                  n.trees = ntree_opt_cv,
                  n.ahead=250)   

print(preds1)
plot(preds1, type="l")
plot(preds2, type="l")


#install.packages("AUC")
#library(AUC)
## Generate the test set AUCs using the two sets of preditions & compare
#auc1 <- auc(TestCTO$USA ,preds1)  #OOB
#auc2 <- auc(TestCTO$USA, preds2)  #CV 

# Compare AUC 
#print(paste0("Test set AUC (OOB): ", auc1))                         
#print(paste0("Test set AUC (CV): ", auc2))