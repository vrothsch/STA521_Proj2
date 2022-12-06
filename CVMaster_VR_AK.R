#### CVMaster File with CV Function
#### Viola Rothschild and Andrew Kenealy

library(caret)
library(dplyr)
library(class)
library(combinat)

###### CVMASTER FUNCTION

#Things to input:
#  classify - classifier model
#  features - classification features and labels
#  data - input dataframe
#  K - number of cross-validation folds
#  loss - loss metric calculated based on classifier predictions and labels
#  fold_f - determines which portion of data belongs to each cross-validation fold

#Things to output:
#  list of metrics

CVmaster <- function(classify, features, data, K, loss_metric, fold_f=random_folds) {
  folds <- fold_f(data, K)
  
  validate_error <- rep(NA, K)
  acc <- rep(NA, K)
  i = 1
  for (f in folds) {
    predicts = classify(features, data[-f,], data[f,])
    validate_error[i] = loss_metric(predicts, data[f,]$exp_label)
    i = i + 1
  }
  return(list(metric=validate_error))
}


## Loss function for accuracy
acc <- function(predicts, labels) {
  return (sum(predicts == labels) / length(predicts))
}



