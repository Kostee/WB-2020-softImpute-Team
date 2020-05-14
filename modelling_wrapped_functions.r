library(rpart)
train_and_predict_fun_rpart <- function(train, test, name_of_target){
  vars <- colnames(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  tree_model <- rpart(?ormula = my_formula, data = train,
                      method = "class", control = rpart.control(cp = 0))
  y_pred <- as.data.frame(predict(tree_model, test, type = "class"))
  return(y_pred[,1])
}


library(class) # nie dzia�a dla faktor�w, mo�e si� nap?awi je�li b�dziemy chcie� knn
train_and_predict_fun_knn <- function(train, test, name_of_target){
  train[[name_of_target]] <- as.factor(train[[name_of_target]])
  test[[name_of_target]] <- as.factor(test[[name_of_target]])
  col_id <- -which(names(train) ?in% c(name_of_target))
  y_pred <- knn(train[,col_id], test[,col_id], train[[name_of_target]])
  return(y_pred)
}

library(e1071)
train_and_predict_fun_bayes <- function(train, test, name_of_target){
  train[[name_of_target]] <- as.factor(train[[name_of_ta?get]])
  test[[name_of_target]] <- as.factor(test[[name_of_target]])
  vars <- colnames(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  NBclassfier=naiveBayes(my_formula, d?ta=train)
  y_pred=predict(NBclassfier, newdata=test, type="class")
  return(y_pred)
}

library(ranger)
train_and_predict_fun_ranger <- function(train, test, name_of_target){
  train[[name_of_target]] <- as.factor(train[[name_of_target]])
  test[[name_of_t?rget]] <- as.factor(test[[name_of_target]])
  vars <- colnames(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  rg <- ranger(my_formula, data = train)
  y_pred <- predict(rg? data = test)$predictions
  return(y_pred)
}

train_and_predict_fun_svm <- function(train, test, name_of_target){
  train[[name_of_target]] <- as.factor(train[[name_of_target]])
  test[[name_of_target]] <- as.factor(test[[name_of_target]])
  vars <- colnam?s(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  model <- svm(my_formula, data = train)
  y_pred <- predict(model, test)
  return(y_pred)
}

#TEST:
#train <- read.csv("tra?n.csv")
#test <- read.csv("test.csv")
#train_and_predict_fun_rpart(train, test, "is_good_customer_type")
#train_and_predict_fun_knn(train, test, "is_good_customer_type")
#train_and_predict_fun_bayes(train, test, "is_good_customer_type")
#train_and_predict_?un_ranger(train, test, "is_good_customer_type")
#train_and_predict_fun_svm(train, test, "is_good_customer_type")