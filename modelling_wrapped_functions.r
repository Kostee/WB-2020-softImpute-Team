library(rpart)
train_and_predict_fun_rpart <- function(train, test, name_of_target){
  vars <- colnames(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  tree_model <- rpart(formula = my_formula, data = train,
                      method = "class", control = rpart.control(cp = 0))
  y_pred <- as.data.frame(predict(tree_model, test, type = "class"))
  return(y_pred[,1])
}


library(class)
train_and_predict_fun_knn <- function(train, test, name_of_target){
  col_id <- -which(names(train) %in% c(name_of_target))
  y_pred <- knn(train[,col_id,  with=FALSE], test[,col_id,  with=FALSE], get(name_of_target, train))
  return(y_pred)
}


library(e1071)
train_and_predict_fun_bayes <- function(train, test, name_of_target){
  vars <- colnames(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  NBclassfier=naiveBayes(my_formula, data=train)
  y_pred=predict(NBclassfier, newdata=test, type="class")
  return(y_pred)
}

train_and_predict_fun_svm <- function(train, test, name_of_target){
  model <- svm(Species ~ ., data = train)
  pred <- predict(model, test)
  return(y_pred)
}