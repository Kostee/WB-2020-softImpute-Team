#setwd("C:/Users/ckostern/Desktop/Studia/03 rok II semestr/WB/WB_Prezentacja_softImpute-master")
#getwd()
set.seed(123)

source('./imputations_wrapped_functions.r')
source('./metrics_functions.r')
source('./reading_datasets_functions.r')

library(rpart) # for classification tree

train_and_predict_fun_rpart <- function(train, test, name_of_target){
  vars <- colnames(train)[colnames(train)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  tree_model <- rpart(formula = my_formula, data = train,
                      method = "class", control = rpart.control(cp = 0))
  y_pred <- as.data.frame(predict(tree_model, test, type = "class"))
  return(y_pred)
}


get_result <- function(dataset_list, imputation_fun, modelling_fun){
  
  imputated_dataset <- dataset_list$dataset
  name_of_target <- dataset_list$target
  
  # train test split
  train_test <- train_test_split(imputated_dataset, 0.8)
  train <- as.data.table(train_test[1])
  test <- as.data.table(train_test[2])
  
  # modelling
  
  y_pred <- train_and_predict_fun_rpart(train, test, name_of_target)
  
  # calculating metrics
  confusion_matrix <- get_confusion_matrix(test[[name_of_target]], y_pred[,1])

  accuracy_v <- accuracy(confusion_matrix)
  precision_v <- precision(confusion_matrix)
  recall_v <- recall(confusion_matrix)
  f1_v <- f1(confusion_matrix)
  
  classification_report <- data.frame(accuracy_v, precision_v,
                                      recall_v, f1_v)
  colnames(classification_report) <- c("accuracy", "precision",
                                       "recall", "f1")
  
  dataset_list$dataset <- NULL
  # in future maybe return all dataset_list ?
  # for now stick with readability
  
  imp_method_name <- deparse(substitute(imputation_fun))
  
  return(list( dataset_id = dataset_list$id, 
               imp_method = imp_method_name,
               confusion_matrix = confusion_matrix,
               classification_report = classification_report,
               imputation_time = dataset_list$imp_time))
}

# example: get_result(data_all[[1]], imputation_remove_rows)



# reading datasets from OpenML's edits
data_all <- read_all_imputed_datasets()

df <- data_all[[1]]

get_result(df, train_and_predict_fun_rpart)
