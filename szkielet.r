#setwd("C:/Users/ckostern/Desktop/Studia/03 rok II semestr/WB/WB_Prezentacja_softImpute-master")
#getwd()
set.seed(123)

source('./imputations_wrapped_functions.r')
source('./modelling_wrapped_functions.r')
source('./metrics_functions.r')
source('./reading_datasets_functions.r')

library(varhandle)

get_result <- function(dataset_list, modelling_fun){
  
  imputated_dataset <- dataset_list$dataset
  name_of_target <- dataset_list$target
  
  # train test split
  train_test <- train_test_split(imputated_dataset, 0.8)
  train <- as.data.table(train_test[1])
  test <- as.data.table(train_test[2])
  
  # modelling
  
  y_pred_raw <- modelling_fun(train, test, name_of_target)
  if( ( is.factor(y_pred_raw) & all(check.numeric(y_pred_raw)) | is.numeric(y_pred_raw) )) {
    y_pred_raw <- as.numeric(as.character(y_pred_raw))
    y_pred <- round(y_pred_raw)
  } else {
    y_pred <- y_pred_raw
  }
  
  # calculating metrics
  confusion_matrix <- get_confusion_matrix(test[[name_of_target]], y_pred)
  
  
  
  mcc <- mcc_wrap(confusion_matrix)
  
  accuracy_v <- accuracy(confusion_matrix)
  precision_v <- precision(confusion_matrix)
  recall_v <- recall(confusion_matrix)
  f1_v <- f1(confusion_matrix)
  
  classification_report <- data.frame(accuracy_v, precision_v,
                                      recall_v, f1_v, mcc)
  colnames(classification_report) <- c("accuracy", "precision",
                                       "recall", "f1", "mcc")
  
  dataset_list$dataset <- NULL
  # in future maybe return all dataset_list ?
  # for now stick with readability
  
  imp_method_name <- deparse(substitute(imputation_fun))
  modelling_fun_name <- deparse(substitute(modelling_fun))
  
  return(list( dataset_id = dataset_list$id, 
               imp_method = dataset_list$imp_method,
               mod_mehtod = modelling_fun_name,
               confusion_matrix = confusion_matrix,
               classification_report = classification_report,
               imputation_time = dataset_list$imp_time, 
               raw = y_pred_raw,
               true = test[[name_of_target]]))
}



# reading datasets from OpenML's edits
data_all <- read_all_imputed_datasets()

df <- data_all[[1]]

get_result(df, train_and_predict_fun_rpart) -> wynik
