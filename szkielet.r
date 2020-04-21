#setwd("C:/Users/ckostern/Desktop/Studia/03 rok II semestr/WB/WB_Prezentacja_softImpute-master")
#getwd()
set.seed(123)

source('./imputations_wrapped_functions.r')
source('./metrics_functions.r')
source('./reading_datasets_functions.r')

library(rpart) # for classification tree
get_result <- function(dataset_list, imputation_fun){
  
  dataset <- dataset_list$dataset
  name_of_target <- dataset_list$target
  
  # imputation
  imputation_start = Sys.time() # start to measure time
  imputated_dataset <- imputation_fun(dataset) 
  imputation_stop = Sys.time() # end measuring time
  
  # train test split
  train_test <- train_test_split(imputated_dataset, 0.8)
  train <- as.data.table(train_test[1])
  test <- as.data.table(train_test[2])
  
  # modelling
  vars <- colnames(dataset)[colnames(dataset)!=name_of_target]
  my_formula <- as.formula(paste(name_of_target, paste(vars, collapse=" + "), sep=" ~ "))
  modelling_start = Sys.time() # start to measure time
  tree_model <- rpart(formula = my_formula, data = train,
                      method = "class", control = rpart.control(cp = 0))
  y_pred <- as.data.frame(predict(tree_model, test, type = "class"))
  modelling_stop = Sys.time() # end measuring time
  
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
               imputation_time = imputation_stop - imputation_start,
               modelling_time = modelling_stop - modelling_start))
}
# example: get_result(data_all[[1]], imputation_remove_rows)
# TODO: dokumentacja



# reading datasets from OpenML's edits
data_all <- read_all_datasets()

# imputations and targets preparation
imputations <- list(imputation_fun_vim, imputation_fun_missForest,
                    imputation_remove_rows, imputation_mode_median, imputation_fun_mice)

targets <- lapply(data_all, function(d){d$target})


#PARALLEL ROBI BRRRRR [TODO bo nie wiem jak]
library(batchtools)

# THIS WILL ERASE ALL PREVIOUS COMPUTATUONS
# # clearRegistry()

# this creates new registry
# # registry <- makeRegistry(file.dir = "./registry", seed = 15390)

loadRegistry(file.dir = './registry', writeable = TRUE)

makeClusterFunctionsMulticore()

data_all_male_i_bezproblemowe <- data_all[c(-1, -3, -4, -7)]

# careful to run once, will result in 40 jobs in getJobTable()
#batchMap(fun = get_result, dataset = data_all, imputation_fun = rep(imputations, length(data_all)))


# pojedynczo dla kazdej imputacji, zeby mniej sie wywalalo rstudio
batchMap(fun = get_result, dataset = data_all_male_i_bezproblemowe, imputation_fun = imputations[2])
submitJobs(resources = list(walltime = 60))

batchMap(fun = get_result, dataset = data_all_male_i_bezproblemowe, imputation_fun = imputations[3])
submitJobs(resources = list(walltime = 60))

batchMap(fun = get_result, dataset = data_all_male_i_bezproblemowe, imputation_fun = imputations[4])
submitJobs(resources = list(walltime = 60))

batchMap(fun = get_result, dataset = data_all_male_i_bezproblemowe, imputation_fun = imputations[5])
submitJobs(resources = list(walltime = 60))

getJobTable()

#clearRegistry()
# resources = list(walltime = 3600, memory = 1024)
# see more at resources at ?submitJobs
submitJobs(resources = list(walltime = 60))

getJobTable()

waitForJobs()


# ...
# ...





# every imputation on each dataset
for (imputation in imputations){
  for(i in 1:8){
    results <- get_result(data_all[[i]], imputation)
    print(results$id)
    print(results$imp_method)
    print(results$confusion_matrix) # confusion_matrix
    print(results$classification_report) # classification_report
    print(results$imputation_time) # imputation tim?
    print(results$modelling_time) # modelling time
  }
}
# Działa dla dobrze wczytanych datasetów
