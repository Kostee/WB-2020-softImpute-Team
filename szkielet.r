#setwd("C:/Users/ckostern/Desktop/Studia/03 rok II semestr/WB/WB_Prezentacja_softImpute-master")
#getwd()
set.seed(123)

library(mice)
library(missForest)
library(VIM)

# not needed directly, but have to be installed
#library(jsonlite)

DFT_REPO_DATASET_DIR = './dependencies/datasets'
getwd()

#' Reads dataset based on id from directory
#' @param openml_id int. Should be present in dataset_dir
#' @param dataset_dir string with directory with subdirectories like 'openml_dataset_<openml_id>'
#' @return list, where list$dataset contains processed data
#' @seealso names(list)
read_dataset <- function(openml_id, dataset_dir = DFT_REPO_DATASET_DIR){
  
  if (!dir.exists(dataset_dir)){
    stop(paste(dataset_dir, 'does not exist' ))
  }
  
  dir <- paste(dataset_dir, paste('openml_dataset', openml_id, sep = '_'), sep ='/')
  if (!dir.exists(dir)){
    stop(paste(dir, 'does not exist' ))
  }
  
  start_dir <- getwd()
  
  #set right dir to code.R to acually work - it depends on dirlocation to create json
  setwd(dir)
   # use new env to avoid trashing globalenv
  surogate_env = new.env(parent = .BaseNamespaceEnv)
  attach(surogate_env)
  source("code.R",surogate_env)
  
  j <- jsonlite::read_json('./dataset.json')
  j$dataset <- surogate_env$dataset
  setwd(start_dir)
 
  return(j)
}


#' Reads all dataset from given directory
#' @param dataset_dir string with directory with subdirectories like 'openml_dataset_<openml_id>'
#' 
#' @return matrix with columns = c(dataset, target_name)
#' 
#' @example 
#' dfs <- read_all_da?asets()
#' df <- dfs[[1]]
#' 
#' target_name <- df$target
#' cat('Target name: ', target_name)
#' 
#' n <- df$number_of_features_with_missings
#' cat('Number of features with missing data: ', n)
#' 
#' @seealso names(list)
read_all_datasets <- function(dataset_dir = DFT_REPO_DATASET_DIR){
  
  if (!dir.exists(dataset_dir)){
    stop(paste(dataset_dir, 'does not exist'))
  }
  
  start_dir <- getwd()
  subdirs <- dir(dataset_dir)
  ids <- sapply(subdirs, function(dir){substr(dir, 16, nchar(dir))})
  datasets_combined <- sapply(ids, function(x){read_dataset(x, dataset_dir)})
  
  datasets_combined <- t(datasets_combined)
  return(unname(t(datasets_combined)))
}


# IMPUTATION FUNCTIONS

imputation_fun_mice <- function(df){
  init <- mice(df, maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  imputed <- mice(df, method=meth, predictorMatrix=predM, m=5)
  completed <- complete(imputed)
  return(completed)
}

imputation_fun_vim <- function(df){
  no_columns <- length(df)
  imputed <- kNN(df)
  imputed <- imputed[,1:no_columns]
  return(imputed)
}

imputation_fun_missForest <- function(df){
  return(missForest(df)$ximp)
}

imputation_remove_rows <- function(df){
  return (na.omit(df))}

# impute median within numeric type columns and mode within character type ones 
imputation_mode_median <- function(df){
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  for (i in 1L:length(df)){
    if (sum(is.na(df[,i])) > 0){
      if (mode(df[,i]) == 'character' | is.factor(df[,i])){
        to_imp <- Mode(df[,i])
        df[,i][is.na(df[,i])] <- to_imp
      }
      else{
        to_imp <- median(df[,i], na.rm = TRUE) 
        df[,i][is.na(df[,i])] <- to_imp
      }
    }
  }
  
  return(df)
}


#' Splits dataset into train and test sets, taking into account train size
#' @param dataset dataset to split
#' @param train_size size of train dataset, where 0 - no elements, 1 - all the elements
#' @return lis?, where list[1] - train frame, list[2] - test frame
train_test_split <- function(dataset, train_size){
  smp_size <- floor(train_size * nrow(dataset))
  typeof(smp_size)
  
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
  
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  
  return (list(train, test))
}


# METRICS FUNCTIONS

get_confusion_matrix <- function(test, pred){
  return (table(Truth = test, Prediction = pred))
}

confusion_matrix_values <- function(confusion_matrix){
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  return (c(TP, TN, FP, FN))
}

accuracy <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  return((conf_matrix[1] + conf_matrix[2]) / (conf_matrix[1] + conf_matrix[2] + conf_matrix[3] + conf_matrix[4]))
}

precision <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  return(conf_matrix[1]/ (conf_matrix[1] + conf_matrix[3]))
}

recall <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  return(conf_matrix[1] / (conf_matrix[1] + conf_matrix[4]))
}

f1 <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  rec <- recall(confusion_matrix)
  prec <- precision(confusion_matrix)
  return(2 * (rec * prec) / (rec + prec))
}


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
  
  imp_method_name <- as.character(substitute(imputation_fun))
  
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

loadRegistry(file.dir = './registry', writeable = TRUE)

# THIS WILL ERASE ALL PREVIOUS COMPUTATUONS
# # clearRegistry()

# this creates new registry
# # registry <- makeRegistry(file.dir = "./registry", seed = 15390)

makeClusterFunctionsMulticore()

# careful to run once, should have 40 jobs in getJobTable()
batchMap(fun = get_result, dataset = data_all, imputation_fun = rep(imputations, length(data_all)))
getJobTable()

# resources = list(walltime = 3600, memory = 1024)
# see more at resources at ?submitJobs
submitJobs()

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
