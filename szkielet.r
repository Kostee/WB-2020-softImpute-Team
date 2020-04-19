#setwd("C:/Users/ckostern/Desktop/Studia/03 rok II semestr/WB/WB_Prezentacja_softImpute-master")
#getwd()
set.seed(123)

library(mice)
library(missForest)
library(VIM)

# not needed directly, but have to be installed
#library(jsonlite)

DFT_REPO_DATASET_DI? = './dependencies/datasets'
getwd()

#' Reads dataset based on id from directory
#' @param openml_id int. Should be present in dataset_dir
#' @param dataset_dir string with directory with subdirectories like 'openml_dataset_<openml_id>'
#' @return list, w?ere list$dataset contains processed data
#' @seealso names(list)
read_dataset <- function(openml_id, dataset_dir = DFT_REPO_DATASET_DIR){
  
  if (!dir.exists(dataset_dir)){
    stop(paste(dataset_dir, 'does not exist' ))
  }
  
  dir <- paste(dataset_dir,?paste('openml_dataset', openml_id, sep = '_'), sep ='/')
  if (!dir.exists(dir)){
    stop(paste(dir, 'does not exist' ))
  }
  
  start_dir <- getwd()
  
  #set right dir to code.R to acually work - it depends on dirlocation to create json
  setwd(dir)
  ?  # use new env to avoid trashing globalenv
  surogate_env = new.env(parent = .BaseNamespaceEnv)
  attach(surogate_env)
  source("code.R",surogate_env)
  
  j <- jsonlite::read_json('./dataset.json')
  j$dataset <- surogate_env$dataset
  setwd(start_dir)
 ?
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
read_all_datasets <- function(dat?set_dir = DFT_REPO_DATASET_DIR){
  
  if (!dir.exists(dataset_dir)){
    stop(paste(dataset_dir, 'does not exist'))
  }
  
  start_dir <- getwd()
  subdirs <- dir(dataset_dir)
  ids <- sapply(subdirs, function(dir){substr(dir, 16, nchar(dir))})
  datasets_?ombined <- sapply(ids, function(x){read_dataset(x, dataset_dir)})
  
  datasets_combined <- t(datasets_combined)
  return(unname(t(datasets_combined)))
}


# IMPUTATION FUNCTIONS

imputation_fun_mice <- function(df){
  init <- mice(data, maxit=0) 
  meth <? init$method
  predM <- init$predictorMatrix
  imputed <- mice(data, method=meth, predictorMatrix=predM, m=5)
  completed <- complete(imputed)
  return(completed[1]) # UWAGA! Kuba dodal tu [1], zeby potem miec juz czysty dataframe
}

imputation_fun_vim <- ?unction(df){
  no_columns <- length(df)
  imputed <- kNN(df)
  imputed <- imputed[,1:no_columns]
  return(imputed)
}

imputation_fun_missForest <- function(df){
  return(missForest(df)$ximp)
}

imputation_remove_rows <- function(df){
  return (na.omit(df))?}

# impute median within numeric type columns and mode within character type ones 
imputation_mode_median <- function(df){
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  for (i in 1L:length(df)){
    if (?um(is.na(df[,i])) > 0){
      if (mode(df[,i]) == 'character' | is.factor(df[,i])){
        to_imp <- Mode(df[,i])
        df[,i][is.na(df[,i])] <- to_imp
      }
      else{
        to_imp <- median(df[,i], na.rm = TRUE) 
        df[,i][is.na(df[,i])] <- ?o_imp
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
  
  train <- datase?[train_ind, ]
  test <- dataset[-train_ind, ]
  
  return (list(train, test))
}


# METRICS FUNCTIONS

get_confusion_matrix <- function(test, pred){
  return (table(Truth = test, Prediction = pred))
}

confusion_matrix_values <- function(confusion_matrix){?  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  return (c(TP, TN, FP, FN))
}

accuracy <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  ret?rn((conf_matrix[1] + conf_matrix[2]) / (conf_matrix[1] + conf_matrix[2] + conf_matrix[3] + conf_matrix[4]))
}

precision <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  return(conf_matrix[1]/ (conf_matrix[1] + co?f_matrix[3]))
}

recall <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(confusion_matrix)
  return(conf_matrix[1] / (conf_matrix[1] + conf_matrix[4]))
}

f1 <- function(confusion_matrix){
  conf_matrix <- confusion_matrix_values(con?usion_matrix)
  rec <- recall(confusion_matrix)
  prec <- precision(confusion_matrix)
  return(2 * (rec * prec) / (rec + prec))
}


library(rpart) # for classification tree
get_result <- function(dataset, imputation_fun, name_of_target){
  # imputation
  i?putation_start = Sys.time() # start to measure time
  imputated_dataset <- imputation_fun(dataset) 
  imputation_stop = Sys.time() # end measuring time
  
  # train test split
  train_test <- train_test_split(imputated_dataset, 0.8)
  train <- as.data.tabl?(train_test[1])
  test <- as.data.table(train_test[2])
  
  # modelling
  modelling_start = Sys.time() # start to measure time
  tree_model <- rpart(train[[name_of_target]] ~ ., data = train,
                      method = "class", control = rpart.control(?p = 0))
  y_pred <- as.data.frame(predict(tree_model, test, type = "class"))
  modelling_stop = Sys.time() # end measuring time
  
  # calculating metrics
  confusion_matrix <- get_confusion_matrix(test[[name_of_target]], y_pred[,1])

  accuracy_v <- accur?cy(confusion_matrix)
  precision_v <- precision(confusion_matrix)
  recall_v <- recall(confusion_matrix)
  f1_v <- f1(confusion_matrix)
  
  classification_report <- data.frame(accuracy_v, precision_v,
                                      recall_v, f1_v)
? colnames(classification_report) <- c("accuracy", "precision",
                                       "recall", "f1")
  
  return(list(confusion_matrix = confusion_matrix,
                    classification_report = classification_report,
                 ?  imputation_time = imputation_stop - imputation_start,
                    modelling_time = modelling_stop - modelling_start))
}
# example: get_result(data_all[[2]]$dataset, imputation_fun_vim, data_all[[2]]$target)
# TODO: dokumentacja tego, tu myœlê war?o

# reading datasets from OpenML's edits
data_all <- read_all_datasets()


#PARALLEL ROBI BRRRRR [TODO bo nie wiem jak]
# ...
# ...
# ...


# imputations and targets preparation
imputations <- list(imputation_fun_vim, imputation_fun_missForest,
          ?         imputation_remove_rows, imputation_mode_median, imputation_fun_mice)
targets <- list()
for (i in 1:length(data_all)){
  targets[[i]] <- data_all[[i]]$target
}

# every imputation on each dataset
for (imputation in imputations){
  for(i in 1:length?data_all)){
    results <- get_result(data_all[[i]]$dataset, imputation, targets[i])
    print(results$confusion_matrix) # confusion_matrix
    print(results$classification_report) # classification_report
    print(results$imputation_time) # imputation tim?
    print(results$modelling_time) # modelling time
  }
}
# TODO: sprawdŸ czy dzia³a