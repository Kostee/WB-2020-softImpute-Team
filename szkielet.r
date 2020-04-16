set.seed(123)

library(mice)
library(missForest)
library(VIM)

REPO_DATASET_DIR = './dependecies/datasets'

#' Reads dataset based on id from directory
#' @param openml_id int. Should be present in dataset_dir
#' @param dataset_dir string with directory with subdirectories like 'openml_dataset_<openml_id>'
#' @return dataset
read_dataset <- function(openml_id, dataset_dir = REPO_DATASET_DIR){
  
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
  source('code.R')
  setwd(start_dir)
  return(dataset)
}


#' Reads all dataset from given directory
#' @param dataset_dir string with directory with subdirectories like 'openml_dataset_<openml_id>'
#' @return vector of datasets
read_all_datasets <- function(dataset_dir){
  
  if (!dir.exists(dataset_dir)){
    stop(paste(dataset_dir, 'does not exist'))
  }
  
  start_dir <- getwd()
  subdirs <- dir(dataset_dir)
  ids <- sapply(subdirs, function(dir){substr(dir, 16, nchar(dir))})
  datasets_combined <- sapply(ids, function(x){read_dataset(x, dataset_dir)})
}



imputation_fun_mice <- function(df){
  init <- mice(data, maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  imputed <- mice(data, method=meth, predictorMatrix=predM, m=5)
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
  return (na.omit(df))
}

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


get_result <- function(dataset, imputation_fun, ...){
  #zmierzyć czas teog ponizej
  imputated_dataset <- imputation_fun(dataset, ...)
  #tworzenie modelu ALBO danie nienauczonego modelu jako argument? - random forest nie jest zły
  #liczymy miary różne
  #ewentualnie zapisywac do pliku wyniki
  #return tabelka z miarami  + czas imputacji
}


#PARALLEL ROBI BRRRRR

#IMPUTACJE ROBIĄ BRRRRR
imputation_fun_mice(data)
imputation_fun_vim(data)
imputation_fun_missForest(data)
imputation_remove_rows(data)
imputation_mode_median(data)

