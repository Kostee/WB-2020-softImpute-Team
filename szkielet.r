set.seed(123)

library(mice)
library(missForest)
library(VIM)

# example dataset
data = read.csv("irish.csv")

read_dataset <- function(openml_id, dataset_dir){
  #ONETIME: dodac jako submodul repozytorium imputacji, żeby mieć w folderze /dependencies
  #sourcowanie odpowiedniego pliku
  #obrobka przez wywołanie code.R albo jakoś tak
  #return dataset
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

