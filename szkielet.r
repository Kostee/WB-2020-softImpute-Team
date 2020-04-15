set.seed(123)

# example dataset
data = read.csv("irish.csv")

read_dataset <- function(openml_id, dataset_dir){
  #ONETIME: dodac jako submodul repozytorium imputacji, żeby mieć w folderze /dependencies
  #sourcowanie odpowiedniego pliku
  #obrobka przez wywołanie code.R albo jakoś tak
  #return dataset
}

imputation_fun_mice <- function(df){
  #df_imp <- mice(cośtam, cośtam)
  #return mice$result czy cokolwiek innego 
}

# i tutaj kolejne 4 funkcje, unless nie potrzebują specjalnego trakowania i można je załatwić elipsisem(...)

# missForest
library(missForest)

imputation_missForest <- function(df){
  return(missForest(df)$ximp)
}


# delete rows with missing values (można wpisać w ellipsis)
imp_remove_rows <- function(df){
  return (na.omit(df))
}

# impute median within numeric type columns and mode within character type ones 
imp_mode_median <- function(df){
  
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


imputation_missForest(data)
imp_remove_rows(data)
imp_mode_median(data)


