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

get_result <- function(dataset, imputation_fun, ...){
  #zmierzyć czas teog ponizej
  imputated_dataset <- imputation_fun(dataset, ...)
  #tworzenie modelu ALBO danie nienauczonego modelu jako argument? - random forest nie jest zły
  #liczymy miary różne
  #ewentualnie zapisywac do pliku wyniki
  #return tabelka z miarami  + czas imputacji
}

#PARALLEL ROBI BRRRRR