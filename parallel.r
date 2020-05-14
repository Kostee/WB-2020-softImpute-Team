set.seed(123)

source('./imputations_wrapped_functions.r')
source('./metrics_functions.r')
source('./reading_datasets_functions.r')

if(!dir.exists('./imputed')){
  dir.create("./imputed")
}

save_imputed_datasets <- function(dataset_list, imputation_fun){
  
  dataset <- dataset_list$dataset
  name_of_target <- dataset_list$target
  
  # imputation
  imputation_start = Sys.time() # start to measure time
  imputated_dataset <- imputation_fun(dataset) 
  imputation_stop = Sys.time() # end measuring time
  
  dataset_list$imp_time <- imputation_stop - imputation_start
  
  dataset_list$dataset <- imputated_dataset
  
  imp_method_name <- deparse(substitute(imputation_fun))
  
  dataset_list$imp_method <- imp_method_name
  
  dfdir <- paste0('./imputed/', imp_method_name)
  
  dfname <- paste0('./imputed/', imp_method_name,'/', dataset_list$id, '.rds')
  
  if(!dir.exists(dfdir)){
    dir.create(dfdir)
  }
  
  saveRDS(dataset_list, dfname)
}


library(parallelMap)
rdzenie <- parallel::detectCores()
parallelStartMulticore(rdzenie, show.info = TRUE)

# reading datasets from OpenML's edits
data_all <- read_all_datasets()

# imputations and targets preparation
imputations <- list(imputation_fun_vim,
                    imputation_fun_missForest,
                    imputation_remove_rows,
                    imputation_mode_median,
                    imputation_fun_mice)

starttime <- Sys.time()
parallelLapply(data_all, function(x){save_imputed_datasets(x,imputation_remove_rows)}, impute.error = function(x){'ERROR'})
parallelLapply(data_all, function(x){save_imputed_datasets(x,imputation_mode_median)}, impute.error = function(x){'ERROR'})
parallelLapply(data_all, function(x){save_imputed_datasets(x,imputation_fun_mice)}, impute.error = function(x){'ERROR'})
parallelLapply(data_all, function(x){save_imputed_datasets(x,imputation_fun_missForest)}, impute.error = function(x){'ERROR'})
parallelLapply(data_all, function(x){save_imputed_datasets(x,imputation_fun_vim)}, impute.error = function(x){'ERROR'})
parallelLapply(data_all, function(x){save_imputed_datasets(x,imputation_random)}, impute.error = function(x){'ERROR'})
endtime <- Sys.time()
endtime - starttime




