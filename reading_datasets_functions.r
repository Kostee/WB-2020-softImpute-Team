DFT_REPO_DATASET_DIR = './dependencies/datasets'
DFT_REPO_IMPED_DATASET_DIR = './imputed'
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
  surogate_env <- new.env(parent = .BaseNamespaceEnv)
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
#' dfs <- read_all_datasets()
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
  datasets_combined <- lapply(ids, function(x){read_dataset(x, dataset_dir)})
  
  datasets_combined <- t(datasets_combined)
  return(unname(t(datasets_combined)))
}

read_imputed_dataset <- function(id, method,  dataset_dir=  DFT_REPO_IMPED_DATASET_DIR){
  
  if(! is.character(method)){
    method <- deparse(substitute(imputation_fun))
  }
  
  dir <- paste0(DFT_REPO_IMPED_DATASET_DIR, '/', method, "/", id, ".rds")
  
  if(!dir.exists(paste0(DFT_REPO_IMPED_DATASET_DIR, '/', method))){
    stop(paste(dir, 'does not exist' ))
  }
  
  readRDS(dir)
}

read_imputed_dataset(188, "imputation_fun_vim")

read_all_imputed_datasets <- function(dataset_dir=  DFT_REPO_IMPED_DATASET_DIR){
  
  out <- list()
  for(m in dir(dataset_dir)){
    d <- paste0(dataset_dir, "/", m)
    if(!dir.exists(d)){
      stop(paste(dir, 'does not exist'))
    }
    ids <- dir(d)
    
    ids <- sapply(ids, function(x){substr(x,1,nchar(x) -4)}, simplify = TRUE)
    dfs <- lapply(ids, read_imputed_dataset, method = m)
    names(dfs) <- paste(m, ids, sep = "_")
    out  <- c(out, dfs)
  }
  return(out)
}

#read_all_imputed_datasets() -> x