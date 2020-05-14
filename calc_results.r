source("reading_datasets_functions.r")
imputed_data <- read_all_imputed_datasets()

for(i in 1:61){
  if (imputed_data[[i]]$imp_method == "imputation_mode_median"){
    imputed_data[[i]]$dataset <- imputation_mode_median(imputed_data[[i]]$dataset)
  }
}



final_results <- vector(mode = "list", length = 5)
i1 <- 1
i2 <- 1
i3 <- 1
i4 <- 1
i5 <- 1
i6 <- 1
# 3 - zawiera NA
# 29 - Error: cannot allocate vector of size 2.1 Gb 
# 35 - R session aborted
# 43 - Error: cannot allocate vector of size 2.1 Gb 
# 48 - Error in 1:numclass : result would be too long a vector
# 49 - R session aborted
# 55 - Error in 1:numclass : result would be too long a vector
c(1,2,4:28)
for (i in c(1:2, 4:28, 30:34, 36:42, 44:47, 50:54, 56:61)){
  print(i)
  
  if (imputed_data[[i]]$imp_method == "imputation_remove_rows"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[1]][[i1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[1]][[i1+1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[1]][[i1+2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[1]][[i1+3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_knn)
    final_results[[1]][[i1+4]] <- result
    i1 <- i1+5
  }
  
  if (imputed_data[[i]]$imp_method == "imputation_mode_median"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[2]][[i2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[2]][[i2+1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[2]][[i2+2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[2]][[i2+3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_knn)
    final_results[[2]][[i2+4]] <- result
    i2 <- i2+5
  }
  
  if (imputed_data[[i]]$imp_method == "imputation_fun_mice"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[3]][[i3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[3]][[i3+1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[3]][[i3+2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[3]][[i3+3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_knn)
    final_results[[3]][[i3+4]] <- result
    i3 <- i3+5
  }
  
  if (imputed_data[[i]]$imp_method == "imputation_fun_missForest"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[4]][[i4]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[4]][[i4+1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[4]][[i4+2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[4]][[i4+3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_knn)
    final_results[[4]][[i4+4]] <- result
    i4 <- i4+5
  }
  
  if (imputed_data[[i]]$imp_method == "imputation_fun_vim"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[5]][[i5]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[5]][[i5+1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[5]][[i5+2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[5]][[i5+3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_knn)
    final_results[[5]][[i5+4]] <- result
    i5 <- i5+5
  }
  
  if (imputed_data[[i]]$imp_method == "imputation_random"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[6]][[i6]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[6]][[i6+1]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[6]][[i6+2]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[6]][[i6+3]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_knn)
    final_results[[6]][[i6+4]] <- result
    i6 <- i6+5
  }
}

saveRDS(final_results, "final_results.RDS")

final <- readRDS("final_results.RDS")
final[[1]][[2]]
