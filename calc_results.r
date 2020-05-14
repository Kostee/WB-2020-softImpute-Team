imputed_data <- read_all_imputed_datasets()

final_results <- vector(mode = "list", length = 5)
i1 <- 1
i2 <- 1
i3 <- 1
i4 <- 1
i5 <- 1

for (i in 1:2){
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
    i1 <- i1+4
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
    i2 <- i2+4
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
    i3 <- i3+4
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
    i4 <- i4+4
  }
  if (imputed_data[[i]]$imp_method == "imputation_fun_vim"){
    result <- get_result(imputed_data[[i]], train_and_predict_fun_rpart)
    final_results[[5]][[i5]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_bayes)
    final_results[[5]][[i5]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_ranger)
    final_results[[5]][[i5]] <- result
    result <- get_result(imputed_data[[i]], train_and_predict_fun_svm)
    final_results[[5]][[i5]] <- result
    i5 <- i5+4
  }
}