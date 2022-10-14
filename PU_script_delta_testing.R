library(dplyr)
library(caret)
source("datasets_prep.R")
source("prep_functions.R")
source("scoring_methods.R")

dsets<-
  ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

for ( i in dsets ){
  
  my_dataset <- get(i)
  
  my_dataset <- set_greater_class(my_dataset,1)
  
  assign(i,my_dataset)
  
  rm(my_dataset) 
}


results <- data.frame(
  
  dataset = character()
  , method = character()
  , c = numeric()
  , experiment_num = integer()
  , score = numeric()
  , Y_real = numeric()
  
)

append_results <- function(results_frame, dataset, method, c, experiment_num, score, Y_real){
  
  my_result_frame <- results_frame
  
  current_result <- data.frame(
    dataset = i
    , method = method
    , c = c
    , experiment_num = num
    , score = score
    , Y_real = y_test
  )
  
  my_result_frame <- rbind.data.frame(
    my_result_frame
    , current_result
  )
  
  my_result_frame
}


c_vector <- c(0.1,0.3,0.5,0.7,0.9)
c_vector <- sort(c_vector, decreasing = T)


for (i in dsets) {
  # results <- data.frame(
  #   
  #   dataset = character()
  #   , method = character()
  #   , c = numeric()
  #   , experiment_num = integer()
  #   , score = numeric()
  #   , Y_real = numeric()
    
  #)
  for (c in c_vector) {
    current_frame <- get(i)
    current_frame$Y <- as.numeric(current_frame$Y)
    current_frame$S <- current_frame$Y * rbinom(nrow(current_frame),1,c)
    current_frame <- as.data.frame(sapply(current_frame, as.numeric))
    for (num in 1:100) {
      
      #current_frame <- current_frame[,apply(current_frame, 2, function(x) sqrt(var(x))/mean(x)) != 0]
      sel_current_frame <- current_frame[,apply(current_frame, 2, function(x) sort(table(x), decreasing = T)[1]/length(x)) < 0.99]
      sel_current_frame$S <- NULL
      sel_current_frame$Y <- NULL
      
      pp <- caret::preProcess(sel_current_frame, method = c("range"))
      sel_current_frame <- predict(pp,sel_current_frame)
      
      sel_current_frame$S <- current_frame$S
      sel_current_frame$Y <- current_frame$Y
      current_frame <- sel_current_frame
      
      train <- sample_frac(current_frame,0.80)
      print(ncol(train))
      train <- train[,apply(train, 2, function(x){length(unique(x))}) != 1]
      print(ncol(train))
      current_frame <- current_frame[,colnames(train)]
      test <- dplyr::setdiff(current_frame,train)
      
      x_train <- as.matrix(train[,!colnames(current_frame) %in% c('Y','S')])
      y_train <- train$Y
      s_train <- train$S 
      
      x_test <- as.matrix(test[,!colnames(current_frame) %in% c('Y','S')])
      y_test <- test$Y
      
      ### Oracle -----
      
      print(paste0("Calculating dataset = ", i, " for c = ", c, " experiment no ", num))
      
      current_result <- data.frame(
        dataset = i
        , method = 'Oracle'
        , c = c
        , experiment_num = num
        , score = logistic(x_train = x_train, y_train = y_train, x_test = x_test)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )

      ### Naive -----
      
      current_result <- data.frame(
        dataset = i
        , method = 'Naive'
        , c = c
        , experiment_num = num
        , score = logistic(x_train = x_train, y_train = s_train, x_test = x_test)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      ### LassoMM_(log(p)/n)^(1/2) ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoMM_(log(p)/n)^(1/2)'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, lambda = '(log(p)/n)^(1/2)')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      ### LassoMM_(log(p)/n)^(1/3) ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoMM_(log(p)/n)^(1/3)'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, lambda = '(log(p)/n)^(1/3)')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      ### LassoMM_(log(p)/n)^(1/2))_dtl=0 ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoMM_(log(p)/n)^(1/3)_dtl=0'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, lambda = '(log(p)/n)^(1/2)', delta_to_lambda = 0)$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      


      ### LassoJoint_BFGS_lambda.min ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min delta_to_lambda = 0.3'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.min", delta_to_lambda = 0.3)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min delta_to_lambda = 0.5'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.min", delta_to_lambda = 0.5)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min delta_to_lambda = 0.7'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.min", delta_to_lambda = 0.7)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min delta_to_lambda = 1'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.min", delta_to_lambda = 1)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      

    }
    
  }
  saveRDS(results,'deltaMM_results.RDS')
}






