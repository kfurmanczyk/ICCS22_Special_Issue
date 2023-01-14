library(dplyr)
library(caret)
library(FSinR)
source("datasets_prep.R") #import datasets
source("prep_functions.R") #import functions to class swapping 
source("scoring_methods.R") #import functions to training and scoring

dsets<-
  ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

#set greater class as '1' class for all datasets
for ( i in dsets ){
  
  my_dataset <- get(i)
  
  my_dataset <- set_greater_class(my_dataset,1)
  
  assign(i,my_dataset)
  
  rm(my_dataset) 
}

#assign dataframe to storage results of scoring
results <- data.frame(
  
  dataset = character()
  , method = character()
  , c = numeric()
  , experiment_num = integer()
  , score = numeric()
  , Y_real = numeric()
  
)
#assign dataframe to storage results of selecting
no_features <- data.frame(
  
  dataset = character()
  , method = character()
  , c = numeric()
  , experiment_num = integer()
  , no_features = integer()
  
)

#function to appending new results in some methods
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

#set levels of c (labelling indicators) in calculations
c_vector <- c(0.1,0.3,0.5,0.7,0.9)
c_vector <- sort(c_vector, decreasing = T)


for (i in dsets) {

  for (c in c_vector) {
    current_frame <- get(i)
    current_frame$Y <- as.numeric(current_frame$Y)
    current_frame$S <- current_frame$Y * rbinom(nrow(current_frame),1,c)
    current_frame <- as.data.frame(sapply(current_frame, as.numeric))
    
    #set number of experiments (optionally it may depends of no. of variables - due to computational time)
    experi_max <- ifelse(ncol(current_frame)<100,50,5) 
    
    for (num in 1:experi_max) {
      
      if(sum(current_frame$S) < 8){break}
      
      #remove quasi-constant variables
      sel_current_frame <- current_frame[,apply(current_frame, 2, function(x) sort(table(x), decreasing = T)[1]/length(x)) < 0.99]

      
      #transform variables to 0-1 scale
      pp <- caret::preProcess(sel_current_frame, method = c("range"))
      sel_current_frame <- predict(pp,sel_current_frame)
      
      sel_current_frame$S <- current_frame$S
      sel_current_frame$Y <- current_frame$Y
      my_current_frame <- sel_current_frame
      
      #split dataset into train and test sample
      train <- sample_frac(my_current_frame,0.80)
      print(ncol(train))
      train <- train[,apply(train, 2, function(x){length(unique(x))}) != 1]
      print(ncol(train))
      my_current_frame <- my_current_frame[,colnames(train)]
      test <- dplyr::setdiff(my_current_frame,train)
      
      x_train <- as.matrix(train[,!colnames(my_current_frame) %in% c('Y','S')])
      y_train <- train$Y
      s_train <- train$S 
      
      
      if(sum(s_train) < 8){break} #break experiments when S have less then 8 labelled obs (due to numeric reasons)
      
      x_test <- as.matrix(test[,!colnames(my_current_frame) %in% c('Y','S')])
      y_test <- test$Y
      
      
      print(paste0("Calculating dataset = ", i, " for c = ", c, " experiment no ", num))
      
      
      
      #perform experiments and store the results
      
      ### LassoJoint_BFGS_(log(p)/n)^(1/2) ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_(log(p)/n)^(1/2)'
        , c = c
        , experiment_num = num
        , score = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, lambda = '(log(p)/n)^(1/2)')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      current_features <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_(log(p)/n)^(1/2)'
        , c = c
        , experiment_num = num
        , no_features = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, lambda = '(log(p)/n)^(1/2)')$nosnik_dl
      )
      
      no_features <- rbind.data.frame(
        no_features
        , current_features
      )
      
      ### LassoJoint_BFGS_lambda.min ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min'
        , c = c
        , experiment_num = num
        , score = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.min')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      current_features <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min'
        , c = c
        , experiment_num = num
        , no_features = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.min')$nosnik_dl
      )
      
      no_features <- rbind.data.frame(
        no_features
        , current_features
      )
      
      ### LassoJoint_BFGS_lambda.1se ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.1se'
        , c = c
        , experiment_num = num
        , score = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.1se')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      current_features <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.1se'
        , c = c
        , experiment_num = num
        , no_features = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.1se')$nosnik_dl
      )
      
      no_features <- rbind.data.frame(
        no_features
        , current_features
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
      
      current_features <- data.frame(
        dataset = i
        , method = 'LassoMM_(log(p)/n)^(1/2)'
        , c = c
        , experiment_num = num
        , no_features = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, lambda = '(log(p)/n)^(1/2)')$nosnik_dl
      )
      
      no_features <- rbind.data.frame(
        no_features
        , current_features
      )
      
      ### LassoMM_lambda.min ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoMM_lambda.min'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.min')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      current_features <- data.frame(
        dataset = i
        , method = 'LassoMM_lambda.min'
        , c = c
        , experiment_num = num
        , no_features = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.min')$nosnik_dl
      )
      
      no_features <- rbind.data.frame(
        no_features
        , current_features
      )
      
      ### LassoMM_lambda.1se ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoMM_lambda.1se'
        , c = c
        , experiment_num = num
        , score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.1se')$scores
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      current_features <- data.frame(
        dataset = i
        , method = 'LassoMM_lambda.1se'
        , c = c
        , experiment_num = num
        , no_features = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = 'lambda.1se')$nosnik_dl
      )
      
      no_features <- rbind.data.frame(
        no_features
        , current_features
      )

      
      
     
    }
    
    saveRDS(results,'LassoJoint_fixed_results3.RDS')
    saveRDS(no_features,'LassoJoint_fixed_no_features3.RDS')
  }
}







