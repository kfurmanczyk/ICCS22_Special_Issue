library(dplyr)
library(caret)
library(FSinR)
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

no_features <- data.frame(
  
  dataset = character()
  , method = character()
  , c = numeric()
  , experiment_num = integer()
  , no_features = integer()
  
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
    
    experi_max <- ifelse(ncol(current_frame)<100,100,5)
    
    for (num in 1:experi_max) {
      
      if(sum(current_frame$S) < 8){break}
      
      #current_frame <- current_frame[,apply(current_frame, 2, function(x) sqrt(var(x))/mean(x)) != 0]
      sel_current_frame <- current_frame[,apply(current_frame, 2, function(x) sort(table(x), decreasing = T)[1]/length(x)) < 0.99]

      train <- sample_frac(sel_current_frame,0.80)
      sel_current_frame <- train
      ### Feature selection 
      sel_current_frame$Y <- NULL
      sel_current_frame$S <- NULL
      sel_current_frame_org <- sel_current_frame
      sel_current_frame <- discretize(sel_current_frame)
      sel_current_frame$S <- train$S
      evaluator <- filterEvaluator('mutualInformation')
      
      max_K = 5
      if (ncol(sel_current_frame)-1 < max_K) {
        k = ncol(sel_current_frame)-2 ## jak mniej niż K to ucinamy jedną zmienną
      } else {
        k = max_K
      }
      
      directSearcher <- directSearchAlgorithm('selectKBest', list(k=k))
      results_fil <- directFeatureSelection(sel_current_frame, 'S', directSearcher, evaluator)

      sel_current_frame <- sel_current_frame_org[, results_fil$featuresSelected]
      ### Feature selection end 
      
      sel_current_frame <- current_frame[, c(results_fil$featuresSelected,'Y','S')]
      
      pp <- caret::preProcess(sel_current_frame, method = c("range"))
      sel_current_frame <- predict(pp,sel_current_frame)
      
      sel_current_frame$S <- current_frame$S
      sel_current_frame$Y <- current_frame$Y
      my_current_frame <- sel_current_frame
      
      train <- sample_frac(my_current_frame,0.80)
      print(ncol(train))
      train <- train[,apply(train, 2, function(x){length(unique(x))}) != 1]
      print(ncol(train))
      my_current_frame <- my_current_frame[,colnames(train)]
      test <- dplyr::setdiff(my_current_frame,train)
      
      x_train <- as.matrix(train[,!colnames(my_current_frame) %in% c('Y','S')])
      y_train <- train$Y
      s_train <- train$S 
      
      if(sum(s_train) < 8){break}
      
      x_test <- as.matrix(test[,!colnames(my_current_frame) %in% c('Y','S')])
      y_test <- test$Y
      
      
      print(paste0("Calculating dataset = ", i, " for c = ", c, " experiment no ", num))
      
      
      ### Joint BFGS-----
      
      current_result <- data.frame(
        dataset = i
        , method = 'MIF5 Joint BFGS'
        , c = c
        , experiment_num = num
        , score = logistic_joint(x_train = x_train, y_train = s_train, x_test = x_test, optim_method = "BFGS") %>% as.numeric()
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      ### Joint MM-----
      score = logistic_cdmm(x_train = x_train, y_train = s_train, x_test = x_test)
      score = round(score,4)
      
      results <- append_results(results_frame = results, dataset = i, method = "MIF5 Joint MM", c = c, experiment_num = num, score = score, Y_real = y_test)
    }
    
    saveRDS(results,'MIF5Joint_results.RDS')
  }
}







