source("datasets_prep.R")


greater_class <- function(data){
  
  n0 <- sum(data$Y == 0)
  n1 <- sum(data$Y == 1)
  
  ifelse(n0 > n1,0,1)
  
}

set_greater_class <- function(data, needed_greater_class){
  
  if (greater_class(data) != needed_greater_class) {
    
    data$Y <- ifelse(data$Y == 0,1,0)
  }
  
  data
  
}

