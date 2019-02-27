centered <- function(test_data, var_names){
  test_data_1 <- test_data - t(matrix(colMeans(test_data), ncol = nrow(test_data),
                                      nrow = ncol(test_data)))
  n <- c()
  for (i in 1:length(var_names)){
    r <- which(colnames(test_data) == var_names[i])
    n <- c(n, r)
  }
  for (i in 1:length(n)){
    test_data[,n[i]] <- test_data_1[,n[i]]
  }
  return(test_data)
}


centered <- function(test_data, var_names){    
  test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x))    
  return(test_data)    
}
