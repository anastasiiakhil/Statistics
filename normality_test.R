normality_test <- function(dataset){
  k <- dataset[sapply(dataset, is.numeric)]
  for( i in 1:length(colnames(k))){
    a[i] <- shapiro.test(k[,i])$p.value
  }
  return(a)
}

normality_test <- function(dataset){
  var <- sapply(dataset, is.numeric)
  return(sapply(dataset[var],function(x) shapiro.test(x)$p.value))
}
