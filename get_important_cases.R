get_important_cases <- function(x){
  result <- seq(0, 0, ncol(x))
  for (i in 1:nrow(x)){
    result[i] <- ifelse(sum(x[i,] > colMeans(x)) > ncol(x)/2,
                        "Yes", "No")
  }
  x$important_cases <- factor(result, levels = c("Yes", "No"))
  return (x)
}


get_important_cases <- function(x){
  v = colMeans(x)
  compare_to_means <- apply(x, 1, function(n) as.numeric(n > v))
  important <- apply(compare_to_means, 2, sum) > ncol(x)/2
  important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))
  x$important_cases <- important
  return(x)
}
