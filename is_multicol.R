is_multicol <- function(d){
  corel <- round(cor(d, d), 4)
  diag(corel) <- 0
  if (any(corel == 1 | corel == (-1))){
    m <- rep(colnames(corel), times = length(colnames(corel)))
    return (sort(m[which(corel == 1 | corel == (-1))]))
  } else {
    return ("There is no collinearity in the data")
  }
}
