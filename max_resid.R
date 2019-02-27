max_resid <- function(x){
  k <- chisq.test(x$Drugs,x$Result)
  p <- which(k$stdres == max(k$stdres), arr.ind = TRUE)
  
  return(c(rownames(k$stdres)[p[1]], colnames(k$stdres)[p[2]]))
}
