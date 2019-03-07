get_pca2 <- function(data){
  fit <- prcomp(data)
  proportion <- summary(fit)$importance[3,]
  k <- which(proportion > 0.9)[1]
  return(cbind(data, fit$x[,1:k]))
}
