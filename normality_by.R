normality_by <- function(test){
  lev_1 <- levels(as.factor(test$y))
  lev_2 <- levels(as.factor(test$z))
  a <- shapiro.test(test$x[as.factor(test$y) == lev_1[1] & as.factor(test$z) == lev_2[1]])$p.value
  b <- shapiro.test(test$x[as.factor(test$y) == lev_1[1] & as.factor(test$z) == lev_2[2]])$p.value
  c <- shapiro.test(test$x[as.factor(test$y) == lev_1[2] & as.factor(test$z) == lev_2[1]])$p.value
  d <- shapiro.test(test$x[as.factor(test$y) == lev_1[2] & as.factor(test$z) == lev_2[2]])$p.value
  result <- data.frame(y = c(lev_1[1], lev_1[1], lev_1[2], lev_1[2]),
                       z = c(lev_2[1], lev_2[2], lev_2[1], lev_2[2]),
                       p_value =c(a,b,c,d))
  return (result)
}

normality_by <- function(test){    
  grouped_data <- aggregate(test[,1],by=list(test[,2], test[,3]),                                  
                            FUN = function(x) {shapiro.test(x)$p.value})                                  
  names(grouped_data) <- c(colnames(test)[2:3],'p_value')                                  
  return(grouped_data)    
}
