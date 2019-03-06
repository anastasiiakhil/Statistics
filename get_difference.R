get_difference <-  function(test_data, n_cluster){
  fit <- hclust(dist(test_data))
  test_data$cluster <- as.factor(cutree(fit, n_cluster))
  p_value <- sapply(test_data[,-(ncol(test_data))], 
                    function(x) summary(aov(x ~ cluster, data = test_data))[[1]]$'Pr(>F)'[1])
  return (colnames(test_data)[ which(p_value < 0.05)])
}
