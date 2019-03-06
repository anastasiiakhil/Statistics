smart_hclust<-  function(test_data, cluster_number){
  fit <- hclust(dist(test_data))
  test_data$cluster <- as.factor(cutree(fit, cluster_number))
  return(test_data)
}
