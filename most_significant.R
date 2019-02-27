most_significant <-  function(x){
  p_value <- c()
  for (i in 1:length(colnames(x))){
    p_value <- c(p_value, chisq.test(table(factor(x[,i])))$p.value)
  }
  min_position <- which(p_value == min(p_value))
  return (colnames(x)[min_position])
}
