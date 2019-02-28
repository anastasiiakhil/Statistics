get_features <- function(dataset){
  model = glm(is_prohibited ~ weight + length + width + type, data=dataset, family = binomial)
  result <- anova(model, test = "Chisq")
  k <- rownames(result)[result$`Pr(>Chi)` < 0.05][-1]
  if (length(k)== 0){
    return ("Prediction makes no sense")
  } else {
    return (k)
  }
}
