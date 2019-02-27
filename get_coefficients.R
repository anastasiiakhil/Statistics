get_coefficients <- function(dataset){
  fit <- glm(y ~ x, dataset, family = "binomial")
  return(exp(fit$coefficients))
}
