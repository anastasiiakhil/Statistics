smart_test <- function(x){
  table(x)
  if (min(table(x)) < 5) {
    return (fisher.test(table(x))$p.value)
  } else {
    return (c(as.vector(chisq.test(table(x))$statistic),
              as.vector(chisq.test(table(x))$parameter),
              chisq.test(table(x))$p.value))
  }
}
