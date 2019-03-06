smart_anova <- function(test_data){
  p_shapiro <- c(shapiro.test(test_data$x[test_data$y == "A"])$p.value, 
                 shapiro.test(test_data$x[test_data$y == "B"])$p.value, 
                 shapiro.test(test_data$x[test_data$y == "C"])$p.value) > 0.05
  p_bartlett <- bartlett.test(test_data$x, test_data$y)$p.value
  
  if (sum(p_shapiro) == 3 & p_bartlett > 0.05) {
    fit <- aov(x ~ y, data = test_data)
    p_value <- c(ANOVA = summary(fit)[[1]]$'Pr(>F)'[1])
    } else {
      p_value <- c(KW = kruskal.test(x ~ y, data = test_data)$p.value)
    }
  return (p_value)
}
