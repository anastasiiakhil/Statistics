mean_a <- c(mean(iris$Sepal.Length), mean(iris$Sepal.Width),
            mean(iris$Petal.Length), mean(iris$Petal.Width))
result <- seq(0, 0, length = 150)
for (i in 1:nrow(iris)){
  result[i] <- ifelse(sum(iris[i,1:4] > mean_a) >= 3, "Yes", "No")
}

iris$important_cases <- as.factor(result)
