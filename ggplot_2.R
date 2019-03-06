library("ggplot2")

obj <- ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_density(alpha = 0.2)
