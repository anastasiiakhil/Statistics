library(ggplot2)

fit <- hclust(dist(swiss))
swiss$cluster <- as.factor(cutree(fit, 2))
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) + geom_smooth(method = "glm") + geom_point()
