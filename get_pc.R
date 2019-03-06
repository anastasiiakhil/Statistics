get_pc <- function(d){
  PC <- prcomp(d)$x
  d$PC1 <- PC[,1]
  d$PC2 <- PC[,2]
  return(d)
}
