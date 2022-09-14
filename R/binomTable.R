#' Binomialverteilungstabelle
#' 
#' Erstellt eine Binomialverteilungstabelle basierend auf der Stichprobengroesse und der Wahrscheinlichkeit Pi
#' 
#' @param n Stichprobengroesse
#' @param probability Wahrscheinlichkeit des Ereignisses
#' @return Binomialverteilungstabelle
#' @export 

binomTable <- function(n, probability) {
  options("scipen" = 4)
  col_1 <- c(probability, 0:n, "")
  
  left = NULL
  for (i in 0:n) {
    left <- c(left, format(round(pbinom(i, n, probability), 4), nsmall = 4))}
  col_2 <- c("X<=x", left,"X>=x")
  
  
  center = NULL
  for (j in 0:n) {
    center <- c(center, format(round(dbinom(j, n, probability), 4), nsmall = 4))}
  col_3 <- c("X=x", center, "X=x")
  
  right = NULL
  for (k in n:0) {
    right <- c(right, format(round(pbinom(k, n, 1-probability),4), nsmall = 4))}
  col_4 <- c("X>=x", right, "X<=x")
  
  col_5 <- c("", n:0, 1-probability)
  
  table <- data.frame(col_1, col_2, col_3, col_4, col_5)
  colnames(table) <- NULL
  print(table)
}
