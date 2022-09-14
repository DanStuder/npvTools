#' Punktwahrscheinlichkeit exakter 2xk-Tests
#' 
#' Berechnet die Punktwahrscheinlichkeit des exakten 2xk-Tests
#' 
#' @param data Matrix mit den beobachteten Werten
#' @return Matrix mit den erwarteten Werten
#' @export


exact_2xk_pw <- function(data)  {
  
  # Berechne Randsummen und n
  col_sums <- colSums(data)
  row_sums <- rowSums(data)
  n <- sum(row_sums)
  
  # Erstelle Matrix mit Input und Randsummen
  
  matrix <- rbind(data, col_sums)
  matrix <- cbind(matrix, c(row_sums,n))
  
  # Benenne Reihen
  a = NULL
  for(r in 1:nrow(data)) {
    a <- c(a, paste0("Stichprobe ", r))
  }
  rownames(matrix) <- c(a, "Spaltensummen")
  
  # Benenne Spalten
  b = NULL  
  for(c in 1:(ncol(matrix)-1)) {
    b <- c(b, paste0("Merkmal ",c))
  }
  colnames(matrix) <- c(b, "Reihensummen")
  
  # Berechne die Punktwahrscheinlichkeit
  Punktwahrscheinlichkeit <- round(
    prod(factorial(c(col_sums, row_sums)))/
      prod(factorial(c(as.vector(data), n))),
    digits = 4)
  
  # Print Output
  writeLines(c("\n", "          Punktwahrscheinlichkeit fuer den exakten kx2-Test"))
  writeLines(c("\n", "Matrix:"))
  print(matrix)
  writeLines("\n")
  print(c("Punktwahrscheinlichkeit:" = Punktwahrscheinlichkeit))
}