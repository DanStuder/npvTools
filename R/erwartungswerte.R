#' Erwartungswerte
#' 
#' Berechnet die Erwartungswerte im Chi-Quadrat-Test
#' 
#' @param data Matrix mit den beobachteten Werten
#' @return Matrix mit den erwarteten Werten
#' @importFrom stats chisq.test
#' @export


erwartungswerte <- function(data) {
  chisq <- chisq.test(data)
  exp <- chisq$expected
  matrix <- matrix(round(exp, digits = 4), nrow = nrow(exp), ncol = ncol(exp), byrow = F)
  rownames(matrix) <- c("Stichprobe 1", "Stichprobe 2")
  print(matrix)
}
