#' McNemar-Test
#' 
#' Funktion zur Berechnung des gerichteten exakten McNemar-Tests
#' 
#' @param data Matrix mit den beobachteten Werten
#' @param alternative Richtung der Hypothese
#' @return x, n, zweiseitiger p-Wert und Interpreation
#' @export


mcn_test <- function(data, alternative = c("less", "greater", "two.sided")) {
  dat <- matrix(data, nrow = 2, ncol = 2, byrow = TRUE) # mache Matrix aus Daten
  mcn_n <- dat[1,2] + dat[2,1] # Definiere n
  mcn_x <- dat[2,1] # Definiere x
  
  # Berechnung der Binomialtests
  left <- binom.test(mcn_x, mcn_n, 0.50, alternative = "less")
  right <- binom.test(mcn_x, mcn_n, 0.50, alternative = "greater")
  twosided <- binom.test(mcn_x, mcn_n, 0.50, alternative = "two.sided")
  
  # Sagt f?r das Argument "alternative", welcher Wert ausgegeben werden soll je nach Input
  pval <- switch(alternative,
                 less = left$p.value,
                 greater = right$p.value,
                 two.sided = twosided$p.value)
  
  # Beschreibt Hypothese je nach angegebener alternative
  hyp <- switch(alternative,
                less = "Linksseitiger p-Wert:",
                greater = "Rechtsseitiger p-Wert:",
                two.sided = "Zweiseitiger p-Wert:")
  
  # Interpretation
  int <- switch(alternative,
                less = if (pval <= 0.05) {
                  "Nullhypothese verwerfen, Pi(A) ist kleiner als Pi(B)"
                } else {
                  "Nullhypothese beibehalten, Pi(A) ist nicht kleiner als Pi(B)"
                },
                greater = if (pval <= 0.05) {
                  "Nullhypothese verwerfen, Pi(A) ist gr?sser als Pi(B)"
                } else {
                  "Nullhypothese beibehalten, Pi(A) ist nicht gr?sser als Pi(B)"
                },
                two.sided = if (pval <= 0.05) {
                  "Nullhypothese verwerfen, Pi(A) ist nicht gleich Pi(B)"
                } else {
                  "Nullhypothese beibehalten, Pi(A) ist gleich Pi(B)"
                })
  
  # Output
  writeLines(c("\n", 
               "         Exakter McNemar Test", 
               "\n",
               "Matrix:"))
  print(dat)
  writeLines("\n")
  cat("x =", mcn_x, "\n")
  cat("n =", mcn_n, "\n")
  cat(hyp, pval, "\n")
  writeLines(c("Interpretation:", int))
}