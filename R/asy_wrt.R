#' Asymptotischer Wilcoxon-Rangsummentest
#' 
#' @param n Stichprobengrösse
#' @param ni n1 oder n2
#' @param ri RS1 oder RS2
#' @param t Tiekorrektur
#' @return Erwartungswert, Varianz und empirischer z-Wert
#' @export
#' @examples 
#' # Fall ohne Ties
#' asy_wrst(n = 7, ni = 3, ri = 11)


asy_wrst <- function(n, ni, ri, t = 1){
  erwRi <- ni*(n+1)/2
  varRi <- ni*(n+1)*(n-ni)*t/12
  z1 <- 1.645
  z2 <- 1.96
  
  zemp <- (ri-erwRi)/sqrt(varRi)
  zemp
  
  cat(c("\n", "     Asymptotischer Wilcoxon Rangsummen-Test", "\n", "\n",
        "Erwartungswert:", round(erwRi, digits = 4), "\n",
        "Varianz:", round(varRi, digits = 4), "\n",
        "z-emp:", round(zemp, digits = 4)))
  
  li1 <- round(erwRi - z1*sqrt(varRi), digits = 4) # Wert links einseitig
  re1 <- round(erwRi + z1*sqrt(varRi), digits = 4) # Wert rechts einseitig
  cat(c("\n", "\n", "Einseitige Werte:", "\n",
        "links:", li1, "\n",
        "rechts:", re1))# ACHTUNG APPROXIMATIV
  
  li2 <- round(erwRi - z2*sqrt(varRi), digits = 4) # Wert links zweiseitig
  re2 <- round(erwRi + z2*sqrt(varRi), digits = 4) # Wert rechts zweiseitig
  
  cat(c("\n", "\n", "Zweiseitige Werte:", "\n",
        "links:", li2, "\n",
        "rechts:", re2))# ACHTUNG APPROXIMATIV
}