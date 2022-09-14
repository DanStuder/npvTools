#' Asymptotischer Wilcoxon-Rangsummentest
#' 
#' @param n Stichprobengrösse
#' @param ni n1 oder n2
#' @param ri RS1 oder RS2
#' @param t Tiekorrektur
#' @return Erwartungswert, Varianz und empirischer z-Wert
#' @export


asy_wrt <- function(n, ni, ri, t){
  erwRi <- ni*(n+1)/2
  varRi <- ni*(n+1)*(n-ni)*t/12
  z1 <- 1.645
  z2 <- 1.96
  
  zemp <- (ri-erwRi)/sqrt(varRi)
  zemp
  
  cat(c("\n", "     Asymptotischer Wilcoxon Rangsummen-Test", "\n", "\n",
        "Erwartungswert:", round(erwRi, digits = 4), "\n",
        "Varianz:", round(varRi, digits = 4),"\n",
        "z-emp:", round(zemp, digits = 4), "\n", "\n"))
  
  li1 <- floor(erwRi - z1*sqrt(varRi)) # Wert links einseitig
  re1 <- ceiling(erwRi + z1*sqrt(varRi)) # Wert rechts einseitig
  cat(c("Einseitige Werte:", "\n",
        "links:", li1, "\n",
        "rechts:", re1))# ACHTUNG APPROXIMATIV
  
  li2 <- floor(erwRi - z2*sqrt(varRi)) # Wert links zweiseitig
  re2 <- ceiling(erwRi + z2*sqrt(varRi)) # Wert rechts zweiseitig
  
  cat(c("\n", "\n", "Zweiseitige Werte:", "\n",
        "links:", li2, "\n",
        "rechts:", re2))# ACHTUNG APPROXIMATIV
}