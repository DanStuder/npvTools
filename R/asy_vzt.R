#' Asymptotischer Vorzeichen-Rangtest
#' 
#' Einfache Funktion zur Berechnung des asymptotischen Vorzeichen-Rangtests
#' 
#' @param n Stichprobengroesse
#' @param alternative Richtung der Hypothese
#' @return Kritischer Wert
#' @export



asy_vzt <- function(n, alternative = c("less", "two.sided", "greater")) {
  
  ew <- n*0.5
  var <- n*0.25
  
  links <- switch(alternative,
                  less = floor(ew - (1.645*sqrt(var))),
                  two.sided = floor(ew - (1.96*sqrt(var))))
  
  rechts <- switch(alternative,
                   two.sided = ceiling(ew + (1.96*sqrt(var))),
                   greater = ceiling(ew + (1.645*sqrt(var))))
  
  output <- switch(alternative,
                   less = links,
                   two.sided = c(links, rechts),
                   greater = rechts)
  
  cat("\n", "     Asymptotischer vorzeichen-Test", "\n",
      "\n",
      "Kritischer Wert:", output)
  
}
