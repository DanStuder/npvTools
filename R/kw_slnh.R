#' Kruskal-Wallis-Test: Spezielle Lag-Nullhypothesen
#' 
#' Experimentelle Funktion zur Berechnung der speziellen Lage-Nullhypothesen des Kruskal-Wallis-Tests basierend auf Kontrasten.
#' 
#' @param data Matrix mit den beobachteten Werten
#' @param ... Platzhalter für Kontraste. Anzahl Kontraste muss = Stichprobengroesse sein.
#' @return Teststatistik L, Tiekorrektur, Erwartungswert, empirischer z-Wert und kritische L-Werte
#' @export


kw_slnh <- function(data, ...) {
  
  # Erstelle Midranks
  data_rank <- rank(data)
  
  # rank() ueberschreibt NA -> zurueckueberschreiben als NA
  data_rank[is.na(data)]<- NA
  
  # darstellen als Matrix
  data_clean <- matrix(data_rank, nrow = nrow(data), ncol = ncol(data), byrow = F)
  colnames(data_clean) <- 1:ncol(data_clean)
  
  # Rangsummenvektor fuer unterschiedliche Anzahl Gruppen
  Ri = NULL
  for(r in 1:nrow(data_clean)) {
    Ri <- c(Ri, sum(data_clean[r,], na.rm = T))}
  
  # Kontrast-Vektor fuer unterschiedliche Anzahl Gruppen
  ci <- c(...)
  
  # Berechnung der Gruppengroesse und Stichprobengroesse
  ni <- rowSums(!is.na(data_clean))
  n <- sum(ni)
  
  # Erstellung einer Matrix fuer den Output mit Midranks, Gruppengroesse, Rangsumme und Kontrasten
  data_output <- cbind(data_clean, ni, Ri, ci)
  
  #Ties
  sorted <- sort(data_clean) # Vektor der Werte erstellen und aufsteigend sortieren
  unique_values <- unique(sorted) # doppelte Werte raus
  numb_uniq <- tabulate(sorted) # tie-Vektor pro Wert

  tie_vector <- numb_uniq[numb_uniq != 0] # bereinigter Tie-Vektor f?r tats?chlich vorkommende Werte
  tabb <- data.frame(unique_values, tie_vector) # grafische Darstellung

  # Berechnung der Tie-Korrektur
  TK <- round(1-(sum((tie_vector^3)-tie_vector)/((n^3)-n)), digits = 4)
  
  # Berechnung der Teststatistik
  L <- sum(ci*Ri)
  
  # Berechnung der Varianz
  VL <- round(((n+1)*TK/12)*(n*(sum(ni*(ci^2))) - ((sum(ni*ci))^2)), digits = 4)
  
  
  EL <- round((n+1)*sum(ni*ci))/2
  # Berechnung des z-Werts
  z <- (L-EL)/sqrt(VL)
  z <- round(z, digits = 4)
  
  ## Kritische L-Werte
  # Zweiseitig
  lkrit.z <- c(round(EL-1.96*sqrt(VL), 4),
               round(EL+1.96*sqrt(VL), 4))
  
  # Einseitig
  lkrit.e <- c(round(EL-1.645*sqrt(VL), 4),
               round(EL+1.645*sqrt(VL), 4))
  
  
  # Output
  cat("     Kruskall-Wallis-Test - Spezielle Lagenullhypothesen", "\n")
  cat(c("\n", "Rang-Matrix mit Stichprobengroessen, Rangsumme und Kontrast", "\n", "\n"))
  print(data_output)
  cat(c("\n",
        "L:", L, "\n",
        "Tie-Korrektur", TK, "\n",
        "Varianz:", VL, "\n",
        "Erwartungswert:",EL, "\n",
        "z-Wert:", z, "\n",
        "Kritische L-Werte:", "\n",
        "     Zweiseitig:", lkrit.z, "\n",
        "     Einseitig:", lkrit.e))
}
