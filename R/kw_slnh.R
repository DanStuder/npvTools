#' Kruskal-Wallis-Test: Spezielle Lage-Nullhypothesen
#' 
#' Experimentelle Funktion zur Berechnung der speziellen Lage-Nullhypothesen des Kruskal-Wallis-Tests basierend auf Kontrasten.
#' 
#' @param data Matrix mit den beobachteten Werten, wobei jede Reihe eine Gruppe ist.
#' @param ... Platzhalter für Kontraste. Anzahl Kontraste muss = Stichprobengroesse sein.
#' @return Teststatistik L, Tiekorrektur, Erwartungswert, empirischer z-Wert und kritische L-Werte
#' @export
#' @examples
#' data <- matrix(c(5,8,7, 4,4,NA, 2,5,NA, 6,9,6), nrow = 4, ncol = 3, byrow = TRUE)
#' 
#' # Gruppengleichheitshypothese
#' kw_slnh(data, 1, -1, 0, 0)
#' 
#' # Lineare Interaktion
#' kw_slnh(data, 1, -1, -1, 1)



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

  tie_vector <- numb_uniq[numb_uniq != 0] # bereinigter Tie-Vektor fuer tatsaechlich vorkommende Werte
  tabb <- data.frame(unique_values, tie_vector) # grafische Darstellung

  # Berechnung der Tie-Korrektur
  TK <- 1-(sum((tie_vector^3)-tie_vector)/((n^3)-n))
  TK_round <- round(TK, digits = 4)
  
  # Berechnung der Teststatistik
  L <- sum(ci*Ri)
  
  # Berechnung der Varianz
  VL <- ((n+1)*TK/12)*(n*(sum(ni*(ci^2))) - ((sum(ni*ci))^2))
  VL_round <- round(VL, digits = 4)
  
  # Berechnung des Erwartungswerts
  EL <- (n+1)*sum(ni*ci)/2
  EL_round <- round(EL, digits = 4)
  
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
        "Tie-Korrektur", TK_round, "\n",
        "Varianz:", VL_round, "\n",
        "Erwartungswert:",EL_round, "\n",
        "z-Wert:", z, "\n",
        "Kritische L-Werte:", "\n",
        "     Zweiseitig:", lkrit.z, "\n",
        "     Einseitig:", lkrit.e))
}
