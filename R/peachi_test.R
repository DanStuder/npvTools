#' Pearsons Chi-Quadrat Test
#' 
#' Berechnet Pearsons Chi-Quadrat Test fuer unbestimmte Anzahl Kategorien
#' 
#' @param data Matrix mit den beobachteten Werten
#' @return n_p2, n_p•, Chi-Quadrat und df, kritischer Bereich, p-Wert und Interpretation
#' @export


peachi_test <- function(data) {
  # Titel
  cat(c("\n", "     Chi-Quadrat Test fuer unbestimmte Anzahl Kategorien", "\n", "\n"))
  # Berechne a und n
  n <- sum(data[ , ])
  a <- sum(data[ ,2])
  
  
  # Fuer jeden Wert i im Bereich von 1 bis Anzahl Zeilen, 
  # schreibe "ai=" und den Wert aus Zeile i Spalte 2
  for (i in 1:nrow(data)) {
    cat(c(paste0("a",i), "=", data[i, 2], "\n"))
  }
  
  
  # Schreibe a = und gib Wert a aus, mache zwei Zeilenumbrueche
  cat(c(" a =", a, "\n", "\n"))
  
  # Fuer jeden Wert k im Bereich von 1 bis Anzahl Zeilen, 
  # schreibe nk und die Summe aus den Werten der Zeile k
  for (k in 1:nrow(data)) {
    cat(c("n",k, "=", sum(data[k,]), "\n"))
  }
  
  # Schreibe n= n und mache zwei Zeilenumbrueche
  cat(c(" n =", n, "\n", "\n"))
  
  # Setze "total" auf 0, um dem Objekt einen Wert zu geben
  total = 0
  
  # Fuer jeden wert ax im Bereich von 1 bis Anzahl Zeilen,
  # addiere zum Wert "total" das Ergebnis aus dem Quadrat von a geteilt durch n
  for (ax in 1:nrow(data)) {
    total <- (total+(sum(data[ax,2]^2))/sum(data[ax,]))
  }
  
  
  chi <- n/(n-a)*(((n/a)*total)-a)            # Formel fuer das Chi-Quadrat
  chi_round <- round(chi, digits = 4)         # Runde Chi-Quadrat auf vier Nachkommastellen
  
  pc_df <- nrow(data)-1                       # Definiere die Freiheitsgrade als Anzahl Zeilen -1
  
  chi_krit <- qchisq(0.95, pc_df)             # Berechne den kritischen Chi-Quadrat-Wert
  chi_krit_round <- round(chi_krit, 4)        # Runde KB auf 4 Nachkommastellen
  
  pv <- round(pchisq(q = chi_round, df = pc_df, lower.tail = F), digits = 4)
  
  cat(c("Chi-Quadrat = ", chi_round,",", 
        "df = ", pc_df, "\n"))                # Schreibe den Output der Freiheitgrade
  cat(c("KB: Chi-Quadrat >=", chi_krit_round, "\n"))
  cat(c("p-Wert =", pv, "\n"))
  
  if (chi > chi_krit) {
    writeLines(c("Interpretation: Empirisches Chi-Quadrat liegt im kritischen Bereich. 
                Verwerfe die Nullhypothese zugunsten der Alternativhypothese."))
  } else {
    writeLines(c("Interpretation: Empirisches Chi-Quadrat liegt nicht im kritischen Bereich. 
                Die Nullhypothese wird beibehalten."))
  }
  
}