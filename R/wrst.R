#' Wilcoxon Rangsummen-Test
#' 
#' Berechnet die Originalrang-Kombinationen und die exakte Verteilung im Wilcoxon Rangsummen-Test.
#' 
#' @param s1 Werte der Stichprobe 1
#' @param s2 Werte der Stichprobe 2
#' @return Originalrang-Kombinationen und Verteilungstabelle
#' @importFrom graphics hist
#' @importFrom utils combn
#' @export


wrst <- function(s1, s2) {
  
  # Erstelle Matrix mit Stichprobenzugehoerigkeit und Wert
  data <- matrix(c(rep(1, length(s1)), rep(2, length(s2)), s1, s2), ncol = 2)
  # Matrix mit Stichprobenzugehoerigkeit und Rang
  data_ranked <- matrix(c(data[,1], rank(data[,2])), ncol = 2)
  
  # Berechne R1
  a = NULL # Definiert a als leeres Obekt
  for(i in 1:length(s1)) {
    a <- sum(c(a,data_ranked[i,2]))} # Erstellt Summe aus den Raengen von s1
  s1_r <- a
  
  # Berechne (Teil-)Stichprobengroessen
  n <- nrow(data)
  s1 <- length(s1)
  s2 <- length(s2)
  s2_r <- n*(n+1)/2-a
  
  ## Entscheidet, ob Ties vorliegen.
  # Wenn nicht, wird der folgende Abschnitt ausgefuehrt.
  if(all.equal(sort(rank(data[,2])), c(1:length(data[,2])))== TRUE) {
    
    #### Exakte Verteilung im Fall ohne Ties
    # Erstelle alle moeglichen Rangkombinationen
    combinations <- combn(n,s1)
    combinations_output <- matrix(combinations, ncol = s1, byrow = TRUE)
    R1 <- colSums(combinations)
    combinations_output <- cbind(combinations_output, R1)
    
    r1_unique <- unique(sort(colSums(combinations))) # Aufgetretene Werte von R1
    r1_freq <- tabulate(sort(colSums(combinations))) # Haeufigkeit der Werte
    r1_freq <- r1_freq[r1_freq!=0] # Bereinigte Haeufigkeiten (Nullen weg)
    
    cat(c("     Exakte Verteilung im Wilcoxon Rangsummen-Test",
          "\n", "\n",
          "Originalrang-Kombinationen:",
          "\n"))
    print(combinations_output)
    
    
    
    
    # Beim Vorliegen von Ties wird der obige Abschnitt uebersprungen
    # Stattdessen wird der Folgende Abschnitt ausgefuehrt
  } else {
    
    # Erstelle die Kombinationen und formatiere den Output
    combinations <- combn(n,s1) # Erstelle die Kombinationen
    
    
    # Erstelle eine Matrix aus den Kombinationen
    combinations_output_original <- matrix(combinations, ncol = s1, byrow = TRUE) 
    
    #### Exakte Verteilung im Fall mit Ties
    replace <- sort(data_ranked[,2]) # Vektor der Midranks
    
    # Ersetze in den Kombinationen die Originalraenge mit den Midranks
    nt = NULL
    for (nt in 1:length(data[,2])) {
      combinations[combinations == nt] <- replace[nt]
      
      combinations_output_mr <- matrix(combinations, ncol = s1, byrow = TRUE)
    }
    
    # R1 und Frequenzen
    R1 <- colSums(combinations)
    
    # Erstelle Output
    combinations_output_midranks <- cbind(combinations_output_mr, R1)
    
    cat(c("     Exakte Verteilung im Wilcoxon Rangsummen-Test",
          "\n", "\n",
          "Midrank-Kombinationen:",
          "\n"))
    print(combinations_output_midranks)
    cat("\n")
    
    # Aufgetretene Werte von R1
    r1_unique <- unique(sort(colSums(combinations)))
    
    # Tabulate funktioniert nur fuer ganze Zahlen, 1.5. wuerde z.B. zu 1 zaehlen
    # Loesung: Histogramm erstellen ohne Plot, von kleinestem bis zum groessten Wert von R1
    # mit Breite von 0.5
    freq = hist(R1, breaks = seq(min(R1), max(R1), by = 0.5), include.lowest = T, plot = F)
    r1_freq <- freq$counts # aus dem Histogramm brauchen wir nur die Counts
    r1_freq <- r1_freq[r1_freq!=0] # Bereinigte Haeufigkeiten (Nullen weg)
    
  }
  
  # Anzahl Kombinationen
  n_comb <- ncol(combinations)
  
  # Punktwahrscheinlichkeiten
  point_p = NULL
  for (f in r1_freq) {
    point_p <- round(c(point_p, f/n_comb), 4)
  }
  
  # Kumulierte Anzahl linksseitig
  kum_l = NULL
  for (r in 1:length(r1_unique)) {
    kum_l <- c(kum_l, sum(kum_l[r-1],r1_freq[r]))}
  
  # Kumulierte Wahrscheinlichkeiten linksseitig
  kum_p_l = NULL
  for(i in kum_l) {
    kum_p_l <- c(kum_p_l, round(i/n_comb,4))}
  
  
  # Dieser Part ab hier ist nicht identisch mit oben,
  # da die Verteilung nicht symmetrisch ist.
    r1_rev_freq <- rev(r1_freq)
  
  # Kumulierte Anzahl rechtsseitig
  kum = NULL
  for (t in 1:length(r1_unique)) {
    kum <- c(kum, sum(kum[t-1],r1_rev_freq[t]))}
  
  kum_r <- rev(kum)
  
  # Kumulierte Wahrscheinlichkeiten rechtsseitig
  kum_p_r = NULL
  for(j in kum_r) {
    kum_p_r <- c(kum_p_r, round(j/n_comb,4))}  
  
  
  # Output-Tabelle
  result <- cbind(r1_unique, r1_freq, point_p, kum_l, kum_p_l, kum_r, kum_p_r)
  colnames(result) = c("RS1", "Anz.", "Punkws.", 
                       "Kum. l", "P(RS1 ≤ rs1)", 
                       "Kum r.", "P(RS1 ≥ rs1)")
  
  cat(c("\n",
        "Verteilungstabelle:",
        "\n", "\n"))
  print(result)
}
