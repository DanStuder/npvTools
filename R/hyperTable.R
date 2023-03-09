#' hyperTable
#' 
#' Berechnet eine Tabelle der hypergeometrisch verteilten Wahrscheinlichkeiten basierend auf n1p, np1 und n
#'
#' @param n1p Grösse der Stichprobe 1
#' @param np1 Anzahl Personen mit Merkmal 1
#' @param n   Gesamte Stichprobengrösse
#' @return Hyperparametrische Verteilungstabelle
#' @importFrom stats dhyper fisher.test
#' @export
#' @examples
#' hyperTable(6, 6, 13)


hyperTable <- function(n1p, np1, n){
  
  # prevent output table from being in the scientific notation
  options(scipen = 99)
  
  # Get the other margins
  n2p = n - n1p
  np2 = n - n2p
  
  # Define a function to fill up the rest of the matrix based on n11 and the margins
  filler = function(n11, n1p, n2p, np1, np2){
    
    n12 = n1p - n11
    n21 = np1 - n11
    n22 = n2p - n21
    
    return(matrix(c(n11, n12, n21, n22), nrow = 2, byrow = T))
  }
  
  # Create an empty object to be filled in the for-loop
  valid = NULL
  
  # n11 can not be smaller than 0 or greater than n1p an np1, so that is where the loop starts and ends respectively.
  for(n11 in 0:min(n1p, np1)){
    
    # Pack Matrix
    fill = filler(n11, n1p, n2p, np1, np2) 
    
    # if any element of the table is lower than 0, the table is de facto not possible. 
    # if none is lower, the n11 is added to a vector of valid numbers,
    # else, it skips to the next number to see if it is valid.
    if(!any(fill < 0)) {
      
      valid = c(valid, n11)
      
    } else {next}
  }
  
  # Empty objects to be filled in the for-loop
  left = NULL
  center = NULL
  right = NULL
  two = NULL
  
  
  # for every valid value of n11, calculate the probabilities
  for(v in valid){
    
    # create table
    tab = filler(v,
                 np1 = np1,
                 np2 = np2,
                 n1p = n1p,
                 n2p = n2p)
    
    # Calculate point-probability
    center <- append(center, 
                     format(round(dhyper(x = v, 
                                         m = n1p, 
                                         n = n2p, 
                                         k = np2), 
                                  digits = 4), 
                            nsmall = 4))
    
    # Calculate left-sided probability
    left <- append(left, 
                   format(round(fisher.test(tab, 
                                            alternative = "less")$p.value, 
                                digits = 4),
                          nsmall = 4))
    
    # Calculate right-sided probability
    right <- append(right, 
                    format(round(fisher.test(tab, 
                                             alternative = "greater")$p.value, 
                                 digits = 4),
                           nsmall = 4))
    
    # Calculate two-sided probability
    two = append(two, 
                 format(round(fisher.test(tab, 
                                          alternative = "two.sided")$p.value, 
                              digits = 4),
                        nsmall = 4))
  }
  

  # Create output
  out = data.frame(valid,
                   left,
                   center,
                   right,
                   two)
  
  colnames(out) = c("n11", "P(X<=x)", "P(X=x)", "P(X>=x)", "P'")
  
  return(out)
}
