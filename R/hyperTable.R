#' hyperTable
#' 
#' Berechnet eine Tabelle der hypergeometrisch verteilten Wahrscheinlichkeiten für x, für gegebene Randsummen.
#'
#' @param n11 
#' @param n1p 
#' @param n2p 
#' @param np1 
#' @param np2 
#'
#' @return
#' @export
#'
#' @examples


filler = function(n11, n1p, n2p, np1, np2){
  
  n12 = n1p - n11
  n21 = np1 - n11
  n22 = n2p - n21
  
  return(matrix(c(n11, n12, n21, n22), nrow = 2, byrow = T))
}



selector <- function(n1p, n2p, np1, np2){
  
  # Creat an empty object to be filled in the for-loop
  valid = NULL
  
  
  # n11 can not be smaller than 0 or greater than n1p an np1, so that is where the loop starts and ends respectively.
  for(n11 in 0:min(n1p, np1)){
    
    # gets the whole table from the margins and the element n11
    fill = filler(n11, n1p, n2p, np1, np2) 
    
    # if any element of the table is lower than 0, the table is de facto not possible. 
    # if none is lower, the n11 is added to a vector of valid numbers,
    # else, it skips to the next number to see if it is valid.
    if(!any(fill < 0)) {
      
      valid = c(valid, n11)
      
    } else {next}
  }
  
  return(valid)
  
}



n1p = 4
n2p = 6
np1 = 5
np2 = 5

hyperTable <- function(n1p, n2p, np1, np2){
  
  # prevent output table from being in the scientific notation
  options(scipen = 99)
  
  # get the valid values for n11
  valid = selector(n1p, n2p, np1, np2)
  
  # for every valid value of n11, calculate the left-sided cumulative probabilities
  left = NULL
  for(l in valid){
    left <- append(left, 
                   format(round(phyper(q = l, 
                                       m = n1p, 
                                       n = n2p, 
                                       k = np2,
                                       lower.tail = TRUE), 
                                digits = 4), 
                          nsmall = 4)
    )
  }
  
  # for every valid value of n11, calculate the point probabilities
  center = NULL
  for(c in valid){
    center <- append(center, 
                     format(round(dhyper(x = c, 
                                         m = n1p, 
                                         n = n2p, 
                                         k = np2), 
                                  digits = 4), 
                            nsmall = 4)
    )
  }
  
  # for every valid value of n11, calculate the right-sided cumulative probabilities
  right = NULL
  for(r in rev(valid)){
    right <- append(right, 
                    format(round(phyper(q = r, 
                                        m = n1p, 
                                        n = n2p, 
                                        k = np2,
                                        lower.tail = TRUE), 
                                 digits = 4), 
                           nsmall = 4)
    )
  }
  
  
  out = data.frame(valid,
                   left,
                   center,
                   right)
  
  colnames(out) = c("x", "X<=x", "X=x", "X>=x")
  rownames(out) = NULL
  
  return(out)
}


hyperTable(6,7,6,7)
