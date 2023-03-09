

# create an example
cars = mtcars[,1]
cars = data.frame(ID = 1:32, 
                  wert = cars, 
                  gruppe = rep(c("g1", "g2", "g3", "g4"), 
                               c(7,9,7,9)))
cars




recast <- function(data, gruppe, wert){
  
  group_counts <- table(data[[gruppe]])
  max_count <- max(group_counts)
  
  
  # # create a dataframe with nrow = amount of groups
  # # and ncol = the highest number of values that was found for any of the groups
  # # and fill every field with NA
  out <-  matrix(data = NA,
                  nrow = length(group_counts),
                  ncol = max(group_counts))

  
  splits <- split(x = data[[wert]], 
                   f = data[[gruppe]]) 
  
  for (g in 1:length(group_counts)) {
    for (l in 1:group_counts[[g]]) {
      out[g, l] <- splits[[g]][l]  # in diesem Teil hier ist i
    }
  }
  
  return(out)
}


recast(data = cars, gruppe = "gruppe", wert = "wert")
