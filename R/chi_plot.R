#' Chi-Quadrat-Verteilung Plot
#' 
#' Plottet die Chi-Quadrat Verteilung
#' 
#' @param chi Empirisches Chi-Quadrat
#' @param df Freiheitsgrade
#' @param x Definiert Breite des Plots. Je höher df und chi, desto höher x
#' @param xlab Move the annotation on the x axis
#' @param ylab Move the annotation on the y axis
#' @importFrom ggplot2 ggplot aes geom_area scale_color_manual geom_vline geom_text labs theme_minimal theme annotate
#' @importFrom stats dchisq qchisq
#' @importFrom dplyr if_else
#' @export


# Based on http://rstudio-pubs-static.s3.amazonaws.com/511113_1df8cae98e264fe3ae677db9fd51150f.html

chi.plot <- function(chi, df, x, xlab = (x/15), ylab = -0.001) {
  
  # define some objects
  alpha = 0.05
  range = seq(0,x,by = 0.01)
  dist = dchisq(x = range,
                df = df)
  H0 = dplyr::if_else(range <= qchisq(p = 1 - alpha, 
                               df = df, 
                               lower.tail = TRUE),
               'Beibehalten', 'Verwerfen')
  
  # Create dataframe
  chi.dist <- data.frame(range = range, 
                         dist = dist,
                         H0 = H0)
  
  
  # Plotting sampling distribution and x_bar value with cutoff
  plot.test <- ggplot(data = chi.dist, 
                      aes(x = range,
                          y = dist)) +
    geom_area(aes(fill = H0)) +
    scale_color_manual(drop = TRUE, 
                       values = c('Beibehalten' = "Steelblue", 'Verwerfen' = "#703342"), 
                       aesthetics = 'fill') +
    geom_vline(xintercept = chi, 
               size = 1) +
    annotate("text",
             x = chi + xlab,
             y = 0 - ylab,
             label = paste0('Chi = ', round(chi,3))) +
    
    labs(x = 'Chi-Quadrat', 
         y = 'p') +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.title.y = element_text(angle = 0, vjust = 0.5))
  
  plot(plot.test)
}
