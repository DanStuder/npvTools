#' Effect Sizes for Cochrans Q
#' 
#' This function calculates the widely used effect size eta2, but also the less prevalent
#' but in many ways more adequate effect size R. See Berry et al. (2007) for a discussion: 
#' https://journals.sagepub.com/doi/10.2466/pms.104.4.1236-1242
#'
#' @param data Dataset in long format
#' @param formula Formula in the format "DV ~ grouping variable | block"
#' @param dep Dependent variable
#' @param samp Grouping variable
#' @param block Block (person variable / ID)
#' @param effsize Effect size of interest, i.e. eta2 or R
#'
#' @return Returns the effect size of interest (values between -1 and +1)
#' @importFrom dplyr filter mutate select
#' @importFrom coin mh_test statistic
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' # Dataset from Berry et al. (2007)
#' berry <- data.frame(subject = rep(1:6, each = 8),
#'                     treatment = rep(1:8, times = 6),
#'                     correct = c(0,1,1,1,0,0,1,0,
#'                                 1,1,1,0,0,1,1,1,
#'                                 0,1,0,1,1,0,1,1,
#'                                 1,1,1,1,0,1,1,1,
#'                                 0,1,1,0,0,0,1,1,
#'                                 1,1,1,1,0,1,1,0))
#' 
#' # Get the effect size R by specifing dep, samp and block 
#' cochrans_effsize(data = berry, 
#'                  dep = "correct", 
#'                  samp = "treatment", 
#'                  block = "subject", 
#'                  effsize = "R")
#' 
#' # Get the effect size R by providing a formula
#' formula = as.formula(correct ~ treatment | subject)
#' cochrans_effsize(data = berry, 
#'                  formula = formula, 
#'                  effsize = "R")
#' 
#' # Get the effect size eta2 by specifing dep, samp and block 
#' cochrans_effsize(data = berry, 
#'                  dep = "correct", 
#'                  samp = "treatment", 
#'                  block = "subject", 
#'                  effsize = "eta2")
#' 
#' # Get the effect size eta2 by providing a formula
#' cochrans_effsize(data = berry, 
#'                  formula = formula, 
#'                  effsize = "eta2")

cochrans_effsize <- function(data, 
                             formula = NULL,
                             dep = NULL,
                             samp = NULL, 
                             block = NULL, 
                             effsize = c("R", "eta2")) {
  
  if(is.null(formula) && any(is.null(dep), is.null(samp), is.null(block))) {
    stop("The call is ambiguous. Please provide either a formula or all of dep, samp and block.")
  }
  
  if(!is.null(formula)) {
    dep <- as.character(formula[[2]])
    samp <- strsplit(as.character(formula[[3]]), "|", fixed = TRUE)[[2]]
    block <- strsplit(as.character(formula[[3]]), "|", fixed = TRUE)[[3]]
    
  } 
  
  data <- data.frame(dep = factor(data[[dep]],
                                  labels = 0:1),
                     samp = factor(data[[samp]]),
                     block = factor(data[[block]]))
  
  formula <- as.formula(dep ~ samp | block)
  
  # Get Cochrans Q
  results <- coin::mh_test(formula = formula,
                           data = data,
                           distribution = "asymptotic")
  
  p <- length(unique(data$samp))
  n <- length(unique(data$block))
  
  # eta2
  if (effsize == "eta2") {
    out <- coin::statistic(results) / (n * p-1)
    
  }
  
  
  if (effsize == "R") {
    
    sum <- 0
    
    for (level in unique(data$samp)) {
      
      grp <- data |> 
        dplyr::filter(samp == level) |> 
        dplyr::select(dep) |> 
        dplyr::mutate(dep = as.character(dep),
                      dep = as.numeric(dep)) |> 
        unlist() |> 
        unname()
      
      for (j in 1:(length(grp)-1)) {
        for (l in (j + 1):length(grp)) {
          sum <- sum + abs(grp[j] - grp[l])
        }
      }
    }
    
    delta <- 1 / (p * choose(n, 2)) * sum
    
    pi <- 1 - results@statistic@expectation[1, ] 
    mu_delta <- 2 / (n * (n - 1)) * (sum(pi) * (n - sum(pi)) - sum(pi*(1-pi)))
    
    R <- 1 - delta / mu_delta
    out <- R
  }
  
  return(out)
  
}





