#' Create weight for imbalanced data.
#' Snipped from https://stats.stackexchange.com/questions/357407/glmnet-weights-and-imbalanced-data
#' 
#' @parameter ADS A data.frame-class object.
#' @parameter var.y A character string of a variable name.
#' @parameter y.class A numeric vector of length=2 indicates levels in the variable var.y.
#' 
#' @export 
#' 

mf.create_weight <-
  function(
    ADS, 
    var.y,
    outname="weight",
    y.class=c(0,1)
    ){
    if(
      outname %in%
      colnames(ADS)
      ) stop("Change 'outname'")
    
    fraction_0 <-
      rep(
        1 -
          sum(as.numeric(ADS[,var.y]) == y.class[1]) /
          length(as.numeric(ADS[,var.y])), 
        sum(as.numeric(ADS[,var.y]) == y.class[1])
        )
    
    fraction_1 <-
      rep(
        1 - 
          sum(as.numeric(ADS[,var.y]) == y.class[2]) / 
          length(as.numeric(ADS[,var.y])),
        sum(as.numeric(ADS[,var.y]) == y.class[2])
        )
    
    # assign that value to a "weights" vector
    weights <- nrow(ADS)
    # if (weighted == TRUE) {
      weights[as.numeric(ADS[,var.y]) == y.class[1]] <- 
        fraction_0
      
      weights[as.numeric(ADS[,var.y]) == y.class[2]] <-
        fraction_1
      
      # } else {
      #   weights <- rep(1, nrow(ADS[,var.y]))
      # }
    ADS[,outname] <- weights
    return(ADS)
    }
