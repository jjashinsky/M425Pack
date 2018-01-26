#' Confidence Bands
#'
#' This function will use the lm function to create and add confidence bands to a previous graph.
#' With a given parameter it can also out put the interval of those bands at that parameter
#'
#'
#' @param lm object
#' @param xh=NULL
#' @param alpha level
#' @return graph of confidence bands or interval
#' @example confbands(cars.lm, xh=NULL, alpha=0.05)
#' @export
confbands <- function(lmObject, xh=NULL, alpha=0.05){

  # Use some fancy code to get the data out of the lmObject
  # while knowing which variable was x and which was y.
  thecall <- strsplit(as.character(lmObject$call[2]), "~")
  yname <- gsub(" ", "", thecall[[1]][1])
  xname <- gsub(" ", "", thecall[[1]][2])
  theData <- lmObject$model
  theData <- theData[,c(yname,xname)]
  colnames(theData) <- c("Y","X")

  # Begin creating confidence bands
  n <- nrow(theData)
  W2 <- 2*qf(1-alpha, 2, n-2)
  SSE <- sum( lmObject$res^2 )
  MSE <- SSE/(n-2)
  s2.Yhat.h <- function(xh){
    MSE*(1/n + (xh - mean(theData$X))^2/sum( (theData$X - mean(theData$X))^2 ))
  }
  b <- coef(lmObject)


  if (!is.null(xh)){
    tmp <- c(b[1]+b[2]*xh - W2*s2.Yhat.h(xh), b[1]+b[2]*xh + W2*s2.Yhat.h(xh))
    names(tmp) <- c("Lower","Upper")
    tmp
  } else{
    # Add upper bound to scatterplot
    curve(b[1]+b[2]*x + W2*s2.Yhat.h(x), add=TRUE)

    # Add lower bound to scatterplot
    curve((b[1]+b[2]*x) - W2*s2.Yhat.h(x), add=TRUE)
  }
}

