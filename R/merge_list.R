merge_list <- function(x, y){
  xout <- list()
  for (nn in intersect(names(x), names(y))){
    xdims <- dim(x[[nn]])
    ydims <- dim(y[[nn]])
    if (!is.null(xdims)){
      if (all(xdims[-length(xdims)] == ydims[-length(ydims)])){
        xout[[nn]] <- array(c(x[[nn]], y[[nn]]), c(xdims[-length(xdims)], xdims[length(xdims)] + ydims[length(ydims)]))
      } else {
        stop('Dimensions do not match')
      }      
    } else {
      xout[[nn]] <- c(x[[nn]], y[[nn]])
    }
  }
  return(xout)
}