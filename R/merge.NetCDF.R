merge.NetCDF <- function(x,y){
  xdims <- dim(x)
  ydims <- dim(y)
  if (!all(xdims[-length(xdims)] == ydims[-length(ydims)])){
    stop('Dimensions do not match for merge')
  }
  xtime <- attr(x, 'time')
  ytime <- attr(y, 'time')
  atime <- sort(unique(c(xtime, ytime)))
  xout <- array(NA, c(prod(xdims[-length(xdims)]), length(atime)))
  xout[,atime %in% xtime] <- x
  xout[,atime %in% ytime] <- y
  xout <- array(xout, c(xdims[-length(xdims)], length(atime)))
  atns <- names(attributes(x))[-grep('dim', names(attributes(x)))]
  for (atn in setdiff(atns, 'time')) attr(xout, atn) <- attr(x, atn)
  attr(xout, 'time') <- atime
  return(xout)
}