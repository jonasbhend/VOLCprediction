annual_average <- function(x){
  xtime <- attr(x, 'time')
  xdims <- dim(x)
  nseas <- median(table(floor(xtime)))
  xout <- apply(x, seq(xdims)[-length(xdims)], tapply, floor(xtime), function(x) if (length(x) == nseas) mean(x) else NA)
  xout <- aperm(xout, c(seq(xdims)[-1], 1))
  atns <- names(attributes(x))[-grep('dim', names(attributes(x)))]
  for (atn in setdiff(atns, 'time')) attr(xout, atn) <- attr(x, atn)
  attr(xout, 'time') <- sort(unique(floor(xtime)))
  return(xout)
}