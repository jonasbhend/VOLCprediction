seas <- c("DJF","MAM","JJA","SON")

# set parameters
# length of prediction interval
n       <- 10
# threshold for above percentile stats
qlev    <- 0.90
# bootstrap repetitions
nb      <- 1000
#years without eruptions for composite
before  <- 8
after   <- 10
t.ave   <- 3
clim    <- 30
strtyr  <- 1900

# recovery
k       <- 6

# skill
skill.init  <- seq(0,0.99,0.01)
col1        <- c("darkgreen", "darkblue", "darkred")
col2        <- c("lightgreen", "lightblue", "salmon")
quants      <- c(0.66,0.9,0.95)
