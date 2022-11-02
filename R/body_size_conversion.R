
fx <- function(l){
  a * (l^b)
}


a <- exp(-2.1)
a <- 0.0403
b <- 3.1
l <- seq(0,40,.1)
df <- data.frame(l)
df$w <- fx(df$l)
plot(df$w ~ df$l)
