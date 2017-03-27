library(reshape2)
options(digits=2)
test.f <- function(x, t, w_number = 250) {
  z <- exp(-(x-800)^2/(2*40^2))*cos(2*pi*100*t*299792458*w_number) * (
    as.numeric(t>0 || t<0)*exp(-(t)^2/(2*75E-15^2)) +
      as.numeric(t > 0)*exp(-t/1.0E-12)
  )
  z <- z + rnorm(n = 1, sd = 0.1)
  return(z)
}

x <- seq(from = 600, to = 1000, length.out = 400)
t <- seq(from = 0, to = 2.5, by = 0.05) * 1E-12
df <- expand.grid(x, t)
colnames(df) <- c("x", "t")
df$z <- apply(df, 1, function(x) {test.f(x[1], x[2])})
df$t <- df$t * 1E12

output.matrix <- acast(df, t~x, value.var = "z")
write.csv(format(output.matrix, digits = 5), quote = FALSE, file = "data/simulated.csv")
