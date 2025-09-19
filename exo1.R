inverseF <- function(u) {
  if ((u>=0) && (u<=0.4)) return(0)
  else if ((u>0.4) && (u<=0.6)) return(5)
  else if ((u>0.6) && (u<=0.9)) return(6)
  else if ((u>0.9) && (u<=1)) return(7)
  else return(8)
}

# inverseF_vec <- Vectorize(inverseF)

# vectorized version of inverseF
inverseF_vec <- function(u) {
  x <- numeric(length(u))
  x[u <= 0.4] <- 0
  x[u > 0.4 & u <= 0.6] <- 5
  x[u > 0.6 & u <= 0.9] <- 6
  x[u > 0.9 & u <= 1] <- 7
  x[u > 1] <- 8
  return(x)
}

n <- as.integer(1e4)

u <- runif(n)

# x <- sample(5:8, size=n, replace=TRUE, prob=c(0.4, 0.2, 0.3, 0.1))

# x <- sapply(u, inverseF)

x <- inverseF_vec(u)

hist(u, breaks = seq(0, 1, by=0.1), freq=FALSE,
     xlab="u", ylab="f(x)", main="Histogramme de X")

barplot(table(x)/n, ylim=c(0,0.5),
        xlab="x", ylab="count", 
        col = "red", main="Histogramme de X")

################################################################################
lambda <- 2

x_sim <- -log(1-u)/lambda
hist(x_sim, breaks = 50, freq=FALSE, 
     col = "ivory", xlab="x", ylab="f(x)", 
     main="Histogramme de X simulé par la méthode de l'inversion")
curve(expr = dexp(x, rate = lambda), from = 0, to = 4, 
      col = "blue", lwd = 2, add = TRUE)

x_theo <- rexp(n = n, rate = lambda)
hist(x_theo, breaks = 50, freq=FALSE, 
     col = "lightblue", xlab="x", ylab="f(x)", 
     main="Histogramme de X simulé par rexp")
curve(expr = dexp(x, rate = lambda), from = 0, to = 4, 
      col = "red", lwd = 2, add = TRUE)

# qq plot of x_sim 
qqplot(x_sim, x_theo, 
       xlab = "Quantiles de X simulé par la méthode de l'inversion", 
       ylab = "Quantiles de X simulé par rexp", 
       main = "QQ plot de X simulé par la méthode de l'inversion et rexp"
       )
