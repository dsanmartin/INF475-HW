# Exercise 1
f1 <- function(x) {
	y <- 5*(1+25*x^2)^(3/2)
}

# Exercise 2
f2 <- function(x) {
	y <- (1 - x)/x^5
}

# Exercise 3
f3 <- function(x) {
	y <- 2*exp(-(1/x-1)^2)/x^2
}

# Exercise 4
f4 <- function(x, y) {
	y <- exp((x + y)^2)
}

# Monte Carlo Integration for 1D
montecarlo = function(f, k) {
	X <- runif(k, 0, 1)
	int <- sum(f(X)) / k
} 

# Monte Carlo Integration for 2D
montecarlo2D <- function (f, k) {
	X <- runif(k, 0, 1)
	Y <- runif(k, 0, 1)
	int <- sum(f(X, Y)) / k
}

# Number of repetitions
k = 1000000

# Compute integrals
r1 <- montecarlo(f1, k)
r1

r2 <- montecarlo(f2, k)
r2

r3 <- montecarlo(f3, k)
r3

r4 <- montecarlo2D(f4, k)
r4