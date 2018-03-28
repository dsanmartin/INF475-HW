# Question 1 - Randon Number Generator

# Get CPU frequency for seed
CPU_seed <- function(N) {
	num <- vector(mode="numeric", length=N)

	for (i in 1:N) {
		cmd <- system("lscpu | grep 'CPU MHz'", intern = TRUE)
		freq <- as.double(sapply(strsplit(cmd, " "), tail, 1))
		num[i] <- as.integer(freq)
	}
	return (num)
}

# Linear congruential generator
RNG_cong <- function(N) {
	m <- 9
	x <- vector(mode="numeric", length=N)
	u <- vector(mode="numeric", length=N)

	x[1] <- 3
	x[2] <- 2
	x[3] <- 1

	u[1] <- x[1] / m
	u[2] <- x[2] / m
	u[3] <- x[3] / m

	a <- CPU_seed(3)

	for (n in 4:N) {
	 x[n] <- (a[1]*x[n-1] + a[2]*x[n-2] + a[3]*x[n-3]) %% m
	 u[n] <- x[n] / m
	}

	return(u)	
}

# Wichmann & Hill algorithm
RNG_WH <- function(N) {
	x <- vector(mode="numeric", length=N)
	y <- vector(mode="numeric", length=N)
	z <- vector(mode="numeric", length=N)
	u <- vector(mode="numeric", length=N)	

	seeds = CPU_seed(3) # Using CPU for seed only for GNU/Linux 
	#seeds = c(2379, 2379, 2578) # For other Operating Systems

	x[1] <- seeds[1] 
	y[1] <- seeds[2] 
	z[1] <- seeds[3] 

	u[1] <- (x[1]/30269 + y[1]/30307 + z[1]/30323) %% 1

	for (i in 2:N) {
		x[i] <- (171*x[i-1]) %% 30269
		y[i] <- (172*y[i-1]) %% 30307
		z[i] <- (170*z[i-1]) %% 30323

		u[i] <- (x[i]/30269 + y[i]/30307 + z[i]/30323) %% 1
	}
	return(u)
}

# Validation with chi-squared and K-S
validation <- function(sample) {
	# Test Chi-squared 
	intervals <- seq(0, 1, length.out=10)
	sample.counts <- hist(sample, breaks=intervals, plot=F)$counts
	chsq <- chisq.test(rbind(sample.counts, mean(sample.counts)))

	# Test de Kolmogorov - Smirnov
	ks <- ks.test(sample, 'punif')

	print(ks)
	print(chsq)
}

# Quality of RNG
N <- 10000000
#X <- RNG_WH(N)
#validation(X)


# Question 2

# Exercise 1
f1 <- function(x) {
	y <- 5*(1+25*x^2)^(3/2)
}

# Exercise 2
f2 <- function(x) {
	y <- (1/x - 1) / (x*(1+(1/x-1)^2))^2
}

# Exercise 3
f3 <- function(x) {
	y <- 2*exp(-(1/x-1)^2)/x^2
}

# Exercise 4
f4 <- function(x, y) {
	z <- exp((x + y)^2)
}

# Monte Carlo Integration for 1D
montecarlo = function(f, k) {
	#X <- runif(k, 0, 1)
	X <- RNG_WH(k)
	int <- sum(f(X)) / k
} 

# Monte Carlo Integration for 2D
montecarlo2D <- function (f, k) {
	#X <- runif(k, 0, 1)
	#Y <- runif(k, 0, 1)

	# Using CPU seed
	X <- RNG_WH(k)
	Y <- RNG_WH(k)

	# Not using CPU seed
	#XY = RNG_WH(2*k) 
	#X <- XY
	#X <- head(XY, k)
	#Y <- tail(XY, k)

	int <- sum(f(X, Y)) / k
}

# Number of repetitions
k = 1000000

# Compute integrals
#r1 <- montecarlo(f1, k)
#r2 <- montecarlo(f2, k)
#r3 <- montecarlo(f3, k)
r4 <- montecarlo2D(f4, k)

# Print results
#cat(sprintf("Integral 1: %f \n", r1))
#cat(sprintf("Integral 2: %f \n", r2))
#cat(sprintf("Integral 3: %f \n", r3))
cat(sprintf("Integral 4: %f \n", r4))