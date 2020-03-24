# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Documents/Github/bayesian-statistics' )

##### Hospital length of stay

# data in days
y <- c( 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 6 )
hist( y, breaks = ( -0.5 ):6.5, prob = T )

# poisson distribution likelihood
poisson.likelihood <- function( lambda, n, s ) {

	likelihood <- lambda^s * exp( - n * lambda )
	return( likelihood )

}

# poisson distribution log-likelihood
poisson.log.likelihood <- function( lambda, n, s ) {

	log.likelihood <- s * log( lambda ) - n * lambda
	return( log.likelihood )

}

n <- length( y )
s <- sum( y )

lambda.grid <- seq( 0.5, 4, length = 500 )

plot( lambda.grid, poisson.likelihood( lambda.grid, n, s ),
	type = 'l', lwd = 2, col = 'red', xlab = 'lambda',
	ylab = 'likelihood', main = 'Poisson sampling model' )

plot( lambda.grid, poisson.log.likelihood( lambda.grid, n, s ),
	type = 'l', lwd = 2, col = 'red', xlab = 'lambda',
	ylab = 'log likelihood', main = 'Poisson sampling model' )

# MLE (mean) and SE, both "hat" estimates
print( lambda.hat.mle <- s / n )
print( se.hat.lambda.hat.mle <- sqrt( lambda.hat.mle / n ) )

# 99.9% confidence interval
print( lambda.interval <- c( lambda.hat.mle - qnorm( 0.9995 ) *
	se.hat.lambda.hat.mle, lambda.hat.mle + qnorm( 0.9995 ) *
	se.hat.lambda.hat.mle ) )

# we are 99.9% confident that the population mean length of stay
# is between about 0.8 days and 3.3 days