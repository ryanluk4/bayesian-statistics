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

# neyman-style frequentist analysis
print( lambda.hat.neyman <- mean( y ) )

# SE = population SD / sqrt( n ) = sqrt( population variance / n )
# the variance of poisson( lambda ) = lambda
print( se.hat.lambda.hat.neyman <- sqrt( lambda.hat.neyman / n ) )

# neyman assumes n = 14 is enough for repeated-sampling to
# 	follow the CLT
print( lambda.interval.neyman <- 
	c( lambda.hat.neyman - qnorm( 0.9995 ) *
	se.hat.lambda.hat.neyman, lambda.hat.neyman + 
	qnorm( 0.9995 ) * se.hat.lambda.hat.neyman ) )

# as long as the poisson sampling distribution is correct and the
# 	CLT applies to a sample size of n = 14, we are 99.9% confident

# the fisher information is the negative second partial derivative
# 	of the log-likelihood function with respect to lambda,
#	evaluated at lambda = lambda.hat.mle
# I.hat( lambda.hat.mle ) = n / lambda.hat.mle

# the repeated sampling variance is given by the inverse of the
# 	fisher information
# V.hat( lambda.hat.mle ) = ( I.hat( lambda.hat.mle ) )^( - 1 )
# 	= lambda.hat.mle / n

# the standard error mle and confidence interval are the same
print( se.hat.lambda.hat.mle <- sqrt( lambda.hat.mle / n ) )
print( lambda.interval.mle <- 
	c( lambda.hat.mle - qnorm( 0.9995 ) *
	se.hat.lambda.hat.mle, lambda.hat.mle + 
	qnorm( 0.9995 ) * se.hat.lambda.hat.mle ) )
