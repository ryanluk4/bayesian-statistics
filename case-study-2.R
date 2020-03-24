# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Documents/Github/bayesian-statistics' )

##### AMI (heart attack) study

# sample size
n <- 403

# people who die
s <- 72

# bernoulii distribution likelihood
bernoulli.likelihood <- function( theta, n, s ) {

	likelihood <- theta^s * ( 1 - theta )^( n - s )
	return( likelihood )

}

# examples
bernoulli.likelihood( 0.2, n, s )
# very small
bernoulli.likelihood( 0.2, 10 * n, 10 * s )

# plotting whole likelihood function
theta.grid.1 <- seq( 0, 1, length = 500 )

plot( theta.grid.1, bernoulli.likelihood( theta.grid.1, n, s ),
	type = 'l', lwd = 2, col = 'red', xlab = 'theta',
	ylab = 'Bernoulli likelihood' )

# zooming in on theta = (0.1, 0.3)
theta.grid.2 <- seq( 0.1, 0.3, length = 500 )

plot( theta.grid.2, bernoulli.likelihood( theta.grid.2, n, s ),
  type = 'l', lwd = 2, col = 'red', xlab = 'theta',
  ylab = 'Bernoulli likelihood' )

# bernoulli distribution log-likelihood
bernoulli.log.likelihood <- function( theta, n, s ) {

	log.likelihood <- s * log( theta ) + ( n - s ) * log( 1 - theta )
	return( log.likelihood )

}

plot( theta.grid.1, bernoulli.log.likelihood( theta.grid.1, n, s ),
	type = 'l', lwd = 2, col = 'red', xlab = 'theta',
	ylab = 'Bernoulli log likelihood' )

plot( theta.grid.2, bernoulli.log.likelihood( theta.grid.2, n, s ),
	type = 'l', lwd = 2, col = 'red', xlab = 'theta',
	ylab = 'Bernoulli log likelihood' )

# MLE for bernoulli distribution
theta.hat.mle <- s / n

# plotting the log-likelihood n = 403
maximum.log.likelihood.for.n.of.403 <- 
	bernoulli.log.likelihood( theta.hat.mle, n, s )

plot( theta.grid.2, bernoulli.log.likelihood( theta.grid.2, n, s ) -
	maximum.log.likelihood.for.n.of.403, type = 'l', lwd = 2, 
	col = 'red', xlab = 'theta', ylab = 'Bernoulli log likelihood' )

text( 0.25, -1.0, 'n = 403', col = 'red' )

# plotting the log-likelihood n = 4030
maximum.log.likelihood.for.n.of.4030 <- 
	bernoulli.log.likelihood( theta.hat.mle, 10 * n, 10 * s )

lines( theta.grid.2, 
	bernoulli.log.likelihood( theta.grid.2, 10 * n, 10 * s ) -
	maximum.log.likelihood.for.n.of.4030, lwd = 2, 
	col = 'blue', lty = 2 )

text( 0.23, -13.0, 'n = 4030', col = 'blue' )

# as our information (sample size) increases, our uncertainty decreases
# this is shown by the narrower log-likelihood with n = 4030
# smaller variance/standard error