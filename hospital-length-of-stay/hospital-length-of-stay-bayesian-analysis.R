# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Documents/Github/bayesian-statistics' )

##### Hospital length of stay (Bayesian analysis)

# bayes bread-and-butter theorem
# posterior PDF for lambda = c * prior PDF for lambda * likelihood for lambda
# the conjugate prior for a poisson distribution is a gamma distribution

# likelihood = constant * lambda^s * exp ( - n * lambda )
# likelihood = gamma( s + 1, n )

# for the length of stay, there is not much known about lambda external
# 	to the data set, so we want to choose alpha, beta in gamma( alpha, beta )
# 	to have a low information content
# the posterior mean is a weighted average of the prior mean and data mean
# 	the data mean gets 'n' votes and the prior mean gets 'beta' votes
#	so the prior sample size is beta

# beta = epsilon > 0, close to 0
# a conventional choice for a low information conjugate prior for the poisson
# 	likelihood is also taking alpha = epsilon and using gamma( epsilon, epsilon )

# say epsilon = 0.001
# the prior has a mean for lambda equal to epsilon / epsilon = 1
# 	with a weight of 0.001, compared to the likelihood data mean of
#	2.07 with sample size n = 14
epsilon <- 0.001
alpha <- epsilon
beta <- epsilon

lambda.grid.2 <- seq( 0, 4, length = 500 )
plot( lambda.grid.2, dgamma( lambda.grid.2, alpha, rate = beta ),
	type = 'l', lwd = 2, col = 'red', xlab = 'lambda',
	ylab = 'Density' )

# we can actually look at this low information content prior in the region
# 	that we are working in, so ( 0.8, 3.3 ) from the interval above,
#	the distribution is small and flat, so it is a good LI

# plotting all three distributions
lambda.grid.1 <- seq( 0.01, 6.3, length = 500 )

# likelihood distribution
plot( lambda.grid.1, dgamma( lambda.grid.1, s + 1, n ),
	type = 'l', lwd = 2, col = 'red', xlab = 'lambda',
	ylab = 'Density', main = 'cyan = prior, blue = posterior, red = likelihood' )

# posterior distribution
lines( lambda.grid.1, dgamma( lambda.grid.1, alpha + s, beta + n ),
	lty = 2, lwd = 2, col = 'blue' )

# low information conjugate prior
lines( lambda.grid.1, dgamma( lambda.grid.1, alpha, beta ),
	lty = 3, lwd = 2, col = 'darkcyan' )

# the posterior mean is ( alpha + s ) / ( beta + n )
#	which is the same as the lambda.hat.mle

# 99.9% bayesian confidence interval with LI prior
print( bayesian.interval <-
	c( qgamma( 0.0005, alpha + s, beta + n ),
	qgamma( 0.9995, alpha + s, beta + n ) ) )
