  # Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### Aspirin

y <- c( 2.77, 2.50, 1.84, 2.56, 2.32, -1.15 )
V <- c( 1.65, 1.31, 2.34, 1.67, 1.98, 0.90 )^2

aspirin.mortality.log.likelihood.for.optim <- function( eta, y, V ) {
	mu <- eta[ 1 ]
	sigma <- eta[ 2 ]
	ll <- ( - 1 / 2 ) * sum( log( V + sigma^2 ) + ( y - mu )^2 /
		( V + sigma^2 ) )
	return( ll )
} 

eta.initial.values <- c( 1.5, 1.25 )

print( ml.results.1 <- optim( eta.initial.values, 
	aspirin.mortality.log.likelihood.for.optim, y = y, V = V, 
	hessian = T, control = list( fnscale = -1 ) ) )

print( maximum.likelihood.covariance.matrix <- 
	solve( - ml.results.1$hessian ) )

print( maximum.likelihood.estimated.standard.errors <-
	diag( maximum.likelihood.covariance.matrix ) )