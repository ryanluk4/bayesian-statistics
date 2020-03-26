# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### Aspirin

# data
y <- c( 2.77, 2.50, 1.84, 2.56, 2.32, -1.15 )
# variance
V <- c( 1.65, 1.31, 2.34, 1.67, 1.98, 0.90 )^2

aspirin.mortality.log.likelihood <- function( mu, sigma, y, V ) {
	ll <- ( - 1 / 2 ) * sum( log( V + sigma^2 ) + ( y - mu )^2 /
		( V + sigma^2 ) )
	return( ll )
} 

n.grid <- 50
mu.grid <- seq( 0.86 - 4 * 0.59, 0.86 + 4 * 0.59, length = n.grid )
sigma.grid <- seq( 0, 3 * sd( y ), length = n.grid )

ll.grid <- matrix( NA, n.grid, n.grid )

for ( i in 1:n.grid ) {
	for ( j in 1:n.grid ) {
		ll.grid[ i, j ] <- aspirin.mortality.log.likelihood( mu.grid[ i ], 
			sigma.grid[ j ], y, V )
	}
}

contour( mu.grid, sigma.grid, ll.grid, nlevels = 50,
	xlab = 'mu', ylab = 'sigma', main = 'Log Likelihood', col = 'red' )

persp( mu.grid, sigma.grid, ll.grid, xlab = 'mu', 
	ylab = 'sigma', zlab = 'Log Likelihood', theta = 30, phi = 30,
	col = 'blue' )

# exp of log-likelihood
contour( mu.grid, sigma.grid, exp( ll.grid ), nlevels = 50,
	xlab = 'mu', ylab = 'sigma', main = 'Likelihood', col = 'red' )

persp( mu.grid, sigma.grid, exp( ll.grid ), xlab = 'mu', 
	ylab = 'sigma', zlab = 'Likelihood', theta = 30, phi = 30, 
	col =  'blue' )

# all four plots
par( mfrow = c( 2, 2 ) )

contour( mu.grid, sigma.grid, ll.grid, nlevels = 50,
  xlab = 'mu', ylab = 'sigma', main = 'Log Likelihood', col = 'red' )

persp( mu.grid, sigma.grid, ll.grid, xlab = 'mu', 
  ylab = 'sigma', zlab = 'Log Likelihood', theta = 30, phi = 30,
  col = 'blue' )

contour( mu.grid, sigma.grid, exp( ll.grid ), nlevels = 50,
  xlab = 'mu', ylab = 'sigma', main = 'Likelihood', col = 'red' )

persp( mu.grid, sigma.grid, exp( ll.grid ), xlab = 'mu', 
  ylab = 'sigma', zlab = 'Likelihood', theta = 30, phi = 30,
  col = 'blue' )

par( mfrow = c( 1, 1 ) )