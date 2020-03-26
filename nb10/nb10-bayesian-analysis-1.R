# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Documents/Github/bayesian-statistics' )

##### NB10

# data
y <- c( 409., 400., 406., 399., 402., 406., 401., 403., 401., 403.,
        398., 403., 407., 402., 401., 399., 400., 401., 405., 402., 
        408., 399., 399., 402., 399., 397., 407., 401., 399., 401., 
        403., 400., 410., 401., 407., 423., 406., 406., 402., 405., 
        405., 409., 399., 402., 407., 406., 413., 409., 404., 402., 
        404., 406., 407., 405., 411., 410., 410., 410., 401., 402., 
        404., 405., 392., 407., 406., 404., 403., 408., 404., 407., 
        412., 406., 409., 400., 408., 404., 401., 404., 408., 406., 
        408., 406., 401., 412., 393., 437., 418., 415., 404., 401., 
        401., 407., 412., 375., 409., 406., 398., 406., 403., 404. 
)

n <- length( y )

hist( y, breaks = 20, prob = T, main = 'NB10 Case Study' )

# quantile plot
qqnorm( y, main = 'NB10 Case Study' )
# target normal distribution
abline( mean( y ), sd( y ), lwd = 2, col = 'red' )

sigma.0 <- sd( y )
mu.hat.mle <- mean( y )

# likelihood for a gaussian
gaussian.likelihood.sigma.known <- function( mu, mu.hat.mle, sigma.0, n ) {

	likelihood <- exp( - ( mu - mu.hat.mle )^2 / 
		( 2 * ( sigma.0^2 / n ) ) )
	return( likelihood )

}

se.mu.hat.mle <- sigma.0 / sqrt( n )

# empirical rule, most of probability should be within 3 standard errors
mu.grid <- seq( mu.hat.mle - 3 * se.mu.hat.mle, mu.hat.mle + 3 * se.mu.hat.mle, length = 500 )
plot( mu.grid, gaussian.likelihood.sigma.known( mu.grid,
	mu.hat.mle, sigma.0, n ), type = 'l', lwd = 2,
	xlab = 'mu', ylab = 'Gaussian likelihood (sigma known)',
	main = 'NB10 Case Study', col = 'darkcyan' )
abline( h = 0, lwd = 2 )
abline( v = mu.hat.mle, lwd = 2, lty = 3, col = 'red' )

# with the gaussian conjugate prior
# with center mu.0 and variance sigma.mu.squared
# posterior mean = ( ( sigma.0^2 / sigma.mu.squared ) * mu.0 + n * y.bar ) /
# 	( ( sigma.0^2 / sigma.mu.squared ) + n )
# variance = 1 / ( 1 / sigma.mu.squared + n / sigma.0^2 )
# precision = 1 / variance = 1 / sigma.mu.squared + n / sigma.0^2
# posterior precision = prior precision + likelihood precision
# prior sample size n.0 = sigma.0^2 / sigma.mu.squared

# if we want a low information content prior, we should choose a
# 	sigma.mu.squared big enough so that the prior sample size is small
# 	compared to n

# a large standard deviation (a large sigma.mu.squared) means that there
# 	is a lot of uncertainty about mu external to the data set = LI prior

sigma.0^2

# [1] 41.8201

# with n = 100, sigma.0 = 6.5, sigma.0^2 = 41.8
# any value of sigma.mu.squared bigger than 41.8 would give the prior less
# 	than 1 vote and the data would continue to get n = 100 votes

# gaussian with mu and sigma unknown
gaussian.likelihood.mu.and.sigma.unknown <- function( mu, sigma.squared, y ) {

	n <- length( y )
	likelihood <- sigma.squared^( - n / 2 ) * 
		exp( - sum( ( y - mu )^2 ) / ( 2 * sigma.squared ) )
	return( likelihood )

}

n.grid <- 500
mu.grid <- seq( mu.hat.mle - 3 * se.mu.hat.mle,
	mu.hat.mle + 3 * se.mu.hat.mle, length = n.grid )
sigma.squared.grid <- seq( 25, 65, length = n.grid )
likelihood.grid <- matrix( NA, n.grid, n.grid )

for ( i in 1:n.grid ) {
	for ( j in 1:n.grid ) {
		likelihood.grid[ i, j ] <- 
			gaussian.likelihood.mu.and.sigma.unknown( mu.grid[ i ], 
				sigma.squared.grid[ j ], y )
	}
}

# contour plot of mu and sigma.squared
contour( mu.grid, sigma.squared.grid, likelihood.grid, nlevels = 50, 
	col = 'red', xlab = 'mu', ylab = 'sigma.squared' )

dynamic.require <- function( package ) {
	if ( eval( parse( text = paste( 'require(', package, ')' ) ) ) ) {
		return( 'done' )
	}
	install.packages( package )
	return( eval( parse( text = paste( 'require(', package, ')' ) ) ) )
}

dynamic.require( 'plotly' )

likelihood.grid.t <- t( likelihood.grid )

# 3D interactive plot looking at gaussian with two unknowns
interactive.likelihood.perspective.plot <- 
	plot_ly( x = mu.grid, y = sigma.squared.grid, 
	z = likelihood.grid.t ) %>% 
	add_surface( contours = list( z = list( show = T, usecolormap = T,
	highlightcolor = '#ff0000', project = list( z = T ) ) ) ) %>%
	layout( title = 'Perspective plot of the bivariate density',
	scene = list( xaxis = list( title = 'x' ), 
	yaxis = list( title = 'y' ), zaxis = list( title = 'f' ),
	camera = list( eye = list( x = 1.5, y = 0.5, z = -0.5 ) ) ) ) %>% 
	colorbar( title = 'Bivariate Density' )

interactive.likelihood.perspective.plot
