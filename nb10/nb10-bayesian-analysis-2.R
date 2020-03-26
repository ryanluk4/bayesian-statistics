# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### NB10

# t-distributions
t.probability.plot <- function( nu, y, M, seed ) {

	set.seed( seed )
	n <- length( y )
	standardized.y <- ( y - mean( y ) ) / sd( y )

	plot( qt( ( ( 1:n ) - 0.5 ) / n, nu ), sort( y ), col = 'red', 
		xlab = 'Quantiles', ylab = 'sorted standardized y values', 
		main = paste( 'T Sampling Distribution (Degrees of Freedom): ', 
		nu, sep = '' ), lwd = 2, type = 'n', xlim = c( -6, 6 ), 
		ylim = c( -10, 10 ) )

	for ( m in 1:M ) {
		y.star <- rt( n, nu )
		lines( qt( ( ( 1:n ) - 0.5 ) / n, nu ), sort( y.star ),
			lwd = 1, col = 'gray70', lty = 3 )
	}

	abline( 0, 1, lwd = 2, col = 'black', lty = 2 )
	lines( qt( ( ( 1:n ) - 0.5 ) / n, nu ), sort( standardized.y ), 
		lwd = 3, col = 'red' )
	text( -2, 0.5, 'NB10', col = 'red' )
	lines( qt( ( ( 1:n ) - 0.5 ) / n, nu ), sort( rnorm( n ) ), 
		lwd = 4, col = 'darkcyan' )
	text( 5, 0.5, 'Normal', col = 'darkcyan' )

	return( 'Done!' )
}

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
# iterations
M <- 1000
seed <- 3412519
# degrees of freedom, t-distribution
nu <- 10

t.probability.plot( nu, y, M, seed )
