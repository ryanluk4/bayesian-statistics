# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Documents/Github/bayesian-statistics' )

##### Beta distributions

# betas are nice
theta.grid <- seq( 0, 1, length = 500 )

plot( theta.grid, dbeta( theta.grid, 1, 1 ), type = 'l',
	lwd = 2, col = 'black', xlab = 'theta', ylab = 'density',
	ylim = c( 0, 5 ) )

lines( theta.grid, dbeta( theta.grid, 2, 1 ), lwd = 2,
	lty = 2, col = 'red' )

lines( theta.grid, dbeta( theta.grid, 0.1, 0.1 ), lwd = 2,
	lty = 2, col = 'orange')

lines( theta.grid, dbeta( theta.grid, 0.1, 1 ), lwd = 2,
	lty = 3, col = 'blue' )

lines( theta.grid, dbeta( theta.grid, 2, 2 ), lwd = 2,
	lty = 4, col = 'darkcyan' )

lines( theta.grid, dbeta( theta.grid, 3, 2 ), lwd = 2,
	lty = 5, col = 'chocolate4' )

lines( theta.grid, dbeta( theta.grid, 4, 4 ), lwd = 2,
	lty = 6, col = 'blueviolet' )

lines( theta.grid, dbeta( theta.grid, 10, 10 ), lwd = 2,
	lty = 7, col = 'darkorchid' )

lines( theta.grid, dbeta( theta.grid, 10, 20 ), lwd = 2,
	lty = 8, col = 'turquoise4' )

text( 0.55, 3.8, '( 10, 10 )', cex = 1.25, col = 'darkorchid' )

text( 0.1, 4.5, '( 0.1, 1 )', cex = 1.0, col = 'blue' )

text( 0.9, 4.5, '( 0.1, 0.1 )', cex = 1.0, col = 'orange' )
