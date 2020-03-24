# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Documents/Github/bayesian-statistics/case-study-1' )

##### HIV Study

# identifying someone as HIV+ when they are HIV-
false.positive.rate <- function( alpha, beta, gamma ) {

	fpr <- ( 1 - alpha ) * ( 1 - gamma ) / 
		( alpha * beta + ( 1 - alpha ) * ( 1 - gamma ) )
	return( fpr )

}

# identifying someone as HIV- when they are HIV+
false.negative.rate <- function( alpha, beta, gamma ) {

	fnr <- alpha * ( 1 - beta ) / 
		( alpha * ( 1 - beta ) + gamma * ( 1 - alpha ) )
	return( fnr )

}

# different values of alpha [,1] column
# different FPR results [,2] column
cbind( c( 0.005, 0.01, 0.02 ), 
	c( false.positive.rate( 0.005, 0.999, 0.994 ), 
	false.positive.rate( 0.01, 0.999, 0.994 ), 
	false.positive.rate( 0.02, 0.999, 0.994 ) ) )

# percent change between alpha = 0.005 and 0.010
# FPR rate goes up by 46%
abs( ( 0.5444596 - 0.3728814 ) / 0.3728814 )

# percent change between alpha = 0.010 and 0.020
# FPR rate goes up by 39%
abs( ( 0.2273782 - 0.3728814 ) / 0.3728814 )

# different values of alpha
# different FNR results
cbind( c( 0.005, 0.01, 0.02 ), 
	c( false.negative.rate( 0.005, 0.999, 0.994 ), 
	false.negative.rate( 0.01, 0.999, 0.994 ), 
	false.negative.rate( 0.02, 0.999, 0.994 ) ) )

# we can see that the FNR is extremely low and not sensitive to prevalence


# plotting FPR as a function of varying alpha
# alpha ranges from 0 to 0.100
alpha.grid <- seq( 0, 0.1, length = 500 )

plot( alpha.grid, false.positive.rate( alpha.grid, 0.999, 0.994 ),
	type = 'l', xlab = 'alpha (prevalence)', ylab = 'false positive rate',
	lwd = 2, col = 'red', ylim = c( 0, 1 ), 
	main = '( beta, gamma ) = ( 0.999, 0.994 )' )

# plotting FNR as a function of varying alpha
plot( alpha.grid, false.negative.rate( alpha.grid, 0.999, 0.994 ),
	type = 'l', xlab = 'alpha (prevalence)', ylab = 'false negative rate',
	lwd = 2, col = 'red', main = '( beta, gamma ) = ( 0.999, 0.994 )' )

# combining plots
par( mfrow = c( 2, 1 ) )

plot( alpha.grid, false.positive.rate( alpha.grid, 0.999, 0.994 ),
	type = 'l', xlab = 'alpha (prevalence)', ylab = 'false positive rate',
	lwd = 2, col = 'red', ylim = c( 0, 1 ), 
	main = '( beta, gamma ) = ( 0.999, 0.994 )' )

plot( alpha.grid, false.negative.rate( alpha.grid, 0.999, 0.994 ),
	type = 'l', xlab = 'alpha (prevalence)', ylab = 'false negative rate',	
	lwd = 2, col = 'red', main = '( beta, gamma ) = ( 0.999, 0.994 )' )

par( mfrow = c( 1, 1 ) )
