# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### Dirichlet distribution sampling

rdirichlet <- function( m, alpha ) {

  k <- length( alpha )

  theta.star <- matrix( 0, m, k )

  for ( j in 1:k ) {

    theta.star[ , j ] <- rgamma( m, alpha[ j ], 1 )

  }

  theta.star <- theta.star / apply( theta.star, 1, sum )

  return( theta.star )

}

epsilon <- 0.01

print( alpha.prior <- rep( epsilon, 3 ) )

N <- c( 727, 583, 137 )

print( n <- sum( N ) )

print( alpha.posterior <- alpha.prior + N )

first.try.M <- 100000

seed <- 44

set.seed( seed )

system.time(

	mc.data.set.first.try <- rdirichlet( first.try.M, alpha.posterior )

)

str( mc.data.set.first.try )

mc.data.set.first.try[ 1:5, ]

table( apply( mc.data.set.first.try, 1, sum ) )

theta.1.star <- mc.data.set.first.try[ ,1 ]
theta.2.star <- mc.data.set.first.try[ ,2 ]
theta.3.star <- mc.data.set.first.try[ ,3 ]

gamma.star <- theta.1.star - theta.2.star

mc.data.set.first.try <- cbind( mc.data.set.first.try, gamma.star )

mc.data.set.first.try[ 1:5, ]

print( gamma.posterior.mean <- mean( gamma.star ) )

print( theta.hat.mle <- N/n )

print( gamma.hat.mle <- theta.hat.mle[ 1 ] - theta.hat.mle[ 2 ] )

print( theta.posterior.sd <- apply( mc.data.set.first.try, 2, sd ) )

print( theta.posterior.mean.mcse <- theta.posterior.sd / sqrt( first.try.M ) )

print( gamma.posterior.sd <- sd( gamma.star ) )

print( gamma.posterior.mean.mcse <- gamma.posterior.sd / sqrt( first.try.M ) )

print( M.required <- ( gamma.posterior.sd / 0.00005 )^2 )

final.try.M <- 250000

seed <- 4104

set.seed( seed )

system.time(

	mc.data.set.final.try <- rdirichlet( final.try.M, alpha.posterior )

)

theta.1.star <- mc.data.set.final.try[ ,1 ]
theta.2.star <- mc.data.set.final.try[ ,2 ]
theta.3.star <- mc.data.set.final.try[ ,3 ]

gamma.star <- ( theta.1.star - theta.2.star )

print( theta.posterior.mean <- apply( mc.data.set.final.try, 2, mean ) )

print( gamma.posterior.mean <- mean( gamma.star ) )

print( theta.posterior.mean.mcse <- theta.posterior.sd / sqrt( final.try.M ) )

print( gamma.posterior.mean.mcse <- gamma.posterior.sd / sqrt( final.try.M ) )

# score <- qnorm( 0.0005, gamma.posterior.mean, gamma.posterior.sd )

print( 1- pnorm( 0, gamma.posterior.mean, gamma.posterior.sd ) )

par( mar = c(3,3,3,3) )

par( mfrow = c(2,2) )

hist( theta.1.star, breaks = 100, prob = T, xlab = 'theta.1', ylab = 'Density', main = 'theta 1' )

hist( theta.2.star, breaks = 100, prob = T, xlab = 'theta.2', ylab = 'Density', main = 'theta 2' )

hist( theta.3.star, breaks = 100, prob = T, xlab = 'theta.3', ylab = 'Density', main = 'theta 3' )

hist( gamma.star, breaks = 100, prob = T, xlab = 'gamma', ylab = 'Density', main = 'gamma' )

par( mfrow = c(1,1) )
