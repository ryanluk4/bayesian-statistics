# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### Roulette

# 18 red slots on a roulette wheel
print( population.data.set <- c( rep( 0,20 ), rep( 1,18 ) ) )

# plot the population probability mass function (PMF) - discrete
plot( 0, 0, type = 'n', xlim = c( -0.5, 1.5 ),
  xlab = '1 if red, 0 if not', main = 'Population PMF: red or not',
  ylab = 'PMF value', ylim = c( 0, 0.55 ) )

# probabilities
segments( 0, 0, 0, 20 / 38, lwd = 2, col = 'red' )

segments( 1, 0, 1, 18 / 38, lwd = 2, col = 'red' )

# theta is the population mean
print( theta <- mean( population.data.set ) )

# sigma is the population standard deviation = sqrt( theta * ( 1 - theta ) )
print( sigma <- sqrt( mean( ( population.data.set - theta )^2 ) ) )
sqrt( theta * ( 1 - theta ) )

# repeated-sampling data set
n <- 10
M <- 100000
seed <- 4391557
set.seed( seed )
repeated.sampling.data.set <- rep( NA, M )

for( m in 1:M ) { 

	simulated.sample.data.set <- sample( population.data.set, n, replace = T )
	repeated.sampling.data.set[ m ] <- mean ( simulated.sample.data.set )

}

# mean from repeated sampling (100,000 iterations)
print( length( repeated.sampling.data.set ) )
print( theta.hat <- mean( repeated.sampling.data.set ) )

M <- 1000000

for( m in 1:M ) { 

	simulated.sample.data.set <- sample( population.data.set, n, replace = T )
	repeated.sampling.data.set[ m ] <- mean ( simulated.sample.data.set )

}

# mean from repeated sampling (1,000,000 iterations)
print( length( repeated.sampling.data.set ) )
print( expected.value.theta.hat <- mean( repeated.sampling.data.set ) )

# standard error from repeated sampling
# long-run standard devitation (SE) is our regular SD divided by n
print( standard.error.theta.hat <- sqrt( theta * ( 1 - theta ) / n ) )
sd( repeated.sampling.data.set )

# frequency table of theta.hat values
table( repeated.sampling.data.set ) / M

# uncertainty with a sample size of only 10
# repeated sampling PMF, probability from '$prob = T'
# almost looks like a normal curve due to the Central Limit Theorem (CLT)
hist( repeated.sampling.data.set, prob = T, main = 'n = 10',
  breaks = 100, xlab = 'theta.hat' )

theta.hat.grid <- seq( 0, 1, length = 500 )

lines( theta.hat.grid, dnorm( theta.hat.grid, expected.value.theta.hat,
  standard.error.theta.hat ), lwd = 2, col = 'red' )

# using less bins
hist( repeated.sampling.data.set, prob = T, main = 'n = 10',
  breaks = 12, xlab = 'theta.hat' )

lines( theta.hat.grid, dnorm( theta.hat.grid, expected.value.theta.hat,
  standard.error.theta.hat ), lwd = 2, col = 'red' )

# simulation approximation
# P( | theta.hat - theta | <= epsilon )
# ex: epsilon = 0.01
# we want a high probability, i.e. theta.hat matches theta
epsilon <- 0.01
mean( abs( repeated.sampling.data.set - theta ) <= epsilon )

# sample size n = 10 sucks
pnorm( epsilon / standard.error.theta.hat ) - 
  pnorm( - epsilon / standard.error.theta.hat )

# using n = 100
n <- 100
seed <- 6350823
set.seed( seed )
repeated.sampling.data.set <- rep( NA, M )

for ( m in 1:M ) {

    simulated.sample.data.set <- sample( population.data.set, n, replace = T )
    repeated.sampling.data.set[ m ] <- mean( simulated.sample.data.set )

}

print( expected.value.theta.hat <- mean( repeated.sampling.data.set ) )
print( standard.error.theta.hat <- sd( repeated.sampling.data.set ) )
sqrt( theta * ( 1 - theta ) / n )

table( repeated.sampling.data.set ) / M

# the CLT will be more apparent with n = 100
hist( repeated.sampling.data.set, prob = T, main = 'n = 100',
  breaks = 1000, xlab = 'theta.hat' )

lines( theta.hat.grid, dnorm( theta.hat.grid, expected.value.theta.hat,
  standard.error.theta.hat ), lwd = 2, col = 'red' )

# less bins
hist( repeated.sampling.data.set, prob = T, main = 'n = 100',
  breaks = 50, xlab = 'theta.hat' )

lines( theta.hat.grid, dnorm( theta.hat.grid, expected.value.theta.hat,
  standard.error.theta.hat ), lwd = 2, col = 'red' )

# this should match our pnorm normal approximation above
mean( abs( repeated.sampling.data.set - theta ) <= epsilon ) 
pnorm( epsilon / standard.error.theta.hat ) - 
  pnorm( - epsilon / standard.error.theta.hat )
