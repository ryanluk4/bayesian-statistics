# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### RJAGS MCMC

##### STEP 1 #####

# install JAGS from sourceforge
# type 'rjags sourceforge' and click on the link 'rjags - Sourceforge'
# download and install

##### STEP 2 #####

# create a new directory 

##### STEP 3 #####

# write and store the model file in the directory
# ex: aspirin-meta-analysis-model.txt

# model {
#   mu ~ dunif( -2, 5 )
#   sigma ~ dunif( 0, 6 )
#   for ( i in 1:k ) {
#     theta[ i ] ~ dnorm( mu, tau.theta )
#     y[ i ] ~ dnorm( theta[ i ], tau.y[ i ] )
#   }
#   tau.theta <- 1.0 / ( sigma * sigma )
#   positive.effect <- step( mu )
# }

##### STEP 4 #####

# change working directory to that directory
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics/aspirin' )

##### STEP 5 #####

# install the rjags package
install.packages( 'rjags' )
# load the rjags package
library( rjags )

##### STEP 6 #####

# aspirin meta analysis data
aspirin.meta.analysis.data <- list( k = 6, 
  y = c( 2.77, 2.50, 1.84, 2.56, 2.32, -1.15 ),
  tau.y = c( 0.3673, 0.5827, 0.1826, 0.3586, 0.2551, 1.235 ) )

##### STEP 7 #####

# initial values for Markov chain
aspirin.meta.analysis.initial.values <- list( mu = 1.45, sigma = 1.24 )

##### STEP 8 #####

# model
aspirin.meta.analysis.run.1 <- jags.model( 
  file = 'aspirin-meta-analysis-model.txt', 
  data = aspirin.meta.analysis.data, inits = 
  aspirin.meta.analysis.initial.values )

##### STEP 9 #####

set.seed( 123454321 )

##### STEP 10 #####

# burn-in
n.burnin <- 1000
update( aspirin.meta.analysis.run.1, n.iter = n.burnin )

##### STEP 11 #####

# sampling
n.monitor <- 100000
aspirin.meta.analysis.run.1.results <- jags.samples( 
  aspirin.meta.analysis.run.1, 
  variable.names = c( 'mu', 'sigma', 'theta', 'positive.effect' ), 
  n.iter = n.monitor )

##### STEP 12 #####

# summarizing the posterior
mu.star <- aspirin.meta.analysis.run.1.results$mu
sigma.star <- aspirin.meta.analysis.run.1.results$sigma
theta.star <- aspirin.meta.analysis.run.1.results$theta
positive.effect.star <- aspirin.meta.analysis.run.1.results$positive.effect

# (a) mu summary

par( mfrow = c( 2, 2 ) )
plot( 1:n.monitor, mu.star, type = 'l', xlab = 'Iteration Number',
  ylab = 'mu' )

# the upper left graph is a time-series plot of the monitored
# iterations (to visually check for stationarity)
plot( density( mu.star ), xlab = 'mu', main = '', lwd = 2, col = 'red' )

# the upper right graph is a density trace of the marginal posterior
# distribution for the quantity being monitored
acf( as.ts( mu.star[ 1:n.monitor ], start = 1, 
  end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )
pacf( as.ts( mu.star[ 1:n.monitor ], start = 1, 
  end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )
par( mfrow = c( 1, 1 ) )

# the lower left and right graphs are plots of the autocorrelation 
# function (ACF) and the partial autocorrelation function (PACF) 
# for the time series of monitored iterations; if these iterations
# behave like an autoregressive time series of order 1 with first-
# order autocorrelation rho (often denoted by AR1( rho )), then
# (a) the PACF will have a single spike of height rho at lag 1 and
# no other significant spikes and (b) the ACF will exhibit a
# geometric decay pattern with a spike of height rho at lag 1, a
# spike of approximate height rho^2 at lag 2, and so on -- for
# the parameter mu the ACF and PACF plots look perfectly like an
# AR1 with a first-order autocorrelation of about rho.1.hat = 0.6

# an MCMC time series with a rho.1.hat value close to 0 (which
# would be equivalent to IID sampling) is said to be "mixing well"

# (b) marginal posterior

c( mean( mu.star ), sd( mu.star ), quantile( mu.star, 
  probs = c( 0.025, 0.975 ) ) )

print( rho.1.hat.mu.star <- cor( mu.star[ 1:( n.monitor - 1 ) ],
  mu.star[ 2:n.monitor ] ) )

# the reason for making the ACF and PACF plots is that if the time
# series of monitored iterations for a given quantity behaves like
# an AR1( rho ), then the Monte Carlo standard error (MCSE) of the 
# mean of this series is given by the following expression:

print( se.mu.star.mean <- ( sd( mu.star ) / sqrt( n.monitor ) ) *
  sqrt( ( 1 + rho.1.hat.mu.star ) / ( 1 - rho.1.hat.mu.star ) ) )

# this is also the right order of magnitude for the MCSE of the
# MCMC estimate of the posterior SD and the quantiles giving the
# 95% interval for the monitored quantity

# sigma

par( mfrow = c( 2, 2 ) )

plot( 1:n.monitor, sigma.star, type = 'l', xlab = 'Iteration Number',
  ylab = 'sigma' )

plot( density( sigma.star ), xlab = 'sigma', main = '', lwd = 2, 
  col = 'red' )

acf( as.ts( sigma.star[ 1:n.monitor ], start = 1, 
  end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )

pacf( as.ts( sigma.star[ 1:n.monitor ], start = 1, 
  end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )

par( mfrow = c( 1, 1 ) )

# the marginal posterior for sigma has a bit of a long right-hand
# tail, as you would expect for a scale parameter; the ACF and PACF
# plots show that the sigma time series behaves more or less like an
# AR1 with a first-order autocorrelation of about 0.45 (so the
# MCMC output for this parameter is not mixing quite as well as
# the output for mu, but 0.45 is still a fairly low autocorrelation)

c( mean( sigma.star ), sd( sigma.star ), quantile( sigma.star, 
  probs = c( 0.025, 0.975 ) ) )

print( rho.1.hat.sigma.star <- cor( sigma.star[ 1:( n.monitor - 1 ) ],
  sigma.star[ 2:n.monitor ] ) )

print( se.sigma.star.mean <- ( sd( sigma.star ) / sqrt( n.monitor ) ) *
  sqrt( ( 1 + rho.1.hat.sigma.star ) / ( 1 - rho.1.hat.sigma.star ) ) )

# theta

str( aspirin.meta.analysis.run.1.results$theta )

theta.star.matrix <- matrix( aspirin.meta.analysis.run.1.results$theta,
  100000, 6, byrow = T )

print( theta.star.mean <- apply( theta.star.matrix, 2, mean ) )

print( theta.star.sd <- apply( theta.star.matrix, 2, sd ) )

par( mfrow = c( 3, 2 ) )

for ( i in 1:6 ) {

  plot( density( theta.star.matrix[ , i ] ), 
    xlab = paste( 'theta.', i, sep = '' ), main = '', lwd = 2,
    col = 'red' )
  
}

par( mfrow = c( 1, 1 ) )

# positive effect

# as j runs from 1 to 100000, positive.effect[ j ] is 1 if 
# mu.star[ j ] > 0 and 0 otherwise, so we can get a monte-carlo estimate
# of the posterior probability that low-dose aspirin would lower mortality
# in the population just by taking the mean of positive.effect:

print( posterior.probability.aspirin.is.beneficial <-
  mean( positive.effect.star ) )

