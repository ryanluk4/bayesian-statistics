# Ryan Luk
# STAT 206
# Credit: Prof. David Draper (draper), UCSC 2020
# I made slight modifications / cleaned up

# unbuffer the output (ctrl + w)
# setwd( 'C:/Users/Ryan Luk/Github/bayesian-statistics' )

##### Aspirin

y <- c( 2.77, 2.50, 1.84, 2.56, 2.32, -1.15 )
V <- c( 1.65, 1.31, 2.34, 1.67, 1.98, 0.90 )^2
epsilon <- 1e-7

n.T <- c( 615, 758, 317, 832, 810, 2267 )
n.C <- c( 624, 771, 309, 850, 406, 2257 )
n <- n.C + n.T

# this function starts at sigma.hat = 0
# continues until two successive values of sigma.hat
# 	differs by no more than epsilon
# n = vector of (treatment + control) for each k = 6 studies
aspirin.case.study.empirical.bayes.calculations <- 
  function( y, V, epsilon, n ) {

  sigma.squared.hat.old <- 0

  W.hat.old <- 1 / ( V + sigma.squared.hat.old )

  mu.hat.old <- sum( W.hat.old * y ) / sum( W.hat.old )

  sigma.squared.hat.new <- sum( W.hat.old^2 * 
    ( ( y - mu.hat.old )^2 - V ) ) / sum( W.hat.old^2 )

  m <- 0

  while ( abs( sigma.squared.hat.new - 
    sigma.squared.hat.old ) > epsilon ) {

    sigma.squared.hat.old <- sigma.squared.hat.new

    W.hat.new <- 1 / ( V + sigma.squared.hat.new )

    mu.hat.new <- sum( W.hat.new * y ) / sum( W.hat.new )

    sigma.squared.hat.new <- sum( W.hat.new^2 * 
      ( ( y - mu.hat.new )^2 - V ) ) / sum( W.hat.new^2 )

    m <- m + 1

  }

  mu.hat <- mu.hat.new

  sigma.squared.hat <- sigma.squared.hat.new

  sigma.hat <- sqrt( sigma.squared.hat )

  W.hat <- W.hat.new

  W.hat.normalized <- W.hat / sum( W.hat )

  n.normalized <- n / sum( n )

  B.hat <- V / ( V + sigma.squared.hat )

  theta.hat <- ( 1 - B.hat ) * y + B.hat * mu.hat

  se.hat.mu.hat <- 1.0 / sqrt( sum( 1 / ( V + sigma.squared.hat ) ) )

  se.hat.theta.hat <- sqrt( V * ( 1 - B.hat ) )

  mle.results <- list( m = m, mu.hat = mu.hat, 
    se.hat.mu.hat = se.hat.mu.hat, sigma.squared.hat = 
    sigma.squared.hat, sigma.hat = sigma.hat, n = n, 
    n.normalized = n.normalized, W.hat = W.hat, 
    W.hat.normalized = W.hat.normalized,
    B.hat = B.hat, y = y, theta.hat = theta.hat, 
    se.hat.theta.hat = se.hat.theta.hat )
    
  return( mle.results )

}

print( empirical.bayes.results <- 
  aspirin.case.study.empirical.bayes.calculations( y, V, epsilon, n ) )

# m is number of iterations until convergence
empirical.bayes.results$m

# mu.hat is the MLE of mu (matching optim code)
empirical.bayes.results$mu.hat

# se.hat.mu.hat is the estimated standard error of mu.hat
empirical.bayes.results$se.hat.mu.hat

# sigma.hat is the MLE of sigma (matching optim code)
empirical.bayes.results$sigma.hat

# sigma.squared.hat is the MLE of sigma^2
empirical.bayes.results$sigma.squared.hat

# n.normalized is normalized n vector

# W.hat is the weights for y[i] defining mu.hat
# W.hat.normalized is normalized W.hat vector
empirical.bayes.results$W.hat
empirical.bayes.results$W.hat.normalized

# B.hat is the weights for y[i] defining theta.hat
empirical.bayes.results$B.hat

# y is the vector of mortality differences (control - treatment)
empirical.bayes.results$y

# theta.hat is MLE vector for theta
empirical.bayes.results$theta.hat

# se.hat.theta.hat is the SE for theta.hat
empirical.bayes.results$se.hat.theta.hat