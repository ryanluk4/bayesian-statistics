scaled.inverse.chisq.density <- function( theta, nu, theta.0 ) {

# note that theta plays the role of sigma^2 in this function;
# when used as a prior for theta, nu represents the prior sample size
# and theta.0 = sigma.0^2 represents the prior estimate
# of theta = sigma^2

  log.density <- ( nu / 2 ) * log( nu / 2 ) - lgamma( nu / 2 ) +
    ( nu / 2 ) * log( theta.0 ) - ( 1 + nu / 2 ) * log( theta ) -
    nu * theta.0 / ( 2 * theta )

  return( exp( log.density ) )

}