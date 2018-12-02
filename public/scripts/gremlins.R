##################################################
## Function purpose: Debugging demo functions
## Date: 30-11-2017
## Author: Tyler Clavelle
##################################################

gizmo <- function(samples) {
  
  # Sample from a normal distribution
  n1 <- rnorm(n = samples)
  n2 <- rnorm(n = samples * '2')
  n3 <- rnorm(n = samples * 3)
  
  return(n2)
}
