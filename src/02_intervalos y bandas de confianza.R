rm(list = ls())
source("lib/helpers.R")

# Intervalos de confianza ------------------------------------------------

intervalo <- function(x, muestra, alpha = 0.05){
  n <- length(muestra)
  fde <- ecdf(muestra)
  F_hat <- fde(x)
  sd <- -qnorm(alpha/2)*sqrt((F_hat*(1-F_hat))/n)
  out <- c(max(F_hat - sd, 0), min(F_hat + sd,1))
  return(out)
}

# Bandas de confianza -----------------------------------------------------

banda <- function(x, muestra, alpha = 0.05){
  n <- length(muestra)
  fde <- ecdf(muestra)
  F_hat <- fde(x)
  epsilon <- sqrt((1/(2*n))*log(2/alpha))
  L <- max(F_hat - epsilon, 0)
  U <- min(F_hat + epsilon, 1)
  out <- c(L, U)
  return(out)
}


# Pruebas -----------------------------------------------------------------

# Poisson(5)

set.seed(3)
grafica_pois <- function(n){
  sam <- rpois(n, 5)
  rejilla <- seq(0, 15, by = 1)
  interv <- sapply(rejilla, intervalo, muestra = sam)
  env <- sapply(rejilla, banda, muestra = sam)
  F_gorro <- ecdf(sam)
  plot(rejilla, env[2,], type = 'l', col = 'orange', ylim = c(0, 1),
       main = paste("Pois(5), n = ", n))
  lines(rejilla, env[1,], col = 'orange')
  lines(rejilla, interv[1,], col = 'blue')
  lines(rejilla, interv[2,], col = 'blue')
  curve(F_gorro(x), add = T)
  curve(ppois(x, 5), add = T, col = 'tomato')
}


# Ejercicio ---------------------------------------------------------------

# Calcular intervalos y bandas de confianza para la densidad N(0,1)
