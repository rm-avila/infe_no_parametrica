rm(list = ls())

# Cargar archivo helpers
source("lib/helpers.R")

# La función ecdf calcula la función de 
# distribución empírica

# Distribución normal -----------------------------------------------------

# Datos normales
n_10 <- rnorm(10)
n_100 <- rnorm(100)
n_1000 <- rnorm(1000)

# Gráficas
compara_fde_normal <- function(n){
  x11()
  plot(ecdf(rnorm(n)), 
       main = paste("n = ", n))
  curve(pnorm(x), add = T,
        col = "tomato", lwd = 2)
}


# Distribución Bernoulli --------------------------------------------------

compara_fde_bernoulli <- function(n, p = 0.3){
  x11()
  plot(ecdf(rbinom(n, size = 1, p)), 
       main = paste("n = ", n,
                    "p = ", p))
  curve(pbinom(x, 1, p), add = T,
        col = "tomato", lwd = 2)
}


# Distribución Poisson ----------------------------------------------------

compara_fde_pois <- function(n, lam = 0.3){
  x11()
  plot(ecdf(rpois(n, lam)), 
       main = paste("n = ", n, ",",
                    "lam = ", lam),
       xlim = c(-1,10))
  curve(ppois(x, lam), add = T,
        col = "tomato", lwd = 2)
}

