rm(list = ls())
source("lib/helpers.R")
cargar_paquetes("goftest")


# Prueba con 4 uniformes (0,1) --------------------------------------------

simula_D_n_unif <- function(n_sim){
  D <- vector()
  for(j in 1:n_sim){
    x_ord <- sort(runif(4))
    
    # Calcular FDE de la muestra 
    F_hat <- ecdf(x_ord)
    
    # Distancia antes y después del salto
    distancias <- function(i){
      antes_salto <- abs(F_hat(x_ord[i]-0.001)-punif(x_ord[i]))
      desp_salto <- abs((F_hat(x_ord[i])-punif(x_ord[i])))
      return(c(antes_salto, desp_salto))
    }
    
    # sup de las distancias
    D_n <- lapply(1:4, distancias) %>% 
      lapply(max) %>% 
      unlist %>% 
      max()
    D[j] <- D_n
  }
  return(D)
}

# Guardar 10 mil simulaciones
set.seed(276)
sim_10k_D <- simula_D_n_unif(10000)
kolm <- sqrt(4)*sim_10k_D

# Histograma y cuantiles
hist(kolm)
# Cuantil para la prueba 
quantile(kolm, 0.95)




# Implementación de las pruebas usando paquetes de R ----------------------

# Simularemos de una Normal (0,2) y haremos la prueba H_0: F~N(0,1) a nivel de signif.
# alpha = 0.05, la cual esperaríamos que se rechace (recordar que se rechaza cuando
# el p-value es menor que el nivel de significancia)

set.seed(813)
x <- rnorm(50, 0, sqrt(2))

# Prueba de Kolmogorov - Smirnov
ks.test(x, pnorm, mean = 0, sd = 1)
# No se rechaza

# Prueba de Cramér - von Mises
cvm.test(x, pnorm, mean = 0, sd = 1)
# Se rechaza

# Prueba de Anderson - Darling
ad.test(x, pnorm, mean = 0, sd = 1)
# Rechaza (más contundentemente)

# Observar que los argumentos de las tres pruebas son 
# (muestra, F_0, argumentos adicionales de F_0)

# Lo que las funciones regresan es una lista con los siguientes elementos:
ks.test(x, pnorm, mean = 0, sd = 1) %>% names

# El p-value está en .$p.value

# Ahora nos preguntamos ¿Cuál es la probabilidad de error Tipo 2 de cada prueba
# a un nivel de signif. 0.05? Es decir, ¿Cuántas veces no rechaza cuando debería?

error_tipo_2 <- function(n, nsim = 10000, alpha = 0.05){
  # n corresponde al tamaño de muestra y nsim al número de simulaciones
  
  muestras <- matrix(rnorm(n*nsim, mean = 0, sd = sqrt(2)), 
                     nrow = nsim, ncol = n)
  # Guardamos una muestra de tamaño n por fila
  
  # Prueba de Kolmogorov-Smirnov
  ks_pvals <- apply(muestras, 1, ks.test, pnorm, mean = 0, sd = 1) %>% 
    map_dbl(2) # Extraer el segundo elemento, que es el p-value
  # Proporción de veces que no rechazó
  ks_prob <- mean(ks_pvals > alpha)
  
  # Prueba de Cramér-Von Mises
  cvm_pvals <- apply(muestras, 1, cvm.test, pnorm, mean = 0, sd = 1) %>% 
    map_dbl(2) # Extraer el segundo elemento, que es el p-value
  # Proporción de veces que no rechazó
  cvm_prob <- mean(cvm_pvals > alpha)
  
  # Prueba de Anderson-Darling
  ad_pvals <- apply(muestras, 1,ad.test, pnorm, mean = 0, sd = 1) %>% 
    map_dbl(2) # Extraer el segundo elemento, que es el p-value
  # Proporción de veces que no rechazó
  ad_prob <- mean(ad_pvals > alpha)
  
  # Salida de la función
  out <- c(error_ks = ks_prob, error_cvm = cvm_prob, error_ad = ad_prob)
  return(out)
}

# Tarda un buen rato...

errors_df <- seq(from = 10, to = 250, by = 10) %>% 
  map(error_tipo_2)


power_df <- errors_df %>% 
  do.call("rbind", .) %>% 
  as.data.frame() %>% 
  mutate(n = seq(from = 10, to = 250, by = 10)) %>% 
  gather(test, power, -n)

# Potencia de las pruebas
power_df %>% 
  ggplot(aes(x = n, y = 1-power, group = test, colour = test)) +
  geom_line() +
  scale_color_discrete(name = "Prueba", 
                       labels = c("Anderson Darling", "Cramér-von Mises",
                                  "Kolmogorov Smirnov")) +
  ylab("Poder")
