rm(list = ls())
source("lib/helpers.R")

pre<-c(1.83, .50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
post<-c(.878, .647, .598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.test(pre, post, paired=TRUE, alterative = "less")


# Prueba de M-W-Wilcoxon (Consumo de alcohol) -----------------------------

cuantiles <- data.frame(u = 0:132) %>% 
  mutate(prob_u = pwilcox(u, 12, 11, lower.tail = T))

view(cuantiles)

x<−c(1042, 1617, 1180, 973, 1552, 1251, 1151, 1511, 728, 1079, 951, 1319)
y<−c(874, 389, 612, 798, 1152, 893, 541, 741, 1064, 862, 213)

wilcox.test(x, y, alternative = "greater")
