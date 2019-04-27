rm(list = ls())
src("lib/helpers.R")

pre<-c(1.83, .50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
post<-c(.878, .647, .598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.test(pre, post, paired=TRUE, alterative = "less")
