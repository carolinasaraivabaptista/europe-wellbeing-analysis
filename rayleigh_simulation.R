# Generating Rayleigh(beta) random numbers in three ways and checking them
library(ggplot2)
library(VGAM)

set.seed(2025)
beta <- 2
n <- 1000

# 1. Inverse transform: F^-1(u) = sqrt(-2 beta^2 log(1 - u))
inv_rayleigh <- function(u, beta) sqrt(-2 * beta^2 * log(1 - u))
rayleigh_inv <- inv_rayleigh(runif(n), beta)

# 2. Box-Muller: the norm of two independent N(0, beta^2) is Rayleigh(beta)
u1 <- runif(n / 2)
u2 <- runif(n / 2)
z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2) * beta
z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2) * beta
rayleigh_bm <- sqrt(z1^2 + z2^2)   # n/2 values

# 3. Reference generator from the VGAM package
rayleigh_vgam <- rrayleigh(n, beta)

# Histogram against the theoretical density (red line)
ggplot() +
  geom_histogram(aes(x = rayleigh_inv, y = after_stat(density)), bins = 30, fill = "lightpink") +
  stat_function(fun = drayleigh, args = list(scale = beta), color = "red", linewidth = 1) +
  labs(title = "Rayleigh distribution: inverse transform sample",
       subtitle = paste("beta =", beta), x = "Value", y = "Density")

# Kolmogorov-Smirnov tests. H0: the sample comes from Rayleigh(beta)
ks_inv <- ks.test(rayleigh_inv, "prayleigh", scale = beta)
ks_bm  <- ks.test(rayleigh_bm, "prayleigh", scale = beta)
cat("KS p-value, inverse transform:", ks_inv$p.value, "\n")
cat("KS p-value, Box-Muller:       ", ks_bm$p.value, "\n")
# Both p-values are above 0.05, so the samples are consistent with Rayleigh(beta)

# The generators are independent of each other (correlation close to 0)
cor(rayleigh_inv, rayleigh_vgam)
cor(rayleigh_bm, rayleigh_vgam[seq_along(rayleigh_bm)])
