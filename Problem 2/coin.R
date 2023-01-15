library(binom)
library(coda)
library(MCMCpack)

# Problem 2
# A

# 0 - reszka, 1 - orzeł
simulation <- c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0)

n <- length(simulation)
success <- sum(simulation)

p <- success/n

# B
# Rozwiązanie za pomocą wzorów

standard_error <- sqrt(p*(1-p)/n)
z <- qnorm(0.95)

lower_limit <- p - standard_error*z
upper_limit <- p + standard_error*z

# Rozwiązanie za pomocą biblioteki języka R

alpha_z <- 0.1
interval <- binom.confint(success, n, conf.level = 1 - alpha_z)

is_in_confidenceinteval <- if(lower_limit <= 1/2 && upper_limit >= 1/2) TRUE else FALSE

