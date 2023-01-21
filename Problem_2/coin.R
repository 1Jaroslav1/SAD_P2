library(binom)

# Problem_2
# A

# 0 - reszka, 1 - orzeł
simulation <- c(0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0)

n <- length(simulation)
success <- sum(simulation)

p <- success/n

# B
# Rozwiązanie za pomocą wzorów

standard_error <- sqrt(p*(1-p)/n)
conf_level <- 0.95

alpha <- 1 - conf_level
z <- qnorm(1 - alpha/2)

lower_limit <- p - standard_error*z
upper_limit <- p + standard_error*z

# Rozwiązanie za pomocą biblioteki języka R

binom.test(success, n, conf.level = conf_level)
interval <- binom.confint(success, n, conf.level = conf_level)

is_in_confidenceinteval <- if(lower_limit <= 1/2 && upper_limit >= 1/2) TRUE else FALSE

