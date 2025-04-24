# EXERCISE 2

## Question 1
one_sample_Ztest <- function(x, sigma, muzero, alpha, null) {
  n <- length(x)
  xbar <- mean(x)
  z_value <- (xbar - muzero) / (sigma / sqrt(n))
  
  # Calculate p-value based on alternative hypothesis
  if (null == "equal") {
    p_value <- 2 * (1 - pnorm(abs(z_value)))
  } else if (null == "less_than_or_equal") {
    p_value <- 1 - pnorm(z_value)
  } else if (null == "more_than_or_equal") {
    p_value <- pnorm(z_value)
  } else {
    stop("Invalid null hypothesis type.")
  }
  
  # Conclusion
  if (p_value < alpha) {
    conclusion <- "Reject the null hypothesis."
  } else {
    conclusion <- "Fail to reject the null hypothesis."
  }
  
  return(list(Z = z_value, p_value = p_value, conclusion = conclusion))
}


x <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
       137.4, 145.6, 135.6, 135.4, 121.5)

result_z <- one_sample_Ztest(x, sigma = 14.5, muzero = 124.6, alpha = 0.05, null = "equal")
print(result_z)

## Question 2
one_sample_t_test <- function(x, muzero, alpha, null) {
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  t_value <- (xbar - muzero) / (s / sqrt(n))
  df <- n - 1
  
  # Calculate p-value based on alternative hypothesis
  if (null == "equal") {
    p_value <- 2 * (1 - pt(abs(t_value), df))
  } else if (null == "less_than_or_equal") {
    p_value <- 1 - pt(t_value, df)
  } else if (null == "more_than_or_equal") {
    p_value <- pt(t_value, df)
  } else {
    stop("Invalid null hypothesis type.")
  }
  
  # Conclusion
  if (p_value < alpha) {
    conclusion <- "Reject the null hypothesis."
  } else {
    conclusion <- "Fail to reject the null hypothesis."
  }
  
  return(list(t = t_value, p_value = p_value, conclusion = conclusion))
}

x_t <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
         96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

result_t <- one_sample_t_test(x_t, muzero = 100, alpha = 0.05, null = "equal")
print(result_t)


## Question 3
# Parameters
x <- 710
n <- 2600
p <- 0.25
alternative <- "greater"

# Exact binomial test
binom_result <- binom.test(x, n, p, alternative = alternative)
print(binom_result)

# Proportion test (normal approximation)
prop_result <- prop.test(x, n, p, alternative = alternative, correct = TRUE)
print(prop_result)
