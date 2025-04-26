## EXERCISE 3

# Qusetion 1:
## a)
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null = 0) {
  n1 <- length(x1)
  n2 <- length(x2)
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  
  # Standard error
  se <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Z statistic
  z <- (mean_x1 - mean_x2 - null) / se
  
  # One-tailed test (right side, since H0: mu1 >= mu2 → Ha: mu1 < mu2)
  p_value <- pnorm(z, lower.tail = FALSE)
  
  # Decision
  decision <- ifelse(p_value < alpha, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis")
  
  # Return results
  list(
    Z_statistic = z,
    p_value = p_value,
    decision = decision,
    mean_x1 = mean_x1,
    mean_x2 = mean_x2
  )
}

## b)
# Read data (assumes space or tab delimited file)
file.exists("/home/ibab/Desktop/Nandhi_BDBP209/two-sample.dat")
#data <- read.table("/home/ibab/Desktop/Nandhi_BDBP209/two-sample.dat", header = FALSE, sep = "", fill = TRUE)
x1 <-c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
        246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
        174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
        231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
        173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
        218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)
x2 <- c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
         206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
         233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
         178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
         243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
         267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)

# Apply the test
result=two_sample_Z_test(x1, x2, sigma_x1 = 24.6, sigma_x2 = 27.8, alpha = 0.05, null = 0)
print(result)

two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null = 0, alternative = "two.sided") {
  n1 <- length(x1)
  n2 <- length(x2)
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  
  # Standard error
  se <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Z statistic
  z <- (mean_x1 - mean_x2 - null) / se
  
  # p-value based on alternative hypothesis
  if (alternative == "two.sided") {
    p_value <- 2 * (1 - pnorm(abs(z)))
    decision <- ifelse(p_value < alpha, "Reject H0", "Fail to reject H0")
    alt_text <- "μ1 ≠ μ2"
  } else if (alternative == "greater") {
    p_value <- 1 - pnorm(z)
    decision <- ifelse(p_value < alpha, "Reject H0", "Fail to reject H0")
    alt_text <- "μ1 > μ2"
  } else if (alternative == "less") {
    p_value <- pnorm(z)
    decision <- ifelse(p_value < alpha, "Reject H0", "Fail to reject H0")
    alt_text <- "μ1 < μ2"
  } else {
    stop("Invalid alternative. Choose from 'two.sided', 'greater', or 'less'.")
  }
  
  return(list(
    z_value = z,
    p_value = p_value,
    decision = decision,
    null_hypothesis = paste("H0: μ1 - μ2 =", null),
    alternative_hypothesis = paste("Ha:", alt_text)
  ))
}

result2 <- two_sample_Z_test(x1, x2, sigma_x1 = 24.6, sigma_x2 = 27.8, alpha = 0.05, null = 0, alternative = "two.sided")
print(result2)


# Question 2:
## a)
Xvar <- c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar <- c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)

t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE)

## b)
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after  <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Paired t-test
t.test(data_before, data_after, paired = TRUE, alternative = "two.sided")

## 2Q)
detailed_t_test <- function(x, y, paired = FALSE, equal_var = FALSE, alpha = 0.05) {
  # Perform t-test
  test_result <- t.test(x, y, paired = paired, var.equal = equal_var, alternative = "two.sided", conf.level = 1 - alpha)
  
  cat("\n=====================================\n")
  cat(ifelse(paired, "Paired t-test", "Two-sample (independent) t-test"), "\n")
  cat("=====================================\n")
  
  cat("\nHypotheses:\n")
  if (paired) {
    cat("H₀: mean(before - after) = 0\n")
    cat("H₁: mean(before - after) ≠ 0\n")
  } else {
    cat("H₀: mean(x) = mean(y)\n")
    cat("H₁: mean(x) ≠ mean(y)\n")
  }
  
  cat("\nTest Statistic:\n")
  cat("t =", round(test_result$statistic, 4), " | df =", round(test_result$parameter, 2), "\n")
  
  cat("\np-value:\n")
  cat("p =", round(test_result$p.value, 4), "\n")
  
  cat("\nConfidence Interval (", 100*(1-alpha), "%):\n", sep = "")
  print(test_result$conf.int)
  
  cat("\nSample Means:\n")
  cat("Mean of x =", round(mean(x), 3), "\n")
  cat("Mean of y =", round(mean(y), 3), "\n")
  
  cat("\nConclusion:\n")
  if (test_result$p.value < alpha) {
    cat("Reject the null hypothesis. There is a statistically significant difference.\n")
  } else {
    cat("Fail to reject the null hypothesis. No statistically significant difference.\n")
  }
  
  invisible(test_result)
}
# Question 2 a) Independent two-sample t-test
Xvar <- c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar <- c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)
detailed_t_test(Xvar, Yvar, paired = FALSE, equal_var = FALSE)

# Question 2 b) Paired t-test
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after  <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)
detailed_t_test(data_before, data_after, paired = TRUE)

## Question 3:
## a)
# Men: 520 out of 600
# Women: 550 out of 600
x <- c(520, 550)
n <- c(600, 600)

prop.test(x, n, alternative = "two.sided", correct = TRUE)

## b)
# Create 2x2 table
matrix_data <- matrix(c(11, 17, 42, 39), nrow = 2, byrow = TRUE)
colnames(matrix_data) <- c("Higher-income", "Lower-income")
rownames(matrix_data) <- c("Abuse", "No Abuse")

# Fisher's test
fisher.test(matrix_data, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)

## Question 4
two_sample_variance_test <- function(x, y, alpha = 0.05) {
  var_x <- var(x)
  var_y <- var(y)
  df_x <- length(x) - 1
  df_y <- length(y) - 1
  
  F_value <- var_x / var_y
  p_value <- 2 * min(pf(F_value, df_x, df_y), 1 - pf(F_value, df_x, df_y))  # Two-tailed
  lower <- qf(alpha / 2, df_x, df_y)
  upper <- qf(1 - alpha / 2, df_x, df_y)
  
  cat("Two-sample F-test for Equality of Variances\n")
  cat("--------------------------------------------------\n")
  cat("Variance of x:", round(var_x, 4), "\n")
  cat("Variance of y:", round(var_y, 4), "\n")
  cat("F-statistic =", round(F_value, 4), " | df1 =", df_x, ", df2 =", df_y, "\n")
  cat("Critical F-range: [", round(lower, 4), ",", round(upper, 4), "]\n")
  cat("p-value =", round(p_value, 4), "\n")
  
  if (F_value < lower || F_value > upper) {
    cat("Conclusion: Reject H0. Variances are significantly different.\n")
  } else {
    cat("Conclusion: Fail to reject H0. No significant difference in variances.\n")
  }
  
  invisible(list(F = F_value, p = p_value))
}

# Sample data for Question 4
x <- c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,1047.0,987.8,1051.0,885.2,1049.5,1098.2,1001.5,1011.1,991.6)
y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)
two_sample_variance_test(x, y, alpha = 0.05)


## Question 5
signed_rank_test <- function(pre, post, alpha = 0.05) {
  test <- wilcox.test(post, pre, paired = TRUE, alternative = "greater", conf.level = 1 - alpha, exact = FALSE)
  
  cat("Wilcoxon Signed-Rank Test (Paired, One-Tailed)\n")
  cat("--------------------------------------------------\n")
  cat("H0: median(post - pre) <= 0\n")
  cat("H1: median(post - pre) > 0\n")
  cat("Test statistic V =", test$statistic, "\n")
  cat("p-value =", round(test$p.value, 4), "\n")
  
  if (test$p.value < alpha) {
    cat("Conclusion: Reject H0. Median increase after is statistically significant.\n")
  } else {
    cat("Conclusion: Fail to reject H0. No statistically significant increase.\n")
  }
  
  invisible(test)
}

# Sample data
pre <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
post <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)
signed_rank_test(pre, post)


## Question 6
rank_sum_test <- function(group1, group2, alpha = 0.05) {
  test <- wilcox.test(group1, group2, alternative = "less", conf.level = 1 - alpha, exact = FALSE)
  
  cat("Wilcoxon Rank-Sum Test (Independent, One-Tailed)\n")
  cat("--------------------------------------------------\n")
  cat("H0: median(group1) >= median(group2)\n")
  cat("H1: median(group1) < median(group2)\n")
  cat("Test statistic W =", test$statistic, "\n")
  cat("p-value =", round(test$p.value, 4), "\n")
  
  if (test$p.value < alpha) {
    cat("Conclusion: Reject H0. Group 1 has significantly lower values than Group 2.\n")
  } else {
    cat("Conclusion: Fail to reject H0. No significant evidence that Group 1 is lower.\n")
  }
  
  invisible(test)
}

# Sample data
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)
rank_sum_test(placebo, drug)


# Question 7:
Group1 <- c(220, 214, 203, 184, 186, 200, 165)
Group2 <- c(262, 193, 225, 200, 164, 266, 179)
Group3 <- c(272, 192, 190, 208, 231, 235, 141)
Group4 <- c(190, 255, 247, 278, 230, 269, 289)

# Combine data into x and group labels into y
x <- c(Group1, Group2, Group3, Group4)
y <- factor(rep(1:4, each = 7))  # Group labels

# Perform Kruskal-Wallis test
kruskal.test(x ~ y)


## Question 8
chi_square_gof_test <- function(observed, expected, alpha = 0.05) {
  # Step 1: Compute the Chi-square statistic
  chi_square_statistic <- sum((observed - expected)^2 / expected)
  
  # Step 2: Degrees of freedom
  df <- length(observed) - 1
  
  # Step 3: Critical value for chi-square distribution
  critical_value <- qchisq(1 - alpha, df)
  
  # Step 4: p-value for chi-square statistic
  p_value <- 1 - pchisq(chi_square_statistic, df)
  
  # Step 5: Conclusion
  conclusion <- ifelse(chi_square_statistic > critical_value,
                       "Reject null hypothesis: Observed and expected distributions are different.",
                       "Fail to reject null hypothesis: Observed and expected distributions are the same.")
  
  # Return results
  return(list(chi_square_statistic = chi_square_statistic, p_value = p_value, critical_value = critical_value, conclusion = conclusion))
}

observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)

# Run the Chi-Square GoF test
result_gof <- chi_square_gof_test(observed, expected, alpha = 0.05)
print(result_gof)

