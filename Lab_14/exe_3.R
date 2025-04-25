## EXERCISE 3

# Qusetion 1:
## a)
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null) {
  n1 <- length(x1)
  n2 <- length(x2)
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  
  # Standard error
  se <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Z statistic
  z <- (mean_x1 - mean_x2) / se
  
  # p-value based on hypothesis
  if (null == "equal") {
    p_value <- 2 * (1 - pnorm(abs(z)))
  } else if (null == "greater") {
    p_value <- 1 - pnorm(z)
  } else if (null == "less") {
    p_value <- pnorm(z)
  } else {
    stop("Invalid null hypothesis type. Use 'equal', 'greater', or 'less'.")
  }
  
  conclusion <- ifelse(p_value < alpha, "Reject the null hypothesis", "Fail to reject the null hypothesis")
  return(list(Z = z, p_value = p_value, conclusion = conclusion))
}

## b)
# Read data (assumes space or tab delimited file)
data <- read.table("two-sample.dat", header = FALSE)
x1 <- data$V1
x2 <- data$V2

# Apply the test
two_sample_Z_test(x1, x2, sigma_x1 = 24.6, sigma_x2 = 27.8, alpha = 0.05, null = "greater")


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
# a)
two_sample_variance_test <- function(x, y, alpha = 0.05) {
  var_x <- var(x)
  var_y <- var(y)
  df_x <- length(x) - 1
  df_y <- length(y) - 1
  
  F_value <- var_x / var_y
  p_value <- 2 * min(pf(F_value, df_x, df_y), 1 - pf(F_value, df_x, df_y))  # two-tailed
  
  # Determine critical values
  lower <- qf(alpha / 2, df_x, df_y)
  upper <- qf(1 - alpha / 2, df_x, df_y)
  
  conclusion <- ifelse(F_value < lower || F_value > upper,
                       "Reject null: Variances are significantly different.",
                       "Fail to reject null: No significant difference in variances.")
  
  return(list(F_value = F_value, p_value = p_value, conclusion = conclusion))
}

# b)
x <- c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
       1047.0,987.8,1051.0,885.2,1049.5,1098.2,1001.5,1011.1,991.6)

y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
       1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

result_f <- two_sample_variance_test(x, y, alpha = 0.05)
print(result_f)


# Question 5:
pre <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
post <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

# Test if post - pre > 0 → H0: median difference <= 0
wilcox.test(post, pre, paired = TRUE, alternative = "greater", conf.level = 0.95)


# Question 6
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

# Test if placebo < drug → H0: placebo mean >= drug mean
wilcox.test(placebo, drug, alternative = "less", conf.level = 0.95)


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

