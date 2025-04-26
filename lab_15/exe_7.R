## EXERCISE 7

# 1) Entering the data sets
xdat <- c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2,
          2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)

ydat <- c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40,
          7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04,
          24.25, 27.48)

# 2) Creating a data frame
data <- data.frame(x = xdat, y = ydat)
head(data)

# 3)
# Fitting the model using nls() for y = β * x^n
nls_fit <- nls(y ~ b * x^n, data = data, start = list(b = 1, n = 2))

# View the summary of the fit
summary(nls_fit)

# 4) Print the summary of the fit
summary(nls_fit)

# 5)
# Degrees of freedom
df <- length(xdat) - 2
cat("Degrees of freedom:", df, "\n")

# 6)
# Create a sequence of x values from 1 to 3 with spacing of 0.1
x_seq <- seq(1, 3, by = 0.1)

# Generate the fitted curve using the fitted coefficients from nls_fit
fitted_y <- predict(nls_fit, newdata = data.frame(x = x_seq))

# Plot the original data points and the fitted curve
plot(xdat, ydat, main = "Non-linear Regression Fit", xlab = "x", ylab = "y", pch = 16, col = "blue")
lines(x_seq, fitted_y, col = "red", lwd = 2)  # Add the fitted curve
