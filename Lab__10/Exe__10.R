### 10) Chi-square distribution:

### 10a)
x_values <- seq(0, 30, length=200)

pdf_2  <- dchisq(x_values, df=2)
pdf_3  <- dchisq(x_values, df=3)
pdf_5  <- dchisq(x_values, df=5)
pdf_10 <- dchisq(x_values, df=10)

plot(x_values, pdf_2, type="l", col="black", lwd=2,
     main="Chi-Square PDF", xlab="X", ylab="density")
lines(x_values, pdf_3, col="blue", lwd=2)
lines(x_values, pdf_5, col="red", lwd=2)
lines(x_values, pdf_10, col="orange", lwd=2)
legend("topright", legend=c("DOF=2", "DOF=3", "DOF=5", "DOF=10"),
       col=c("black", "blue", "red", "orange"), lwd=2)



### 10b) PDF of Chi-Square Distribution at x = 6, df = 5

chi_pdf_6_df5 <- dchisq(6, df=5)
cat("Chi-Square PDF at x = 6, df = 5:", chi_pdf_6_df5, "\n")


### 10c) CDF of Chi-Square Distribution up to x = 6, df = 10

chi_cdf_6_df10 <- pchisq(6, df=10)
cat("Chi-Square CDF at x = 6, df = 10:", chi_cdf_6_df10, "\n")


### 10d) 85th Quantile of Chi-Square Distribution with df = 6

chi_q_85_df6 <- qchisq(0.85, df=6)
cat("85th Quantile (df = 6):", chi_q_85_df6, "\n")


### 10e) Histogram of Random Chi-Square Samples (df = 6)

# Generate 10,000 random samples
chi_samples <- rchisq(10000, df=6)

# Plot histogram with 30 bins
hist(chi_samples, breaks=30, probability=TRUE, col="lightgreen",
     main="Histogram of Chi-Square (df = 6)", xlab="X")

# Overlay theoretical PDF
curve(dchisq(x, df=6), col="lightseagreen", lwd=2, add=TRUE)

# Add label to plot
text(x=10, y=0.05, labels="r = 6", col="blue", cex=1.2)


### 10f) Transform to Z² and Plot Chi-Square PDF with df = 1

# Assume normal parameters μ and σ
mu <- 2
sigma <- 1

# Define x values
x_vals <- seq(-5, 10, length=200)

# Compute Z²
z_squared <- ((x_vals - mu)^2) / sigma^2

# Evaluate Chi-Square PDF at Z² with 1 degree of freedom
pdf_chisq_df1 <- dchisq(z_squared, df=1)

# Plot
plot(z_squared, pdf_chisq_df1, type="l", col="palevioletred1", lwd=2,
     main="Chi-Square PDF (df = 1)", xlab="Z²", ylab="Density")


