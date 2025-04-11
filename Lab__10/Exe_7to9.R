# 7) 

#7a)
runif(5, min=0, max=1)
#7b)
runif(5, min=50, max=100)

#7c)
uniform_samples <- runif(10000, min=1, max=2)

hist(uniform_samples, breaks=50, probability=TRUE, col="lightblue",
     main="Uniform Distribution", xlab="X", xlim=c(1,2))


# 8) 
#8a)
dexp(3, rate=2)

#8b)
qexp(0.995, rate=2)

#8c)
x_vals <- seq(0, 2, length=100)

cdf_2 <- pexp(x_vals, rate=2)
cdf_10 <- pexp(x_vals, rate=10)
cdf_100 <- pexp(x_vals, rate=100)

plot(x_vals, cdf_2, type="l", col="blue", lwd=4, main="Exponential CDF for Different Lambda Values", xlab="X", ylab="Cumulative Probability")
lines(x_vals, cdf_10, col="red", lwd=4)
lines(x_vals, cdf_100, col="green", lwd=4)

legend("bottomright", legend=c("λ = 2", "λ = 10", "λ = 100"),
       col=c("blue", "red", "green"), lwd=4, lty=c(1,1,1))

#8d)
rexp(4, rate=3)


### 9) Exploring the Gamma Distribution

##9a)
# Set up plotting window: 1 row, 2 plots side by side
par(mfrow=c(1,2))

# X values to evaluate the gamma PDFs
x_vals <- seq(0, 40, length=200)

# --- Left Plot: Varying shape (α), fixed scale (θ = 4) ---

# Gamma PDFs with different shapes
pdf_shape_1 <- dgamma(x_vals, shape=1, scale=4)
pdf_shape_2 <- dgamma(x_vals, shape=2, scale=4)
pdf_shape_3 <- dgamma(x_vals, shape=3, scale=4)
pdf_shape_4 <- dgamma(x_vals, shape=4, scale=4)

# Plot Gamma PDFs for varying α
plot(x_vals, pdf_shape_1, type="l", col="darkgreen", lwd=2,
     main="9) Gamma PDFs (Varying α, θ=4)", xlab="X", ylab="Density")
lines(x_vals, pdf_shape_2, col="blue", lwd=2)
lines(x_vals, pdf_shape_3, col="red", lwd=2)
lines(x_vals, pdf_shape_4, col="darkblue", lwd=2)

legend("topright", legend=c("α=1", "α=2", "α=3", "α=4"),
       col=c("darkgreen", "blue", "red", "darkblue"), lwd=2)

# --- Right Plot: Varying scale (θ), fixed shape (α = 4) ---

# Gamma PDFs with different scales
pdf_scale_1 <- dgamma(x_vals, shape=4, scale=1)
pdf_scale_2 <- dgamma(x_vals, shape=4, scale=2)
pdf_scale_3 <- dgamma(x_vals, shape=4, scale=3)
pdf_scale_4 <- dgamma(x_vals, shape=4, scale=4)

# Plot Gamma PDFs for varying θ
plot(x_vals, pdf_scale_1, type="l", col="darkgreen", lwd=2,
     main="Gamma PDFs (Varying θ, α=4)", xlab="X", ylab="Density")
lines(x_vals, pdf_scale_2, col="blue", lwd=2)
lines(x_vals, pdf_scale_3, col="red", lwd=2)
lines(x_vals, pdf_scale_4, col="darkblue", lwd=2)

legend("topright", legend=c("θ=1", "θ=2", "θ=3", "θ=4"),
       col=c("darkgreen", "blue", "red", "darkblue"), lwd=2)


#9b)
# PDF at x = 6 for α = 4, θ = 1
pdf_value <- dgamma(6, shape=4, scale=1)
cat("PDF at x = 6 (α=4, θ=1):", pdf_value, "\n")

#9c)
# CDF at x = 6
cdf_value <- pgamma(6, shape=4, scale=1)
cat("CDF at x = 6 (α=4, θ=1):", cdf_value, "\n")

##9d)
# 95th percentile (quantile) for the same distribution
quantile_95 <- qgamma(0.95, shape=4, scale=1)
cat("95th Percentile (α=4, θ=1):", quantile_95, "\n")


##9e)
### Generate Gamma Samples and Plot Histogram
# Generate 10,000 random samples from Gamma(α=4, θ=1)
gamma_samples <- rgamma(10000, shape=4, scale=1)

# Plot histogram
hist(gamma_samples, breaks=50, probability=TRUE, col="lightblue",
     main="Histogram of Gamma(α=4, θ=1)", xlab="X")

# Overlay theoretical density curve
lines(x_vals, dgamma(x_vals, shape=4, scale=1), col="darkgreen", lwd=2)


