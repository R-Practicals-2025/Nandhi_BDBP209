# Q1:  CLT
##Central Limit Theorem using Uniform[0,10]

set.seed(123)

# (i) Generate 10,000 samples of size 5 from Uniform[0,10]
samples <- matrix(runif(10000 * 5, min = 0, max = 10), ncol = 5)
sample_means <- rowMeans(samples)

# (ii) Plot histogram and inspect bin width
hist_out <- hist(sample_means, breaks = 50, col = "lightblue", 
                 main = "CLT: Sample Means from Uniform[0,10]", 
                 xlab = "Sample Mean")

# (ii continued) Print mean and standard deviation of sample means
mean_sample <- mean(sample_means)
sd_sample <- sd(sample_means)
print(paste("Mean of sample means:", mean_sample))
print(paste("Standard deviation of sample means:", sd_sample))

# (iii) Compute normal PDF over sample mean range
x_vals <- seq(min(sample_means), max(sample_means), length.out = 100)
normal_pdf <- dnorm(x_vals, mean = mean_sample, sd = sd_sample)

# (iv) Scale normal curve to match histogram
bin_width <- hist_out$breaks[2] - hist_out$breaks[1]
scaled_pdf <- normal_pdf * length(sample_means) * bin_width

# (v) Re-plot histogram with overlaid normal curve
hist(sample_means, breaks = 50, col = "lightblue", 
     main = "CLT: Sample Means from Uniform[0,10]", 
     xlab = "Sample Mean", freq = TRUE)
lines(x_vals, scaled_pdf, col = "red", lwd = 2)
legend("topright", legend = c("Sample Means", "Normal Curve"), 
       col = c("lightblue", "red"), lwd = 2, fill = c("lightblue", NA))


# Q2: Dice roll simulation to illustrate the CLT

# (i) Single die roll (Uniform discrete)
a <- sample(1:6, replace = TRUE, 10000)
hist(a, breaks = seq(0.5, 6.5, by = 1), col = "skyblue",
     main = "Single Die Roll (Uniform)", xlab = "Die Value")

# (ii) Sum of two dice (Triangular distribution)
b <- sample(1:6, replace = TRUE, 10000)
ab_sum <- a + b
hist(ab_sum, breaks = seq(1.5, 12.5, by = 1), col = "lightgreen",
     main = "Sum of Two Dice", xlab = "Sum")

# (iii) Sum of three dice (distribution begins to bell-shape)
c <- sample(1:6, replace = TRUE, 10000)
abc_sum <- a + b + c
hist(abc_sum, breaks = seq(2.5, 18.5, by = 1), col = "lightcoral",
     main = "Sum of Three Dice", xlab = "Sum")

# (iv) Sum of five dice (nearly normal)
d <- sample(1:6, replace = TRUE, 10000)
e <- sample(1:6, replace = TRUE, 10000)
abcde_sum <- a + b + c + d + e

hist(abcde_sum, breaks = seq(4.5, 30.5, by = 1), col = "orchid",
     main = "Sum of Five Dice", xlab = "Sum")

# Overlay normal distribution
mu <- mean(abcde_sum)
sigma <- sd(abcde_sum)
x_vals <- seq(min(abcde_sum), max(abcde_sum), by = 0.1)
normal_pdf <- dnorm(x_vals, mean = mu, sd = sigma)

# Bin width = 1, so scale normal PDF by 10000
lines(x_vals, normal_pdf * 10000, col = "darkblue", lwd = 2)


# Q3: ROC Curves for predicting wine quality thresholds using alcohol content

# Load necessary package
library(pROC)

# Step 1: Read white wine dataset
white_wine <- read.csv("winequality-white.csv", sep = ";")

# Step 2: Create binary target variables for thresholds from 6 to 10
thresholds <- 6:10
for (t in thresholds) {
  col_name <- paste0("quality_", t)
  white_wine[[col_name]] <- ifelse(white_wine$quality >= t, 1, 0)
}

# Step 3: Plot ROC curves for each threshold
colors <- c("red", "blue", "green", "purple", "orange")

# Set up blank ROC plot
plot(NULL, xlim = c(1, 0), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     main = "ROC Curves for Different Quality Thresholds")
abline(a = 0, b = 1, lwd = 2, lty = 2)  # diagonal reference line

# Plot ROC curves for each threshold
for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  response <- white_wine[[paste0("quality_", t)]]
  roc_obj <- roc(response, white_wine$alcohol, legacy.axes = TRUE, ci = TRUE)
  plot(roc_obj, col = colors[i], add = TRUE, print.auc = TRUE, print.thres = TRUE)
}

# Add legend
legend("bottomright", legend = paste("Threshold >=", thresholds), 
       col = colors, lwd = 2)
