### EXERCISE 1

## Question 1:
means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)

# Create barplot
barplot_obj <- barplot(means, names.arg = c("A", "B", "C"),
                       col = "grey", ylim = c(0, 30),
                       main = "Errors on bar plot")
box()

# Add error bars using arrows()
arrows(barplot_obj, means + stderr,
       barplot_obj, means - stderr,
       angle = 90, code = 3, length = 0.06, col = "red")

# Question 2:
x <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y <- c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors <- c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

# Plot points
plot(x, y, pch = 16, xlab = "concentration", ylab = "optical activity",
     main = "Error bars on data points", ylim = c(0, 35))

# Add error bars
arrows(x, y + errors, x, y - errors,
       angle = 90, code = 3, length = 0.06, col = "blue")


# Question 3:
x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)

# Covariance
cov_xy <- cov(x, y)
print(cov_xy)

# Correlation
cor_xy <- cor(x, y)
cat("Covariance of x,y :", cor_xy)
#print(cor_xy)

# Correlation matrix for multivariate dataset
data(longley)
head(longley) 
cor(longley)



