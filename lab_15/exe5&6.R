## EXERCISE 5

# (1) Data
x <- c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
y <- c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)

# Fit a 3rd-degree polynomial: y ~ x + x^2 + x^3
model <- lm(y ~ poly(x, 3, raw=TRUE))

# (2) Coefficient of x term
cat("Coefficient of x: ", coef(model)[2], "\n")

# (3) Coefficient of x^2 term
cat("Coefficient of x^2: ", coef(model)[3], "\n")

# (4) Coefficient of x^3 term
cat("Coefficient of x^3: ", coef(model)[4], "\n")

# (5) Plot the original data
plot(x, y, main = "3rd Degree Polynomial Fit", xlab = "x", ylab = "y", col = "blue", pch = 19)

# Add fitted curve
x_fit <- seq(min(x), max(x), length.out = 200)
y_fit <- predict(model, newdata = data.frame(x = x_fit))
lines(x_fit, y_fit, col = "red", lwd = 2)

legend("topleft", legend = c("Data Points", "Fitted Curve"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))




## EXERCISE 6

## i) Read and convert trees dataset to SI units
# Load the required dataset
library(datasets)
df <- trees

# Convert units:
# Girth (inches to meters), Height (feet to meters), Volume (cubic feet to cubic meters)
df$Girth <- df$Girth * 0.0254       # 1 inch = 0.0254 meters
df$Height <- df$Height * 0.3048     # 1 foot = 0.3048 meters
df$Volume <- df$Volume * 0.028317   # 1 cubic foot = 0.028317 cubic meters

# Show the updated dataset
head(df)

## ii) Scatter Plot Matrix using splom
# Install lattice if not already installed
# install.packages("lattice")
library(lattice)

# Scatter plot matrix
splom(df, xlab = "Scatter Plot Matrix")

## (iii) Save Volume as the response vector y
y <- df$Volume

## (iv) Initialize intercept β₀ as a column of 1s
# Create a column of 1s for β0
beta_0 <- rep(1, nrow(df))  # 31 rows

## (v) Build the matrix X with β₀, Girth, and Height
# Construct the design matrix X with intercept, Girth, and Height
X <- as.matrix(cbind(beta_0, df$Girth, df$Height))

# View first few rows of X
head(X)


## (vi) Solve for the regression coefficients using the formula:
# Solving for beta using matrix operations
model1 <- solve(t(X) %*% X) %*% t(X) %*% y

# Print the values of β0, β1, and β2
cat("β0 (Intercept):", model1[1], "\n")
cat("β1 (Girth coefficient):", model1[2], "\n")
cat("β2 (Height coefficient):", model1[3], "\n")

## (vii) Predict volume for specific values of Girth and Height:
# Predicting volume for specific girth and height values
girth_values <- c(0.3, 0.4, 0.5)
height_values <- c(20, 21, 22)

# Compute predicted volumes using the regression model
predicted_volumes <- model1[1] + model1[2] * girth_values + model1[3] * height_values

# Print predicted volumes
cat("Predicted Volumes:\n")
print(predicted_volumes)

## (viii) Use lm() to solve the linear regression:
# Fit the linear model using lm()
model2 <- lm(Volume ~ Girth + Height, data = df)

# Print the model summary
summary(model2)

# Compare the coefficients with model1
cat("β0 (Intercept):", coef(model2)[1], "\n")
cat("β1 (Girth coefficient):", coef(model2)[2], "\n")
cat("β2 (Height coefficient):", coef(model2)[3], "\n")

## (ix) Use predict() to predict volume for new data:
# New data for prediction
newdata <- data.frame(Girth = c(0.3, 0.4, 0.5), Height = c(20, 21, 22))

# Predict volumes using the lm model
predicted_volumes_lm <- predict(model2, newdata)

# Print predicted volumes
cat("Predicted Volumes using lm():\n")
print(predicted_volumes_lm)

