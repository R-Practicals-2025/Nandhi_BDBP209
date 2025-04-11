##(5) Poisson distribution:

#5a)
lambda <- 10  # Mean number of events
m <- 7        # Observed count

poisson_prob <- dpois(m, lambda)
print(paste("P(m=7) =", round(poisson_prob, 4)))

#5b)
cum_prob_pois <- ppois(m, lambda)
print(paste("P(m ≤ 7) =", round(cum_prob_pois, 4)))

#5c)
n <- 10000  
# p <- 0.3
p <- 0.01
lambda_binom <- n * p  # λ = np

m_vals <- 0:n

binom_pmf <- dbinom(m_vals, n, p)
poisson_pmf <- dpois(m_vals, lambda_binom)

plot(m_vals, binom_pmf, type="h", col="lightblue", lwd=2,
     main="Comparison of Binomial and Poisson Distributions",
     xlab="m (Event Count)", ylab="Probability",ylim =range(c(binom_pmf,poisson_pmf)),xlim=range(c(0,200)))

lines(m_vals, poisson_pmf, type="h", col="green", lwd=2)


##5d)
quantile_pois <- qpois(0.22, lambda)
print(paste("Quantile (0.22 CDF):", quantile_pois))

#5e)
set.seed(123)
sample_pois <- rpois(10000, lambda=9)
hist(sample_pois, breaks=30, col="skyblue", main="Poisson Deviates (λ=9)",
     xlab="Event Count (m)", ylab="Frequency", probability=TRUE)


