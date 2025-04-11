##I. Sampling, permutations and combinations

##1Q)
#sampling from a vector

x <- seq(1,100)

s <- sample(x,10)

sample(x,20,replace=TRUE)


##2Q)
##package gtools has permutations and combinations functions
library(gtools)

x <- c("A","B","C","D")
per <- permutations(n=length(x), r=3,v=x,repeats.allowed=TRUE)
print(per)

comb <- combinations(n=length(x), r=3, v=x)
print(comb)


##II. Distributions
##(1) Binomial distribution:

#1a&1b)
binom=function(m,n,p){
  prob=dbinom(m,n,p)
  print(prob)
  cumprob=pbinom(m,n,p)
  print(cumprob)
}
binom(m=3,n=10,p=0.4)
binom(m=3,n=10,p=0.5)

#1c)
find_m=function(cum_prob,n,p){
  m=qbinom(cum_prob,n,p)
  print(m)
}
find_m(0.8,10,0.4)

#1d)
random_points=function(npts,n,p){
  points=rbinom(npts,n,p)
  print(points)
}
random_points(5,p=0.4,n=10)


#1e)
plot_pdf=function(n,p1,p2){
  m_values <- 0:n
  
  pdf_p1 <- dbinom(m_values, n, p1)
  pdf_p2 <- dbinom(m_values, n, p2)
  
  plot(m_values, pdf_p1, type = "b", col = "blue", pch = 16, xlab = "Number of successes (m)", 
       ylab = "Probability", main = "Binomial Distribution PDF for n=10",ylim=c(0,max(pdf_p1,pdf_p2)*1.2))
  lines(m_values, pdf_p2, type = "b", col = "red", pch = 16)
  
  legend("topright", legend = c("p = 0.4", "p = 0.7"), col = c("blue", "red"), pch = 16)
  grid()
}
plot_pdf(10,0.4,0.7)

#1f)
random_sampling=function(n,p1){
  set.seed(123)
  
  samples_100 <- rbinom(100, n, p1)
  samples_10000 <- rbinom(10000, n, p1)
  
  freq_100 <- table(samples_100)
  freq_10000 <- table(samples_10000)
  
  y_range_100 <- range(freq_100)
  y_range_10000 <- range(freq_10000)
  
  par(mfrow = c(1,2))
  
  barplot(freq_100, col = "lightblue", main = "Frequency Distribution (100 samples)", 
          xlab = "Number of successes (m)", ylab = "Frequency", border = "black",
           ylim = c(0, 25))
  names.arg=abline(h=0,col='black',lwd=2.5)

  
  barplot(freq_10000, col = "lightgreen", main = "Frequency Distribution (10000 samples)", 
          xlab = "Number of successes (m)", ylab = "Frequency", border = "black", ylim = c(0, max(y_range_10000)))
  names.arg=abline(h=0,col='black',lwd=2.5)

}
random_sampling(10, 0.4)


##(2) Hypergeometric distribution:

#2a)
N <- 100  # Population size
K <- 70   # Number of success items in population
n <- 12   # Sample size

x_vals <- 0:n  # Possible values of k (successes in sample)
pdf_vals <- dhyper(x_vals, K, N-K, n)

barplot(pdf_vals, names.arg=x_vals, col="slategray2", main="Hypergeometric PDF (N=100, K=70, n=12)", xlab="Number of Successes in Sample", ylab="Probability")

text(4, max(pdf_vals) * 0.8, labels="N=100, K=70, n=12", col="lightpink2")

#2b)
cum_prob <- phyper(10, K, N-K, n)
round(cum_prob, 3)

#2c)
qhyper(0.9, K, N-K, n)


sample_points <- rhyper(5, K, N-K, n)
format(sample_points, digits=2)

#2d)
# Set parameters
N <- 100  # Total population size
K <- 70   # Total number of success states in population
n <- 12   # Number of draws
k_values <- 0:n  # Possible values of k (successes in draws)

# Compute the probability mass function (PMF)
pdf_hyper <- dhyper(k_values, K, N - K, n)  

# Plot the histogram-type bar plot
barplot(pdf_hyper, names.arg = k_values, col = "skyblue", border = "black",
        main = "Hypergeometric Distribution (N=100, K=70, n=12)",
        xlab = "Number of Successes (k)", ylab = "Probability Mass Function")

# Add text within the plot window
text(5, max(pdf_hyper) * 0.8,
     labels = paste("N =", N, "\nK =", K, "\nn =", n),
     col = "brown", cex = 1)


