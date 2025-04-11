
##(3) Geometric distribution:

#3a)
par(mfrow=c(1,2))
x_vals <- 1:10

p1 <- 0.3
pdf_p1 <- dgeom(x_vals - 1, p1)
barplot(pdf_p1, names.arg=x_vals, col="lightsteelblue2",main="Geometric PDF (p=0.3)",
        xlab="Trial Number (m)", ylab="Probability")

p2 <- 0.8
pdf_p2 <- dgeom(x_vals - 1, p2)
barplot(pdf_p2, names.arg=x_vals, col="thistle2",main="Geometric PDF (p=0.8)",
        xlab="Trial Number (m)", ylab="Probability")

par(mfrow=c(1,1)) # to reset

#3b)
pgeom(4 - 1, prob = 0.3)

#3c)
qgeom(0.2, prob = 0.3) + 1

#3d)
rgeom(6, prob = 0.4) + 1  


##(4) Negative binomial distribution:

#4a)
dnbinom(5, size = 3, prob = 0.3)
#4b)
pnbinom(5, size = 3, prob = 0.3)
#4c)
qnbinom(0.5, size = 3, prob = 0.3)
#4d)
rnbinom(4, size = 3, prob = 0.3)

#4e)
r <- 10
p <- 0.3
y_vals <- 0:30  

pdf_vals <- dnbinom(y_vals, size = r, prob = p)

barplot(pdf_vals, names.arg=y_vals, col="navajowhite",
        main="Negative Binomial Distribution (r=10, p=0.3)",
        xlab="No of Failures (y)", ylab="Probability")


#4f)
samples <- rnbinom(10000, size = 10, prob = 0.3)

hist(samples, breaks=30, col="lightblue", main="Negative Binomial Distribution", xlab="Number of Failures", ylab="Frequency", border="black")

