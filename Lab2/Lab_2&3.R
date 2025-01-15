round(12.1343,digits=3)

round(123.12344,digits=3)

round(1234.12344,digits=3)

round(12345.12344,digits=3)

options(digits=15)
round(12345.123466664,digits=3)

formatC(round(12345.12344,digits=3),format="f",digits=3)

print(1234.12344)

print(1234.723,digits=3)
print(1234.723,digits=5)

round(123456788.123,digits=3)

print(round(123456788.123,digits=2),digits=20)

print(round(123456789.1234,digits=4),digits=20)

paste("Hello World") 
paste("Hello","World")

paste(1:10) 
paste(1:10)[4]

as.numeric(paste(1:10))

paste(1:10,collapose=".")

paste(c('Hello',"World"),1:10,sep='-')
print(paste('Hello',1:10,sep='-'))



0:15

15:5

seq(0,1.5,0.1)

seq(6,4,-0.2)

N <- c(55,76,92,103,84,88,121,91,65,77,99)
N

seq(from=0.04,by=0.01,length=11)
seq(0.04,by=0.01,along=N)

seq(from=0.04,to=0.14,along=N)

sequence(c(4,3,4,4,4,5))

rep(9,5)
rep(1:4,2)
rep(1:4,each=2)
rep(1:4,each=2,times=3)
rep(1:4,1:4)

rep(1:4,c(4,1,4,2))
rep(c("cat","dog","goldfish","rat"),c(2,3,2,1))

seq(-1,1,by=0.1) 
seq(-1,1,0.1)

seq(-1,1,length=7)

-1 + 0.1 * 0:20


seq('TRue')
seq("FALSE")    
seq("11,3,4")



3/0

exp(-Inf)

(0:4)**Inf

0/0

Inf - Inf

Inf/Inf

is.finite(10)

is.infinite(10)

is.infinite(Inf)

y<- c(4,NA,7)
is.na(y)
y[!is.na(y)]

c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
full.frame

reduced.frame <- full.frame[! is.na(full.frame$c1),]
reduced.frame

v <- c(1:6,NA,NA,9:12)
seq(along=v)[is.na(v)]
which(is.na(v))

full.frame[c(seq(11:21)),]
,
