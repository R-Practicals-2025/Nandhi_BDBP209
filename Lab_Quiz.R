#Question 1
maths_ops<-function(k,n){
  print(paste("Addition: ",k+n))
  print(paste("Subtraction: ",k-n))
  print(paste("Multiplication: ",k*n))
  print(paste("Quotient: ",k%/%n))
  print(paste("Remainder: ",k%%n))
  print(paste("Power: ",k**n))
}
maths_ops(2,3)

#Question 2
my_quadratic<-function(a,b,c){
  root1=(-b+sqrt((b*b)-(4*a*c)))/(2*a)
  root2=(-b-sqrt((b*b)-(4*a*c)))/(2*a)
  print(paste("Roots of the equation: ",root1,"&",root2))
}
my_quadratic(1,5,6)
