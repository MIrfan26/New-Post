
#1.1
f1<- function (x){
    result <- x^3 + x^2 -6
    return(result)
}

f1(4)

#1.2
g1 <- function (a,b){
    result <- a*b*(b-a)
    return (result)
} 

g1(2,4)

#1.3
h <- function (m,n){
    result <- (sqrt(m)/n)+m-2*n
return (result)
    }

h(4,2)

#2.1
a<- matrix (c(1,2,3,4),2,2,TRUE)
b<- matrix (c(5,6,7,8),2,2,TRUE)
f <- function(a,b){
    result <- ((a+b)%*%a%*%b)
return (result)
    }
f(a,b)

#2.2
m<- matrix (c(1:4),2,2,TRUE)
n<- matrix (c(5:8),2,2,TRUE)
h<- function (m,n){
    result <- det(m)*n-m%*%n
    return (result)
}
h(m,n)

#2.3
a<- matrix (c(1:4),2,2,TRUE)
g <- function (x){
    result <- solve(x)%*%x-2*x
    return(result)
}
g(a)

#1
f <- function (x){
    result <- sin(x)
    return (result)
}
plot (input <- -1:10,
     sapply(input, f),
      type="l",
      xlab="x",
     ylab = "f(x)")

#2
f <- function(x){
    result <- log(x)
    return (result)
}
input <- 1:10
plot (input ,
     sapply(input, f),
      type="l",
      xlab="x",
     ylab = "f(x)")

#3
f<- function (x){
    result <- sqrt(x)-2
    return (result)
}
plot (input,
     sapply(input, f),
      type="l", 
      xlab="x",
     ylab = "f(x)")

#4
f<- function (x){
    result <- sqrt(x-2)
    return (result)
}
input <- 5:10
plot (input ,
     sapply(input, f),
      type="l",
      xlab="x",
     ylab = "f(x)")

#Constant functions
# f(x) = a
f<- function (x){
    #suppose a=5
    result <- 5
    return (result)
}
input <- 0:10
plot(input, 
sapply (input, f),
type="l", xlab= "x",
ylab="f(x)")

#Linear functions
# f(x) = ax + b
f <- function (x){
    # a=2 dan b=4
    result<- 2*x + 2
    return(result)
}
input <- 1:10
plot (input,
sapply(input, f),
type="l", xlab="x",
ylab="f(x)")

#Quadratic functions
#f(x)= gx^2 + hx +i
f<- function (x){
#g=5 , h=3, i=2
    result <- 5*x^2 + 2*2 + 2
    return (result)
}
input <- -10:10
plot(input,
sapply(input, f),
type = "l", xlab="x",
ylab = "f(x)")

#Polynomial functions
#f(x)=4x^3-3x^2+6
f <- function (x){
    result <- 4*x^3-3*x^2+6
    return (result)
}
input <- seq(5,3)
plot(input,
sapply (input, f),
type ="l", xlab ="x",
ylab="f(x)")

#Rational functions
f<- function (x){
    result <- x/(x-3)
    return (result)
}
input <- seq (4,10,1)
plot(input,
sapply (input, f),
type ="l",
xlab ="x",
ylab = "f(x)")
