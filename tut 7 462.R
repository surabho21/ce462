
#question2
q <- seq(from=0.05,to=0.25,length=2000)
q_square <- q^2
 
z <- rep(40,length(q))

h <- z+ 2045.61*q_square
plot(q,h)


#question 3

q_2 <- seq(from=0,to=0.25,length=2500)
h_original <- 90-500*q_2^2

h_total <- 90-571.56*q_2^2-7.78*q_2^1.852

plot(q_2,h_total)

plot(q_2,h_original)

dat <- data.frame(q,h,h_total)
#question 4
f <- function(q_11){
  f<-50-2617.17*q_11^2-7.78*q_11^1.852
  return(f)
}

f_dash <- function(q_11){
  f_dash <- -5234.34*q_11-14.40*q_11^0.852
  return(f_dash)
}

x_initial <- 0.05
 x_next <- x_initial - f(x_initial)/f_dash(x_initial)
error <- ((x_next-x_initial)/x_initial)*100
 run<- 0
while(abs(error)>=0.1){
  x_initial <- x_next
  x_next <- x_initial - f(x_initial)/f_dash(x_initial)
  error <- ((x_next-x_initial)/x_initial)*100
  run <- run+1
}
x_next
error

