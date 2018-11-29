#This code fits a neural network with 2 inputs, 
#3 nodes in a single hidden layer, and one output.  


x1 = c(0,0,1,1)
x2 = c(0,1,0,1)
y = c(0,1,1,0)

#x1 = c(2,4,1,1)
#x2 = c(3,2,4,1)
#y = c(1,2,3,4)


#activation Function 
sigma <- function(x){
  out <- 1/(1+exp(-x))
  return(out)
}

#Derivative of the activation function 
sigma_prime <- function(x){
  out <- exp(-x)/(1+exp(-x))^2
  return(out)
}

gamma <- 1

#Variable Transofrmations
z1 <- function(x1, x2){
  out <- sigma(alpha[1,1] + alpha[1,2]*x1 + alpha[1,3]*x2)
  return(out)
}

z2 <- function(x1, x2){
  out <- sigma(alpha[2,1] + alpha[2,2]*x1 + alpha[2,3]*x2)
  return(out)
}

z3 <- function(x1, x2){
  out <- sigma(alpha[3,1] + alpha[3,2]*x1 + alpha[3,3]*x2)
  return(out)
}

f <- function(x1, x2){
  out <- beta[1] + beta[2]*z1(x1, x2) + beta[3]*z2(x1, x2)
  return(out)
}
#Learning rate
 g <- 0.1
#partial R wrt  beta
#beta1
-2*(y - f(x1,x2))*z1(x1,x2)
#beta2
-2*(y - f(x1,x2))*z2(x1,x2)
 
 
#Initial Values for the weights
beta <- c(0,0,0,0)+0.5
alpha <- rbind(c(0,0,0),c(0,0,0),c(0,0,0))+0.5


for (i in 1:1000){
beta[1] <- beta[1] - g*sum(-2*(y - f(x1,x2)))
beta[2] <- beta[2] - g*sum(-2*(y - f(x1,x2))*z1(x1,x2))
beta[3] <- beta[3] - g*sum(-2*(y - f(x1,x2))*z2(x1,x2))
beta[4] <- beta[4] - g*sum(-2*(y - f(x1,x2))*z3(x1,x2))

#partial R wrt alpha
#l = 1
alpha[1,1] <- alpha[1,1]-g*sum(-2*(y - f(x1,x2))*beta[2]*sigma_prime(alpha[1,1] + alpha[1,2]*x1 + alpha[1,3]*x2))
alpha[1,2] <- alpha[1,2]-g*sum(-2*(y - f(x1,x2))*beta[2]*sigma_prime(alpha[1,1] + alpha[1,2]*x1 + alpha[1,3]*x2)*x1)
alpha[1,3] <- alpha[1,3]-g*sum(-2*(y - f(x1,x2))*beta[2]*sigma_prime(alpha[1,1] + alpha[1,2]*x1 + alpha[1,3]*x2)*x2)

alpha[2,1] <- alpha[2,1]-g*sum(-2*(y - f(x1,x2))*beta[3]*sigma_prime(alpha[2,1] + alpha[2,2]*x1 + alpha[2,3]*x2))
alpha[2,2] <- alpha[2,2]-g*sum(-2*(y - f(x1,x2))*beta[3]*sigma_prime(alpha[2,1] + alpha[2,2]*x1 + alpha[2,3]*x2)*x1)
alpha[2,3] <- alpha[2,3]-g*sum(-2*(y - f(x1,x2))*beta[3]*sigma_prime(alpha[2,1] + alpha[2,2]*x1 + alpha[2,3]*x2)*x2)

alpha[3,1] <- alpha[3,1]-g*sum(-2*(y - f(x1,x2))*beta[4]*sigma_prime(alpha[3,1] + alpha[3,2]*x1 + alpha[3,3]*x2))
alpha[3,2] <- alpha[3,2]-g*sum(-2*(y - f(x1,x2))*beta[4]*sigma_prime(alpha[3,1] + alpha[3,2]*x1 + alpha[3,3]*x2)*x1)
alpha[3,3] <- alpha[3,3]-g*sum(-2*(y - f(x1,x2))*beta[4]*sigma_prime(alpha[3,1] + alpha[3,2]*x1 + alpha[3,3]*x2)*x2)


print(f(x1,x2))

}
