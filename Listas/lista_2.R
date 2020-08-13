### Lista 2 - Tais Bellini - 205650 ###

#### Questao 2 ####

## a 
x = runif(200, 2, 40)
epslon = rnorm(200)
beta = c(60, -0.05)

y = beta[1]*exp(beta[2]*x)+epslon


## b
# ref: https://pt.wikipedia.org/wiki/Algoritmo_de_Gauss-Newton

J = function (beta, x) {
  return(cbind(-exp(beta[2]*x), -beta[1]*x*exp(beta[2]*x)))
}

r = function(beta, x, y) {
  return(y-beta[1]*exp(beta[2]*x))
}


gauss_newton = function (beta_0, J, r, x, y) {
  
  #aux to store execution
  z = beta_0
  
  #aux cc
  i = 0
  
  cc = 1
  while(cc > 0.0001 && i<1000) {
    beta_i = beta_0 - solve(t(J(beta_0, x))%*%J(beta_0, x))%*%t(J(beta_0, x))%*%r(beta_0, x, y)
    cc = sum((beta_i - beta_0)^2)
    beta_0 = c(beta_i)
    z = cbind(z, beta_i)
  }
  return (list(beta_hat = beta_i, z = z))
  
}

beta_0 = c(50, -0.10)
beta_hat = gauss_newton(beta_0, J, r, x, y)

colnames(beta_hat$z) = paste("i",1:ncol(beta_hat$z),sep="")
rownames(beta_hat$z) = c("beta0-gn", "beta1-gn")

##c

f = function(beta,x,y){
  sum((y-beta[1]*exp(beta[2]*x))^2)
}

d1 = function(beta, x){
  A = rbind(0,0)
  A[1] = sum(2*(y-beta[1]*exp(beta[2]*x))*(-exp(beta[2]*x)))
  A[2] = sum(2*(y-beta[1]*exp(beta[2]*x))*(-beta[1]*x*exp(beta[2]*x)))
  return(A)
}
  
d2 = function (beta, x) { 
  A = matrix(0,nrow=length(beta),ncol=length(beta))
  A[1,1] = sum(-2*exp(beta[2]*x)*(-exp(beta[2]*x)))
  A[2,2] = sum(2*(-beta[1]*x*exp(beta[2]*x))*(-beta[1]*x*exp(beta[2]*x)))
  + sum(2*(y-beta[1]*exp(beta[2]*x))*(-beta[1]*x^2*exp(beta[2]*x)))
  A[2,1] = A[1,2] = sum(2*(-beta[1]*x*exp(beta[2]*x))*(-exp(beta[2]*x)))
  + sum(2*(y-beta[1]*exp(beta[2]*x))*(-x*exp(beta[2]*x)))
  return(A)
}

newton_raphson = function(beta_0,d1,d2, x, e=0.0001){
  
  z = beta_0
  i = 0
  
  cc = 1
  while(cc>e && i<1000){
    beta_i = beta_0 - solve(d2(beta_0, x))%*%d1(beta_0, x)
    cc = sum((beta_i-beta_0)^2)
    beta_0 = beta_i
    i=i+1
    z = cbind(z,beta_i)
  }
  return(list(beta_hat = beta_i, z = z))
}

beta_hat.nr = newton_raphson(beta_0, d1, d2, x)
colnames(beta_hat.nr$z) = paste("i",1:ncol(beta_hat$z),sep="")
rownames(beta_hat.nr$z) = c("beta0-nr", "beta1-nr")

beta_0.comp = rbind(beta_hat$z[1,], beta_hat.nr$z[1,])
beta_0.comp

beta_1.comp = rbind(beta_hat$z[2,], beta_hat.nr$z[2,])
beta_1.comp


matplot(t(beta_0.comp), type='l')
