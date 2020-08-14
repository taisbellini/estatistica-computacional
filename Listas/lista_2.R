### Lista 2 - Tais Bellini - 205650 ###


#### Questao 1 ####

#funcao a ser minimizada
f = function(u, x, z){ 
  u = as.matrix(u)
  sum(z*(x-u[1,])^2) + sum((1-z)*(x-u[2,])^2)
}


#Algoritmo de atualizacao em blocos
block.opt = function(u0, f, data){
  
  cc = FALSE
  data = as.matrix(data)
  res = res_temp = cbind(data, rep(0, nrow(data)))
  zindex = ncol(data)+1
  count = 0

  
  while(!cc && count < 1000){
    for (i in 1:nrow(data)){
      if (f(u0, data[i,], 0) < f(u0, data[i,], 1)) res[i,zindex] = 0 else res[i,zindex] = 1
    }
    
    if(sum(res[,zindex] != res_temp[,zindex]) == 5) cc = TRUE
    res_temp = res
    
    u0 = cbind(u0, rep(0, nrow(u0)))
    u0[1,] = colMeans(res[res[,zindex]==1,])
    u0[2,] = colMeans(res[res[,zindex]==0,])
    u0 = u0[1:ncol(data)]
    
    count = count + 1 
    
    
  }
  
  colnames(res) = c("x", "z")
  res = as.data.frame(res)
  
  return(list("res.dataframe" = res, "u" = u0))
  
}

#inicializar 2 clusters

a = rnorm(100, -1, 2)
b = rnorm(100, 9, 3)
data = c(a,b)

u0 = sample_n(as.data.frame(data), 2)
result = block.opt(u0, f, data)

ggplot(aes(x = as.numeric(row.names(result$res.dataframe)), y = x, color = z), data=result$res.dataframe) + geom_point()

result$u

#validando o resutado com um algoritmo pre pronto
kmeans <- kmeans(data, centers = 2)
result_kmeans = cbind(as.matrix(data), kmeans$cluster)
colnames(result_kmeans) = c("x", "z")
result_kmeans = as.data.frame(result_kmeans)
ggplot(aes(x=as.numeric(row.names(result_kmeans)), y=x, color = z), data=result_kmeans) + geom_point()
kmeans$centers

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
