### Lista 2 - Tais Bellini - 205650 ###

library(ggplot2)
library(dplyr)

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

#### Questao 3 ####

install.packages("mixtools")
library(mixtools)

##a 
m = 3
n = 40
u = rbind(c(0,0,0), c(3,3,3))
sigma = matrix(1,2,3)

w1 = 0.7
w2 = 1-w1

set.seed(205650)
X = rmvnormmix(n, lambda=c(w1,w2), mu=u, sigma=sigma)

#verificando a dist de V1
X1 = as.data.frame(X)
ggplot(X1, aes(x=V1)) + geom_density()

#Vamos inicializar aqui os chutes iniciais para as simulacoes:

w0_ci = c(0.6, 0.4)
u0_ci = c(1,1,1,4,4,4)

##b

#Verossimilhanca

f = function(u,x){
  w = c(w1,w2)
  u1 = u[1:3]
  u2 = u[3:6]
  sum(log(w[1]*dmvnorm(x-u1) + w[2]*dmvnorm(x-u2)))
}

#Simulated Annealing
opt.sa = function(iter, x, u0) {
  theta = matrix(0,nrow=iter,ncol=length(u0))
  theta[1,] = u0
  f.eval = rep(0, iter)
  f.eval[1] = f(theta[1,],x)
  
  for(i in 2:iter){
    temp = 1/log(1+i)
    d = 0.1*sqrt(temp) #0.1*sqrt(temp)
    e = rnorm(6)*d
    te1 = theta[(i-1),]+e
    u = runif(1)
    prob = min(exp((f(te1,x)-f(theta[(i-1),],x))/temp),1)
    theta[i,] = (u <= prob)*te1 + (u > prob)*theta[(i-1),]    
    f.eval[i] = f(theta[i,],x)
  }
  
  #encontra o máximo
  pos = which.max(f.eval)   
  theta.ot = theta[pos,]    
  plot.ts(theta[,1]);plot.ts(theta[,2])
  return(list("theta_hat" = theta.ot, "history_u" = theta ))
  
}

sa_res = opt.sa(5000, X, u0_ci)
sa_res

##c
#ref: https://www.youtube.com/watch?v=qMTuMa86NzU

#Passo E
# Para o Passo E, calculamos a E(Zi|X=x, mu = mu). 
#Para pi1, temos: P(Zi=1|X=x, mu = mu)/PTotal. Para pi2, temos: P(Zi=0|X=x, mu = mu)/PTotal.

passoE = function(X, mu, w0) {
  p_i1 = (w0[1]*dmvnorm(X, mu = mu[1:3]) / 
           (w0[1]*dmvnorm(X, mu = mu[1:3]) + w0[2]*dmvnorm(X, mu = mu[4:6])))
  p_i2 = w0[2]*dmvnorm(X, mu = mu[4:6]) / 
    (w0[1]*dmvnorm(X, mu = mu[1:3]) + w0[2]*dmvnorm(X, mu = mu[4:6]))
  
  return(cbind(p_i1,p_i2))
}


# theta1_hat = sum(pi1*X)/sum(pi1)
# theta2_hat = sum(pi2*X)/sum(pi2)
passoM = function(p_i, x) {
    m1 <- sum(p_i[,1]) # sum(pi1)
    m2 <- sum(p_i[,2])  #sum(pi2)
    
    theta_hat1 = theta_hat2 = c(rep(0, ncol(x)))
    
    #atualizacao de pi (pesos estimados para cada media)
    w1 <- m1/(m1 + m2) 
    w2 <- m2/(m1 + m2) 
    
    
    #atualizacao de u1 e u2
    theta_hat1 = sapply(1:ncol(x), function(i){
      sum(p_i[,1]*x[,i])/m1
    })
    
    theta_hat2 <- sapply(1:ncol(x), function(i){ #estimando vetor de médias da mistura 2
      sum(p_i[,2]*x[,i])/m2
    })
    
    
    return(list("u" = c(theta_hat1, theta_hat2), "w" = c(w1,w2)))
}

opt.em = function(w0, u0, x, iter){ 
  
  cc = 1
  i = 2
  
  #salva historico das estimacoes
  history_u = matrix(0, nrow = 2*ncol(x), ncol = iter)
  history_u[,1] = u0
  history_w = matrix(0, nrow = length(w0), ncol = iter)
  history_w[,1] = w0
  
  while (cc > 0.00001 & i < iter){
    exp = passoE(x, u0, w0)
    max = passoM(exp, x)
    
    u1 = max$u
    history_u[,i] = max$u
    cc = sum((u0 - u1)^2)
    u0 = u1
    
    w0 = max$w
    history_w[,i] = max$w
    
    i = i+1
  }
  
  return(list("theta_hat" = u0, "history_u" = history_u[,1:i], "history_w" = history_w[,1:i]))
    
}

#Executar o algoritmo

#EM
EM_res = opt.em(w0_ci, u0_ci, X, 500)
EM_res


##d

logf = function (u, x) {
  u1 = u[1:3]
  u2 = u[4:6]
  sum(log((w1*dnorm(x-u1)) + (w2*dnorm(x-u2))))
}


opt_res = optim(par = u0_ci, fn = logf, x = X, method = 'Nelder-Mead', control = list(fnscale = -1))$par
opt_res


##e 

u_real = c(0, 0, 0, 3 , 3, 3)

#medidas: tempo, media desvio padrao das respostas, numero de iteracoes ate convergir
compare = matrix(ncol = 4, nrow = 3)
rownames(compare) = c("SA", "EM", "optim")
colnames(compare) = c("Time Good Guess", "Error Good Guess", "Time Bad Guess", "Error Bad Guess")


iter = 500

#executando com chutes iniciais proximos
u0_ci_gg = c(0.8, 0.8, 0.8, 2.2, 2.2, 2.2)
u0_ci_bg = c(-3, -3, -3, 8, 8, 8)
w0_ci_gg = c(0.6, 0.4)
w0_ci_bg = c(0.5, 0.5)

t0 = Sys.time()
sa_comp = opt.sa(iter, X, u0_ci_gg)
compare[1,1] = Sys.time()-t0

media_erro2 = mean((sa_comp$theta_hat-u_real))^2
compare[1,2] = media_erro2

t0 = Sys.time()
em_comp = opt.em(w0_ci_gg,u0_ci_gg,X,iter)
compare[2,1] = Sys.time()-t0

media_erro2 = mean((em_comp$theta_hat-u_real))^2
compare[2,2] = media_erro2

t0 = Sys.time()
optim_comp = optim(par = u0_ci_gg, fn = logf, x = X, method = 'Nelder-Mead', control = list(fnscale = -1))
compare[3,1] = Sys.time()-t0

media_erro2 = mean((optim_comp$par-u_real))^2
compare[3,2] = media_erro2

#Executando com chutes iniciais piores

t0 = Sys.time()
sa_comp = opt.sa(iter, X, u0_ci_bg)
compare[1,3] = Sys.time()-t0

media_erro2 = mean((sa_comp$theta_hat-u_real))^2
compare[1,4] = media_erro2

t0 = Sys.time()
em_comp = opt.em(w0_ci_bg,u0_ci_bg,X,iter)
compare[2,3] = Sys.time()-t0

media_erro2 = mean((em_comp$theta_hat-u_real))^2
compare[2,4] = media_erro2

t0 = Sys.time()
optim_comp = optim(par = u0_ci_bg, fn = logf, x = X, method = 'Nelder-Mead', control = list(fnscale = -1))
compare[3,3] = Sys.time()-t0

media_erro2 = mean((optim_comp$par-u_real))^2
compare[3,4] = media_erro2

compare = as.data.frame(compare)
compare

#Tempo good guess
ggplot(aes(x=(row.names(compare)), y=compare[,1]), data=compare) + geom_point()

#Erro good guess
ggplot(aes(x=(row.names(compare)), y=compare[,2]), data=compare) + geom_point()

#Tempo bad guess
ggplot(aes(x=(row.names(compare)), y=compare[,3]), data=compare) + geom_point()

#Erro bad guess
ggplot(aes(x=(row.names(compare)), y=compare[,4]), data=compare) + geom_point()


##f 

#Reinicializando as variaveis
m = 3
n = 40
u = rbind(c(0,0,0), c(1,1,0))
sigma = matrix(1,2,3)

w1 = 0.7
w2 = 1-w1

set.seed(205650)
X_f = rmvnormmix(n, lambda=c(w1,w2), mu=u, sigma=sigma)

#verificando a dist de V1
X1_f = as.data.frame(X)
ggplot(X1_f, aes(x=V1)) + geom_density()

#medidas: tempo, media desvio padrao das respostas, numero de iteracoes ate convergir
compare_f = matrix(ncol = 4, nrow = 3)
rownames(compare_f) = c("SA", "EM", "optim")
colnames(compare_f) = c("Time Good Guess", "Error Good Guess", "Time Bad Guess", "Error Bad Guess")

iter = 500
u_real_f = c(0, 0, 0, 1, 1, 0)

#executando com chutes iniciais proximos
u0_ci_gg = c(0.8, 0.8, 0.8, 1.8, 1.8, 1.8)
u0_ci_bg = c(-3, -3, -3, 5, 5, 5)
w0_ci_gg = c(0.6, 0.4)
w0_ci_bg = c(0.5, 0.5)

t0 = Sys.time()
sa_comp_f = opt.sa(iter, X_f, u0_ci_gg)
compare_f[1,1] = Sys.time()-t0

media_erro2 = mean((sa_comp_f$theta_hat-u_real_f))^2
compare_f[1,2] = media_erro2

t0 = Sys.time()
em_comp_f = opt.em(w0_ci_gg,u0_ci_gg,X_f,iter)
compare_f[2,1] = Sys.time()-t0

media_erro2 = mean((em_comp_f$theta_hat-u_real_f))^2
compare_f[2,2] = media_erro2

t0 = Sys.time()
optim_comp_f = optim(par = u0_ci_gg, fn = logf, x = X_f, method = 'Nelder-Mead', control = list(fnscale = -1))
compare_f[3,1] = Sys.time()-t0

media_erro2 = mean((optim_comp_f$par-u_real_f))^2
compare_f[3,2] = media_erro2

#Executando com chutes iniciais piores

t0 = Sys.time()
sa_comp_f = opt.sa(iter, X_f, u0_ci_bg)
compare_f[1,3] = Sys.time()-t0

media_erro2 = mean((sa_comp_f$theta_hat-u_real_f))^2
compare_f[1,4] = media_erro2

t0 = Sys.time()
em_comp_f = opt.em(w0_ci_bg,u0_ci_bg,X_f,iter)
compare_f[2,3] = Sys.time()-t0

media_erro2 = mean((em_comp_f$theta_hat-u_real_f))^2
compare_f[2,4] = media_erro2

t0 = Sys.time()
optim_comp_f = optim(par = u0_ci_bg, fn = logf, x = X_f, method = 'Nelder-Mead', control = list(fnscale = -1))
compare_f[3,3] = Sys.time()-t0

media_erro2 = mean((optim_comp_f$par-u_real_f))^2
compare_f[3,4] = media_erro2

compare_f = as.data.frame(compare_f)
compare_f

#Tempo good guess
ggplot(aes(x=(row.names(compare_f)), y=compare_f[,1]), data=compare_f) + geom_point()

#Erro good guess
ggplot(aes(x=(row.names(compare_f)), y=compare_f[,2]), data=compare_f) + geom_point()

#Tempo bad guess
ggplot(aes(x=(row.names(compare_f)), y=compare_f[,3]), data=compare_f) + geom_point()

#Erro bad guess
ggplot(aes(x=(row.names(compare_f)), y=compare_f[,4]), data=compare_f) + geom_point()

optim_comp

#g

data = read.csv2("dados_geneticos.txt", sep = " ", header = T, as.is = T)
data = data[,colSums(data != 0) > 0]

passoE = function(X, mu, w0) {
  mu = u0_ci_gn
  mu1 = mu[1:ncol(X)]
  w0 = c(0.5,0.5)
  mu2 = mu[(ncol(X)+1):(ncol(X)*2)]
  p_i1 = w0[1]*dmvnorm(X, mu = mu1) / 
            (w0[1]*dmvnorm(X, mu = mu1) + w0[2]*dmvnorm(X, mu = mu2))
  p_i2 = w0[2]*dmvnorm(X, mu = mu2) / 
    (w0[1]*dmvnorm(X, mu = mu1) + w0[2]*dmvnorm(X, mu = mu2))
  
  return(cbind(p_i1,p_i2))
}


# theta1_hat = sum(pi1*X)/sum(pi1)
# theta2_hat = sum(pi2*X)/sum(pi2)
passoM = function(p_i, x) {
  m1 <- sum(p_i[,1]) # sum(pi1)
  m2 <- sum(p_i[,2])  #sum(pi2)
  
  theta_hat1 = theta_hat2 = c(rep(0, ncol(x)))
  
  #atualizacao de pi (pesos estimados para cada media)
  w1 <- m1/(m1 + m2) 
  w2 <- m2/(m1 + m2) 
  
  
  #atualizacao de u1 e u2
  theta_hat1 = sapply(1:ncol(x), function(i){
    sum(p_i[,1]*x[,i])/m1
  })
  
  theta_hat2 <- sapply(1:ncol(x), function(i){ #estimando vetor de médias da mistura 2
    sum(p_i[,2]*x[,i])/m2
  })
  
  
  return(list("u" = c(theta_hat1, theta_hat2), "w" = c(w1,w2)))
}

opt.em = function(w0, u0, x, iter){ 
  
  cc = 1
  i = 2
  
  #salva historico das estimacoes
  history_u = matrix(0, nrow = 2*ncol(x), ncol = iter)
  history_u[,1] = u0
  history_w = matrix(0, nrow = length(w0), ncol = iter)
  history_w[,1] = w0
  
  while (cc > 0.00001 & i < iter){
    exp = passoE(x, u0, w0)
    max = passoM(exp, x)
    
    u1 = max$u
    history_u[,i] = max$u
    cc = sum((u0 - u1)^2)
    u0 = u1
    
    w0 = max$w
    history_w[,i] = max$w
    
    i = i+1
  }
  
  return(list("theta_hat" = u0, "history_u" = history_u[,1:i], "history_w" = history_w[,1:i]))
  
}

#EM
d = matrix(unlist(data), ncol = ncol(data), byrow = T)
class(d) = "numeric"

u0_ci_gn = c(rep(-0.1, 78), rep(0.1, 78))
w0_ci_gn = c(0.5, 0.5)
EM_res = opt.em(w0_ci_gn, u0_ci_gn, d, 500)
EM_res$history_w[,ncol(EM_res$history_w)-1]
# classificou como sendo um cluster so (deu peso 1 para w1)

model = kmeans(data, 2)
model$cluster

compare



