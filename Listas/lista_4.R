#### Exercicio 2 ####
funcao_p = function(x){
  return(exp(-(x[1]^2/2)-(x[2]^2/2)))
}

# Proposta: x1, x2 +- 1, 2, 3, 5, 8
proposta = function(x){
  range_step = c(1,2,3,5,8)
  range_direction = c(-1,1)
  x1 = x[1] + sample(range_step,1)*sample(range_direction,1)
  x2 = x[2] + sample(range_step,1)*sample(range_direction,1)
  return(c(x1,x2))
}

prob_aceita = function(x_new, x_old){
  return(min(funcao_p(x_new)/funcao_p(x_old), 1))
}

MCMC_run = function(start, n_MCMC){
  
  len = length(start)
  
  MCMC_d=matrix(ncol=n_MCMC, nrow=len)    
  
  MCMC_d[,1]=start
  
  it=1
  while(it < (n_MCMC-1)){
    x_old=MCMC_d[,it]
    x_new=x_old
    x_new=proposta(x_old)
      
    if(runif(1)<prob_aceita(x_new,x_old)){
      MCMC_d[,it+1]=x_new 
    }else{
      MCMC_d[,it+1]=x_old 
    }
    it=it+1
  }
  return(MCMC_d)
}

n_MCMC = 5000
MCMC_mvn = MCMC_run(c(0,0), n_MCMC)

mu = rowMeans(MCMC_mvn, na.rm = T)
sigma = apply(MCMC_mvn, 1, var, na.rm = T)
paste("Ex2 Mean: ", mu)
paste("Ex2 Var: ", sigma)

totals_x1 = table(MCMC_mvn[1,])
prob_x1 = table(MCMC_mvn[1,])/sum(totals_x1)

totals_x2 = table(MCMC_mvn[2,])
prob_x2 = table(MCMC_mvn[2,])/sum(totals_x2)

barplot(prob_x1,main="Marginal x1",ylab="Frequency",xlab="X1")
barplot(prob_x2,main="Marginal x2",ylab="Frequency",xlab="X2")

library(ggplot2)
library(plyr)

df = as.data.frame(t(MCMC_mvn), na.rm = T)
colnames(df) = c("x1", "x2")
df$x1 = as.factor(df$x1)
df$x2 = as.factor(df$x2)
# Get the frequency counts
dfc <- ddply(df, c("x1", "x2"), "nrow", .drop = F)
dfc = na.omit(dfc)

ggplot(data = dfc, aes(x = x1, y = x2, size = factor(nrow))) + 
  geom_point()

remove(list = ls())

#### Exercicio 3 ####
install.packages("coda")
library(coda)

funcao_p=function(x){
  p = 0
  for(i in 1:length(x)){
    p = p+x[i]
  }
  return(p^5)
}

valor_x = function(x) {
  size = length(x)
  val_x = 0
  for (i in 1:size){
    val_x = val_x + 10^(size-i)*x[i]
  }
  return(val_x)
}

proposta=function(xi){    
  candidatos=c(0:9)
  candidatos=candidatos[-which(candidatos==xi)]
  return(sample(candidatos,1))
}

prob_aceita=function(x_old,x_new){
  return(min(funcao_p(x_new)/funcao_p(x_old), 1))
}

MCMC_run = function(start, n_MCMC){

  len = length(start)
  
  MCMC_d=matrix(ncol=n_MCMC, nrow=len)    
  
  MCMC_d[,1]=start
  
  it=1
  while(it < (n_MCMC-1)){
    x_old=MCMC_d[,it]
    x_new=x_old
    for (i in 1:len){
      x_new[i]=proposta(x_old[i])
      
      if(runif(1)<prob_aceita(x_old,x_new)){
        MCMC_d[,it+1]=x_new 
      }else{
        MCMC_d[,it+1]=x_old 
      }
      it=it+1
      if (it == n_MCMC){
        break
      }
    }
  }
  return(MCMC_d)
}

## MCMC da aula
n_MCMC=2500
MCMC_aula = MCMC_run(c(0,0,0), n_MCMC)

plot( MCMC_aula[1,-c(1:n_MCMC/10)],type="l")
hist( MCMC_aula[1,-c(1:n_MCMC/10)])

X_aula = apply(MCMC_aula, 2, valor_x)  #montando os numeros em função dos algarismos
plot(X_aula)
hist(X_aula,nclass=100)
hist(X_aula,nclass=500)
paste("Mean Aula: ", mean(X_aula,na.rm=T))
paste("Var Aula: ", var(X_aula,na.rm=T))
paste("Sd Aula: ", sd(X_aula,na.rm=T))

f_x = apply(MCMC_aula, 2, funcao_p)

MCMC_obj_aula=rbind(MCMC_aula, X_aula, f_x)
row.names(MCMC_obj_aula)=c("centena","dezena","unidade","valor x","f(x)")

x = mcmc(na.omit(t(MCMC_obj_aula)))
summary(x)
par(mar = rep(2, 4))
plot(x)
effectiveSize_aula = effectiveSize(x)
effectiveSize_aula
#centena   dezena  unidade  valor x     f(x) 
#4378.211 2889.116 2345.437 3986.744 1039.236 

# Ou seja, 50000 iteracoes foram equivalentes a menos de 4000 amostras independentes do valor x.


## MCMC exercicios (5 elementos)
n_MCMC=3000     
MCMC_ex = MCMC_run(c(0,0,0,0,0), n_MCMC)

plot( MCMC_ex[1,-c(1:n_MCMC/10)],type="l")
hist( MCMC_ex[1,-c(1:n_MCMC/10)])

X_ex = apply(MCMC_ex, 2, valor_x)
par(mar = rep(2, 4))
plot(X_ex)
hist(X_ex,nclass=100)
hist(X_ex,nclass=500)
paste("Mean Exercicio: ", mean(X_ex,na.rm=T))
paste("Var Exercicio: ", var(X_ex,na.rm=T))
paste("Sd Exercicio: ", sd(X_ex,na.rm=T))

MCMC_ex=rbind(MCMC_ex,X,apply(MCMC_ex,2,funcao_p))
row.names(MCMC_ex)=c("dezena de milhar", "unidade de milhar", "centena","dezena","unidade","valor x","f(x)")

x_ex = mcmc(na.omit(t(MCMC_ex))) #cria elemento coda.mcmc
summary(x_ex)
effectiveSize_ex = effectiveSize(x_ex)
effectiveSize_ex
#dezena de milhar unidade de milhar           centena            dezena           unidade 
#3642.390          2898.682          2225.900          2238.239          2027.050 
#valor x              f(x) 
#3843.802           810.572 

# Da mesma forma que no exemplo anterior, 50000 iteracoes foram equivalentes a menos de 4000 amostras independentes do valor x.

#Nos dois casos, entre 2500-3000 repeticoes sao necessarias para um tamanho amostral efetivo de 200.
# A interpretacao eh que nos dois casos todos os indices sao atualizados com a mesma probabilidade, 
# portanto a aleatorizacao possui o mesmo efeito


remove(list = ls())

#### Exercicio 4 ####

# Parametros e dados a serem usados
## Definição dos parametros da priori (maiusculo para identificar var global)
M0=0
M1=0
T0=1/200
T1=1/200
ALPHA=1
BETA=0.2


#Simulação dos dados
set.seed(13)
x=runif(50,1,9)
y=5-0.5*x +rnorm(50)


# a) Posteriori: Verossimilhanca conjunta x densidade conjunta

# Recebe um vetor de parametros par =[b0, b1, sigma]
f_posteriori = function(par){
  res = NA
  b0 = par[1]
  b1 = par[2]
  sigma = par[3]
  
  if(sigma > 0){
    res = sum(dnorm(y, (b0+b1*x), sqrt(1/sigma), log= T) + 
                dnorm(b0, M0, sqrt(1/T0), log = T) + 
                dnorm(b1, M1, sqrt(1/T1), log = T) + 
                dgamma(sigma, ALPHA, BETA, log = T))
  }
  return(res)
}

# b) Metropolis Hastings 
# qij ~ N(i, s^2)

library(coda)

# proposta: atualiza um de cada vez de forma circular
proposta = function(par_old, sigma2 = 0.5){ 
  prop = rnorm(1, mean = par_old, sd = sqrt(sigma2))
  return(prop)
}

prob_aceita = function(par_old, par_new){ 
  p_old = exp(f_posteriori(par_old))
  p_new = exp(f_posteriori(par_new))
  return(min((p_new/p_old), 1))
}

# Algoritmo
run_MCMC = function(par0, sigma_proposta, n_MCMC){
  
  MCMC_par = matrix(NA, nrow = n_MCMC, ncol = 3)
  MCMC_par[1,] = par0
  
  #parametro pra ser atualizado pela proposta
  par_update = 1
  
  for(it in 1:(n_MCMC-1)){    ### inicia o algoritmo
    par_old_vec = MCMC_par[it,]
    par_old = par_old_vec[par_update]
    par_new = proposta(par_old, sigma2 = sigma_proposta)
    
    # se for sigma, precisa ser > 0
    if (par_update == 3) par_new = abs(par_new)
    
    
    par_new_vec = par_old_vec
    par_new_vec[par_update] = par_new
    
    if(runif(1) < prob_aceita(par_old_vec,par_new_vec)){
      MCMC_par[it+1,] = par_new_vec 
    } else {
      MCMC_par[it+1,] = par_old_vec
    }
    par_update = ifelse(par_update == 3, 1, par_update+1)
  }
  return(MCMC_par)
  
}

#Executar o algoritmo
n_MCMC = 5000
par0 = c(0,0,1)
sigma_proposta = 0.5
MCMC_par = run_MCMC(par0, sigma_proposta, n_MCMC)


X = mcmc(MCMC_par)
par(mar = rep(2, 4))
plot(X)
summary(X)
(1 - rejectionRate(X)) # taxa de aceitacao
effectiveSize(X) # tamanho efetivo

# c) Efeito de sigma2 e o seu valor otimo

sigma2_vec = seq(0.1, 10, 0.1) # 100 valores para testar

# vetor com 6 colunas para salvar taxa de aceitacao dos 3 parametros + tamanho amostral efetivo
# estas duas medidas serao os criterios de avaliacao da convergencia do algoritmo para determinar
# o sigma2 otimo
sigma2_result_vec = matrix(NA, nrow = length(sigma2_vec), ncol = 6) # salvar os resultados
colnames(sigma2_result_vec) = c('acceptRate_b0', 'acceptRate_b1', 'acceptRate_sig', 
                                'effectiveSize_b0', 'effectiveSize_b1', 'effectiveSize_sig')

n_MCMC = 5000
set.seed(13)
for (i in 1:length(sigma2_vec)){
  MCMC_result = run_MCMC(par0, sigma2_vec[i], n_MCMC)
  
  Xc = mcmc(MCMC_result)
  
  sigma2_result_vec[i, 1:3] = (1 - rejectionRate(Xc))
  sigma2_result_vec[i, 4:6] = effectiveSize(Xc)
}

# Analise numerica: 

# Taxa de aceitacao mais proxima do 0.234 para cada par 
if(!require(DescTools)) install.packages("DescTools")
library(DescTools)

closest_to_target = function(vec){
  return(Closest(vec, 0.234, which = T))
}

closestb0 = closest_to_target(sigma2_result_vec[,1])
acceptance_rate_index = apply(sigma2_result_vec[,1:3], 2, closest_to_target)
acceptance_rate_index
#acceptRate_b0  acceptRate_b1 acceptRate_sig 
#1              1              1

sigma2_vec[acceptance_rate_index[1]]
#[1] 0.1

# A variancia com a taxa de aceitacao mais proxima de 0.234 eh 0.1

# Maior tamanho de amostra efetivo para cada par

max_index = function(vec){
  return(which(vec==max(vec)))
}

effective_sample_index = apply(sigma2_result_vec[,4:6], 2, max_index)
effective_sample_index
#effectiveSize_b0  effectiveSize_b1 effectiveSize_sig 
#30                 1                25 
sigma2_vec[effective_sample_index[1]]
#[1] 9.3
sigma2_vec[effective_sample_index[2]]
#[1] 9.3
sigma2_vec[effective_sample_index[3]]
#[1] 0.2

# analise grafica dos resultados
# graficos criados com a ajuda da colega Juliana
plot(sigma2_vec, sigma2_result_vec[,1], col = 'red', type = 'l', ylim = c(0, 0.20), 
     ylab = 'taxa de aceitação', xlab = 'variância da proposta')
lines(sigma2_vec, sigma2_result_vec[,2], col = 'blue')
lines(sigma2_vec, sigma2_result_vec[,3], col = 'green')
lines(sigma2_vec, apply(sigma2_result_vec[,1:3], 1, mean), col = 'gray', lty = 2)
legend('topright', 
       legend = c(expression(paste(beta[0])), 
                  expression(paste(beta[1])), 
                  expression(paste(phi)), 'valor médio'), 
       col = c("red", "blue", "green", "gray"), lty = c(1,1,1,2), cex = 0.8)
abline(v = sigma2_vec[which.max(apply(sigma2_result_vec[,1:3], 1, mean))], lty = 2)


plot(sigma2_vec, sigma2_result_vec[,4], col = 'red', type = 'l', ylim = c(0,300),
     ylab = 'tamanho efetivo', xlab = 'variância da proposta')
lines(sigma2_vec, sigma2_result_vec[,5], col = 'blue')
lines(sigma2_vec, sigma2_result_vec[,6], col = 'green')
lines(sigma2_vec, apply(sigma2_result_vec[,1:3], 1, mean), col = 'gray', lty = 2)
legend('topright', 
       legend = c(expression(paste(beta[0])), 
                  expression(paste(beta[1])), 
                  expression(paste(phi)), 'valor médio'), 
       col = c("red", "blue", "green", "gray"), lty = c(1,1,1,2), cex = 0.8)
abline(v = sigma2_vec[which.max(apply(sigma2_result_vec[,4:6], 1, mean))], lty = 2)

# Tamanho efetivo varia muito, portanto, vamos usar a taxa de aceitacao e considerar s2 otimo como 0.1

#d) 

res = summary(X)
res
res$statistics[,1]
#[1]  4.5046119 -0.4446029  0.8102690

# e) Gibbs Sampler

M0=0
M1=0
T0=1/200
T1=1/200
ALPHA=1
BETA=0.2


#Simulação dos dados
set.seed(13)
x=runif(50,1,9)
y=5-0.5*x +rnorm(50)
N = length(x)
X2 = sum(x^2)


b0_prop = function(par_old_vec){
  s = 1/(T0 + par_old_vec[3]*N)
  m = par_old_vec[3]*sum(y-par_old_vec[2]*x)*s
  par_new_vec = par_old_vec
  par_new_vec[1] = rnorm(1, m, sqrt(s))
  return(par_new_vec)
}

b1_prop = function(par_old_vec){
  s = 1/(T1 + par_old_vec[3]*X2)
  m = par_old_vec[3]*sum((y-par_old_vec[1])*x)*s
  par_new_vec = par_old_vec
  par_new_vec[2] = rnorm(1, m, sqrt(s))
  return(par_new_vec)
}

sig_prop = function(par_old_vec){
  beta = BETA + sum((y - par_old_vec[1] - par_old_vec[2]*x)^2)/2
  par_new_vec = par_old_vec
  par_new_vec[3] = rgamma(1, shape = ALPHA + N/2, rate = beta)
  return(par_new_vec)
}

f_posteriori = function(par){
  res = NA
  b0 = par[1]
  b1 = par[2]
  sigma = par[3]
  
  if(sigma > 0){
    res = sum(dnorm(y, (b0+b1*x), sqrt(1/sigma), log= T) + 
                dnorm(b0, M0, sqrt(1/T0), log = T) + 
                dnorm(b1, M1, sqrt(1/T1), log = T) + 
                dgamma(sigma, ALPHA, BETA, log = T))
  }
  return(res)
}


proposta = list(b0_prop, b1_prop, sig_prop)

post = vector()
gibbs_sampler = function(par0, n_MCMC){
  
  MCMC_par = matrix(ncol = n_MCMC, nrow = 3)     
  MCMC_par[,1] = par0
  
  post[1] = f_posteriori(MCMC_par[,1])
  
  #parametro pra ser atualizado pela proposta
  par_update = 1
  
  for(it in 1:(n_MCMC-1)){    ### inicia o algoritmo
    par_old_vec = MCMC_par[,it]
    par_new_vec = proposta[[par_update]](par_old_vec)
    
    # se for sigma, precisa ser > 0
    if (par_update == 3) par_new_vec[3] = abs(par_new_vec[3])
    
    par_update = ifelse(par_update == 3, 1, par_update+1)
    MCMC_par[,it+1] = par_new_vec  
    post[it+1] = f_posteriori(par_new_vec)
    
  }
  
  return(MCMC_par)
  
}

n_MCMC = 5000
par0 = c(0,0,1)
set.seed(13)
MCMC_gibbs = gibbs_sampler(par0, n_MCMC)


#Comparando

par(mar = rep(2, 4))
plot(X)
summary(X)
effectiveSize(X)


X1 = mcmc(t(rbind(MCMC_gibbs,post)))
par(mar = rep(2, 4))
plot(X1)
summary(X1)
effectiveSize(X1)


plot(MCMC_gibbs[1,-c(1:100)],MCMC_gibbs[2,-c(1:100)], col="blue")

plot(MCMC_gibbs[1,],MCMC_gibbs[2,], col="blue")

lines(MCMC_gibbs[1,1:10],MCMC_gibbs[2,1:10])
lines(MCMC_gibbs[1,1:40],MCMC_gibbs[2,1:40])
lines(MCMC_gibbs[1,1:300],MCMC_gibbs[2,1:300])

res = summary(X1)
res
res$statistics[,1]
#[1]   4.7586548   -0.4867658    1.2259163


