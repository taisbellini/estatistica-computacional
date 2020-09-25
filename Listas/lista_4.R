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