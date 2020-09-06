####################################  Teste de permutação


Wa=c(1.72,1.64,1.74,1.7,1.82,1.82,1.9,1.82,2.08)
Aa=c(1.24,1.38,1.36,1.4,1.38,1.48,1.38,1.54,1.56)

Wp=c(1.78,1.76,1.96,2,2,1.96)
Ap=c(1.14,1.2,1.3,1.26,1.28,1.18)


### Permutation test for W

t=function(A,B){
  (mean(A)-mean(B))/sqrt(var(c(A,B)*(1/9+1/6)))
}

t_stat=t(Wa,Wp) # estatística do teste

B=10000
na=9
nb=6
W=c(Wa,Wp)  # vetor combinado
ts=numeric(B)
for(b in 1:B){   #inicia as replicações
  wstar=sample(W,replace=F)
  ts[b]=t(wstar[1:na],wstar[(na+1):(na+nb)])
}

hist(ts)
abline(v=t_stat)
mean(ts<t_stat)

#### Exercício: Fazer o mesmo para exemplo bivariado

######################## Importance ressampling

