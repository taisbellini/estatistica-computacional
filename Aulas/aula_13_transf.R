## Transformação integral de probabilidade
### Exercício 1: 

n = 1000
u = runif(n)

#Inversa de f(x) = 1/8x
x = 4*sqrt(u)

# FDA a partir da integral de f(x)
F=function(x){  ### OK pois todos os valores estão entre 0 e 4.
  y=x^2/16
}

ks.test(x,F)
hist(x)

### Exercício 2: 

n = 1000
u = runif(n)
la = 3
#inversa
x = -log(u)/la #u tem distribuicao Uniforme e 1-u tambem, por isso esse e o do caderno dao certo
ks.test(x,"pexp",rate=la)
hist(x, probability = T)
lines(sort(x),dexp(sort(x),rate=la))


# Exercicio adicional
u = runif(1000)
x = 4*u-2
hist(x)

ks.test(x, "punif")

### Exercicio 3

p.bin = function(x,n,p){
  choose(n,x)*(p^x)*(1-p)^(n-x)
} 
###
n = 10
p = 0.3
x = 0:n
fmp.bin = p.bin(x,n,p)
plot(x, fmp.bin)
probs = cumsum(fmp.bin)
ns = 10000
y = rep(0,ns)
for(i in 1:ns){
  u = runif(1)  
  pos = sum(u > probs) + 1 #vetor de probs dizendo se eh maior que o P. +1 pra ficar no intervalo certo
  y[i] = x[pos]
}

plot(table(y))
mean(y)


### Exercício 4

p.pois = function(x,la){
  fmp = (exp(-la)*la^x)/factorial(x)
  fmp
}

limsup = 0.999985
cc = 0
z = 0 
la = 3
probs.aux = NULL

while(cc <= limsup){
  probs.aux = c(probs.aux,p.pois(z,la))
  cc = sum(probs.aux)
  z = z+1
}

probs.cum = cumsum(probs.aux)
probs.poisson = probs.cum[probs.cum <= limsup]  


### Opção 1: considera o que passou do limite como o ultimo
gna.disc2 = function(x,prob.fmp,n){
  y = rep(0,n)
  probs = cumsum(prob.fmp)
  
  for(i in 1:n){
    u = runif(1)  
    pos = min(c(sum(u > probs) + 1), length(x))  
    y[i] = x[pos]
  }
  y
}


### Opção 2: se cai fora do limite, repete a operacao
#isto acaba gerando um sequencia de numeros diferentes, mesmo com a semente, 
#pois estamos 'saindo da ordem'

gna.disc3 = function(x,prob.fmp,n){
  y = rep(0,n)
  probs = cumsum(prob.fmp)
  i = 0
  while(i < n){
    u = runif(1)  
    pos = sum(u > probs) + 1
    if(pos <= length(x)){
      i = i+1
      y[i] = x[pos]
    }
  }
  y
}

## Comparando as opções

z = 0:length(probs.poisson)
ns = 1000
la = 3

set.seed(10)
x1 = gna.disc2(z,p.pois(z,la),ns)
set.seed(10)
x2 = gna.disc3(z,p.pois(z,la),ns)
set.seed(10)
x3 = rpois(ns,la)

sum(x1 == x3) 
sum(x2 == x3)
# fica igual pois aquela ultima prob eh tao pequena que em 1000 repeticoes nao aconteceu



#######################  Distribuição Biomial e teste de aderencia

n = 10
v.assumidos = 0:n
p = 0.3

probs.bin = dbinom(v.assumidos,size=n,prob = p)

# para ns=500 esperamos a seguinte frequencia
ns = 500
probs.bin*ns


## Função que faz a contagem
conta = function(v.assum,x){
  z = v.assum
  nz = length(z)
  res = rep(0,nz)
  names(res) = paste(z)
  for(i in 1:nz){
    res[i] = sum(x==z[i])  
  }
  return(res)
}


## Comparando com esperado e table
v.assum = 0:10
n = 10
p = 0.3
ns = 500
fmp.bin = p.bin(x,n,p)
x = gna.disc3(v.assum,fmp.bin,ns)

t1 = conta(v.assum,x); t1
t2 = table(x); t2

plot(t1,type="h")
points(fmp.bin*ns,col=2)

plot(table(x))


# Definindo funcao que faz teste
aderencia = function(freq.obs,freq.esp){
  k = length(freq.obs)
  est.teste = sum(((freq.obs-freq.esp)^2)/freq.esp)
  valor.p = pchisq(q=est.teste,df=(k-1),lower.tail=F)
  list("Estatistica.teste"= est.teste,"valor.p"=valor.p)
}


# faz teste p rbinom
set.seed(1234)
ns = 300
n = 10
p = 0.3
x.ver = 0:n
x.a = rbinom(ns,n,p)

freq.e = p.bin(x.ver,n,p)*ns
freq.o = conta(x.ver,x.a)

aderencia(freq.o,freq.e)
