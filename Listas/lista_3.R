## Exercicio 1 

# a - Distribuicao Pareto
# ref: http://www.eletrica.ufpr.br/pedroso/2012/TE816/Distribuicoes.pdf

pareto.func = function(alpha, beta, x){
  return((alpha*beta^alpha)/x^(alpha+1))
}

pareto.inversa = function(alpha, beta, x){
  return(beta/(1-x)^(1/alpha))
}

pareto.generator = function (alpha, beta, n) {
  u = runif(n)
  pareto = pareto.inversa(alpha, beta, u)
  return(pareto)
}

n = 1000
alpha = 1
beta = 1
pareto.gen = pareto.generator(alpha, beta, n)

ks.test(pareto.gen, pareto.func(alpha, beta, pareto.gen))

# b - Distribuicao Gumbel 

gumbel.func = function(x) {
  return (exp(-exp(-x)))
}

gumbel.inversa = function(x) {
  return (-log(-log(x)))
}

gumbel.generator = function(n){
  u = runif(n)
  gumbel = gumbel.inversa(u)
  return(gumbel)
}

n = 100
gumbel = gumbel.generator(n)

hist(gumbel)
ks.test(gumbel, gumbel.func)

# c - Distribuicao F 
# F pode ser descrita a partir de duas qui-quadrado

F.generator = function(d1,d2,n) {
  u1 = rchisq(n, d1)
  u2 = rchisq(n, d2)
  
  return ((u1/d1)/(u2/d2))
}

n = 10000
d1 = 5
d2 = 2

F.var = F.generator(d1,d2,n)
hist(F.var)


ks.test(F.var, df(F.var, d1, d2))

# d - Binomial Negativa 

k = 5
n = 100 
p = 0.3

#Gerador de Binomial Negativa: k sucessos, n experimentos, p probabilidade de sucesso
BN.generator = function(k, n, p){
  x = rep(0, n)
  y = rgeom(k*n,p)
  Y.geom = matrix(y,nrow=n,ncol=k)
  x = apply(Y.geom,1,sum)
  return(x)
}

bn = BN.generator(k,n,p)

# e - Multivariada


## Exercicio 2

# b

unif.generator = function(a, b, n){
  u = runif(n)
  x = a + b*u
  return(x)
}

# Gerar U[a-1/2;a+1/2] a partir de U[0,1]
# fx = 1*f0(x-(a+1/2))
# define a = 2

n = 1000
a = 2 - (1/2) # o a da locacao
b = 1 

unif.1 = unif.generator(a, b, n)
hist(unif.1)
summary(unif.1)

# Gerar U[0;b] a partir de U[0,1]
# fx = 1/b(f(x/b))
# a = 0, define b = 4

a = 0
b = 4

unif.2 = unif.generator(a, b, n)
hist(unif.2)

# Gerar U[a;b] a partir de U[0,1]
# fx = (1/(b-a))f0(x-a/(b-a))
# define a = 2, b = 4 
# a = a
# b = b-a

adef = 2
bdef = 4

a = adef
b = bdef-adef

unif.3 = unif.generator(a, b, n)
hist(unif.3)
