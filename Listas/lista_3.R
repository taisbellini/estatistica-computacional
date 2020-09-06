## Exercicio 1 

# a - Distribuicao Pareto
# ref: http://www.eletrica.ufpr.br/pedroso/2012/TE816/Distribuicoes.pdf
install.packages("EnvStats")
library(EnvStats)


pareto.inversa = function(alpha, beta, x){
  return(beta/(1-x)^(1/alpha))
}

pareto.generator = function (alpha, beta, n) {
  u = runif(n)
  pareto = pareto.inversa(alpha, beta, u)
  return(pareto)
}

n = 1000
alpha = 7
beta = 5
pareto.gen = pareto.generator(alpha, beta, n)

pareto = rpareto(n, alpha, beta)
hist(pareto, xlim = c(0,1000))
hist(pareto.gen, xlim = c(0,1000))

ks.test(pareto.gen, rpareto(n, alpha, beta))

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

nr = 1000

#range de x e y
n = 10

#p arbitrario
p = 0.7

fmp_conjunta = function(x, y, n, p)
{
  return( choose(n,x) * choose(n,y) * ( ( x^y * (n - x)^(n - y) * p^x * (1-p)^(n - x) ) / (n^n) ) )
}

# Matriz com todas as combinacoes possiveis de x e y e sua prob

x = 1:n
y = 1:n
ea = expand.grid(x,y)
probs = cbind(ea, 0)
colnames(probs) = c('x', 'y', 'prob')

for (i in 1:length(probs[,1])){
  probs[i,3] = fmp_conjunta(probs[i,1], probs[i,2], n, p)
}

fda = cumsum(probs[,3])
fda = c(fda, 1)
plot(fda)

bivar.gen = matrix(nrow = nr, ncol = 2)
colnames(bivar.gen) = c('x', 'y')

for (i in 1:nr) {
  u = runif(1)
  pos = sum(u>fda)
  bivar.gen[i,1] = probs[pos,1]
  bivar.gen[i,2] = probs[pos,2]
}

bivar.gen = cbind(bivar.gen, 0)

for (i in 1:length(bivar.gen[,1])){
  bivar.gen[i,3] = fmp_conjunta(bivar.gen[i,1], bivar.gen[i,2], n, p)
}

hist(bivar.gen[,1])
hist(bivar.gen[,2])


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


## Exercicio 3 

# a - E(X)

# utilizando o metodo naive 

set.seed(205650)
nu = 100
nr = 5000
X = rep(0, nr)

for(i in 1:nr){ 
  u = runif(nu)
  j = 2
  x = u[1] + u[2]
  while(x <= 1){
    j = j+1
    x = x + u[j]
  }
  X[i] = j
}

# estimador
xbarra = mean(X)
xbarra
hist(X)
#[1] 2.708

# erro 
vn = (1/nr^2)*sum((X-xbarra)^2)
vn 
#[1] 0.001549472
sd = sqrt(vn)
sd
#[1] 0.01223201


# b - P(X>=10)
# https://www.dam.brown.edu/lcds/publications/fulltext/2006-003.pdf

#naive
p_greater10 = sum(X>=10)/nr
p_greater10
# apenas o metodo naive nao funciona, pois estamos querendo estimar uma cauda
# assim, a probabilidade de se gerar um valor neste range eh mt prox de zero

# Importance Sampling

# usando beta como g(x), vamos utilizar uma parametrizacao que tenha alta 
#probabilidade de gerar valores pequenos, assim, temos mais chance de o n  
# da soma ser > 10

set.seed(205650)

shape1 = 1
shape2 = 8
k = 9
n = 1000

# testando a parametrizacao
beta = rbeta(k, shape1, shape2)
hist(beta)

sim = matrix(nrow = n, ncol = k)


for (i in 1:n) {
  sim[i,] = rbeta(k, shape1, shape2) 
}

# indicadora se temos a soma menor ou igual a 1 das k variaveis geradas
hx = apply(sim, 1, sum) <= 1

#densidade da uniforme padrao
fx = 1

# conjunta da beta
fdp_beta = apply(sim, 2, function (x) { dbeta(x, shape1, shape2) })
gx = apply(fdp_beta, 1, prod)

est_vec = (hx*fx)/gx
est = mean(est_vec)
paste ('Estimativa: ', est)
# [1] 2.71119972633006e-06

#erro
vn = (1/n^2)*sum((est_vec-est)^2)
paste('Variancia da estimativa: ', vn)
# [1] 4.05721543984121e-13

sd = sqrt(vn)
sd
paste('Desvio padrao da estimativa: ', sd)
# [1] 6.36962749290821e-07


# c - P(X=10)

# Neste caso, queremos avaliar se o n = 10. Assim, vamos gerar 10 vars com 
#distribuicao beta e nossa h(x) sera a indicadora da soma de 1-9 ser <=1 e a soma 
# de 1-10 > 1

shape1 = 1
shape2 = 8
k = 10
n = 1000

sim = matrix(nrow = n, ncol = k)

for (i in 1:n) {
  sim[i,] = rbeta(k, shape1, shape2) 
}


# indicadora se temos a soma menor que 1 das k variaveis geradas
hx = (apply(sim[,1:9], 1, sum) <= 1) & ((apply(sim, 1, sum) > 1)) 

#densidade da uniforme padrao
fx = 1

# conjunta da beta
fdp_beta = apply(sim, 2, function (x) { dbeta(x, shape1, shape2) })
gx = apply(fdp_beta, 1, prod)

est_vec = (hx*fx)/gx
est = mean(est_vec)
paste ('Estimativa: ', est)
# [1] 2.25728603112163e-06

#erro
vn = (1/n^2)*sum((est_vec-est)^2)
paste('Variancia da estimativa: ', vn)
# [1] 1.48997002743075e-13

sd = sqrt(vn)
sd
paste('Desvio padrao da estimativa: ', sd)
# [1] 3.86001298887808e-07


# Exercicio 4 

X = c(6.2, 5.1, 7.6, 2.5, 3.5, 9.4, 4.1, 6.3, 3.0, 0.8)
Y = c(6.9, 5.1, 7.5, 11.1, 10.9, 4.2, 10.5, 6.8, 12.3, 14.3)

B = 3000

data = data.frame(X = X, Y = Y)

f.mqo = function(data){
  model = lm(data$Y ~ data$X)
  b0.h = model$coefficients[1]
  b1.h = model$coefficients[2]
  return(list("b0" = b0.h, "b1" = b1.h))
}

estimador_amostra = f.mqo(data)
estimador_amostra

# a - IC 95%

# vetores de b0 e b1 para as 300 amostras Bootstrap
b0b = numeric(B)
b1b = numeric(B)

for (i in 1:B){
  datab = data[sample(nrow(data), nrow(data), replace = T), ]
  est = f.mqo(datab)
  b0b[i] = est$b0
  b1b[i] = est$b1
}

# IC por normalidade

b0.sdb = sd(b0b)
b1.sdb = sd(b1b)

paste("IC por normalidade para b0: ", 
      estimador_amostra$b0 - 1.96*b0.sdb, estimador_amostra$b0 + 1.96*b0.sdb)
paste("IC por normalidade para b1: ", 
      estimador_amostra$b1 - 1.96*b1.sdb, estimador_amostra$b1 + 1.96*b1.sdb)

# IC por quantil

print("IC por quantil para b0: ")
print(quantile(b0b, c(0.025, 0.975)))

print("IC por quantil para b1: ")
print(quantile(b1b, c(0.025, 0.975)))

# b
