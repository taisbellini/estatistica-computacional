
### Calculando integrais no R

f = function(x) x^2
integrate(f,2,4)


## A mesma integral por Monte Carlo

n = 100
x = runif(n,2,4)
y = mean(2*x^2)
y


##  A função Gama, integração numerica e monte carlo

la = 5
f = function(x) x^(la-1)*exp(-x)

integrate(f,0,Inf)

set.seed(1)
h = function(x) x^(la-1)
n = 100
x = rexp(n,1)
mean(h(x))

gamma(la)

##  Função Gama com Uniforme auxiliar

# Encontra um ponto de corte
la = 5
f = function(x) x^(la-1)*exp(-x)
f(25)

#sounds good

n = 100
a = 0
b = 25
x = runif(n,a,b)
mean(b*f(x))

#########################################

### Erro de Monte Carlo

#set.seed(1)
la = 5
h = function(x) x^(la-1)
n = 5000
x = rexp(n,1)
y = h(x)
estint = cumsum(y)/(1:n)
vest = sqrt(cumsum((y-estint)^2))/(1:n)
limsup = estint + 2*vest
liminf = estint - 2*vest
estint[n]
vest[n]

# Os graficos
plot(estint,ylim=c(0,50),type="l")
lines(limsup,col=2)
lines(liminf,col=2)
abline(h=24,col=4)


## função
plot.band = function(y,ver){
  n = length(y)
  estint = cumsum(y)/(1:n)
  vest = sqrt(cumsum((y-estint)^2))/(1:n)
  limsup = estint + 2*vest
  liminf = estint - 2*vest
  plot(estint,ylim=c(min(liminf),max(limsup)),type="l")
  lines(limsup,col=2)
  lines(liminf,col=2)
  abline(h=ver,col=4)
  res = list("est"=estint[n],"dp"=vest[n])
  return(res)
}

########## Exercício 5


set.seed(1)
h = function(x) (cos(50*x)+sin(20*x))^2
n = 5000
x = runif(n,0,1)
y = h(x)
estint = cumsum(y)/(1:n)
vest = sqrt(cumsum((y-estint)^2))/(1:n) limsup = estint + 2*vest
liminf = estint - 2*vest

plot(estint,ylim=c(min(liminf),max(limsup)),type="l") lines(limsup,col=2)
lines(liminf,col=2)
ver = integrate(h,0,1)$value
abline(h=ver,col=4)

### Obs variancia do estimador

set.seed(1)
h = function(x) (cos(50*x)+sin(20*x))^2
n = 10000
x = runif(n,0,1)
y = h(x)
yb = mean(y)
estint = yb
vest = sqrt(sum((y-yb)^2))/n
estint
vest

########################################  Outras considerações
## Por outro lado (replicaçoes)
h = function(x) (cos(50*x)+sin(20*x))^2
nr = 100 # número de repetições
ns = 100 # tamanho da amostra
y = rep(0,nr)

for(i in 1:nr){
  x = runif(ns,0,1)
  y[i] = mean(h(x))
}
mean(y)


################  O TCL
set.seed(1)

h = function(x) (cos(50*x)+sin(20*x))^2

nr = 1000 # número de repetições
ns = 2000 # tamanho da amostra
y = rep(0,nr)
for(i in 1:nr){
  x = runif(ns,0,1)
  y[i] = mean(h(x))
}
yb = mean(y)
s1 = sd(y) # estimativa desvio-padrão de h barra.

# Aqui estamos replicando o TCL

hist(x=((y-yb)/s1),freq=F)
curve(dnorm,-3,3,add=TRUE)


#########################################################
# Aqui temos apenas uma amostra de tamanho n            #
# estimamos a variância como se replicássemos nr vezes  #
# amostras de tamanho n                                 # 
#########################################################
set.seed(1)
n = ns
x = runif(n,0,1)
y = h(x)
yb = mean(y)
vest = sqrt(sum((y-yb)^2))/n  
yb
vest



### Exercicio 6


nr = 1000
n = 10
#y = rep(0,nr)  # usando loop
#for(i in 1:nr){
#  x = runif(n,0,1)
#  y[i] = max(x)   
#}
x = matrix(runif(n*nr),ncol=n,nrow=nr,byrow=T)
y = apply(x,1,max)

w = 1*(y<0.7)
ver = 0.7^n
plot.band(w,ver) 
sd.ver = sqrt(ver*(1-ver))/sqrt(nr)
