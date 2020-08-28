#Gerador de numeros aleatorios

#GLC: wi = (a*wi-1+b)%%m , i = 1,...,n
#precisa ter o w0 para gerar os proximos


# Exercicio 1: m=32, a=5, b=3, w0=11, n=m
m = 32   
a = 5
b = 3
w0 = 11
n = m

#vetor que vai guardar os resultados
w = rep(0,n)
#cria o primeiro
w[1] = (a*w0 + b) %% m

for(i in 1:(n-1)){
  w[(i+1)] = (a*w[i] + b) %% m
}

w


# Exercicio 2: criar uma função

glc =function(w0,m,a,b,n){
  w = rep(0, n)
  w[1] = (a*w0 + b) %% m
  for (i in 1:(n-1)){
    w[(i+1)] = (a*w[i] + b) %% m
  }
  return(w)
}


w.ex1 =glc(w0=11,m=32,a=5,b=3,n=32)


# Exercicio 3:
w.ex3 =glc(w0=20,m=32,a=7,b=13,n=32)

w.ex1
w.ex3 # este se repete a partir de 8. A parametrizacao a, b, m eh muito importante para um gerador bom


#Exercicio 4: Sequencia uniforme
glc.u = function(w0,m,a,b,n){
  w = rep(0, n)
  w[1] = (a*w0 + b) %% m
  for (i in 1:(n-1)){
    w[(i+1)] = (a*w[i] + b) %% m
  }
  w.u = w/m
  return(list("seq" = w, "unif" = w.u))
}

## Comparando resultados

w1 = glc.u(w0=13234,m=2^32,a=69069,b=23606797,n=5000)
w2 = glc.u(w0=13234,m=2^32,a=69068,b=23606797,n=5000)

## Olhar histograma

#para colocar os graficos lado a lado
par(mfrow=c(1,2))

hist(w1$unif)
hist(w2$unif)


## Diagrama de dispersao

par(mfrow=c(1,2))
plot(w1$unif)
plot(w2$unif)


## Descritivas

summary(w1$unif)
summary(w2$unif)

## ACF

par(mfrow=c(1,2))  
acf(w1$unif,lag.max=20)
acf(w2$unif,lag.max=20)

## Kolmogorov-Smirnov

ks.test(w1$unif,"punif",0,1)
ks.test(w2$unif,"punif",0,1)

w.runif = runif(1000)

ks.test(w.runif, "punif", 0.1)
