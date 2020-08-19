#Gerador de numeros aleatorios

### Testar o primeiro gerador:

m = 32   
a = 5
b = 3
w0 = 11
n = m
w = rep(0,n)
w[1] = (a*w0 + b) %% m

for(i in 1:(n-1)){
  w[(i+1)] = (a*w[i] + b) %% m
}


### A função

glc =function(w0,m,a,b,n){
  w = rep(0, n)
  w[1] = (a*w0 + b) %% m
  for (i in 1:(n-1)){
    w[(i+1)] = (a*w[i] + b) %% m
  }
  return(w)
  
}


w.ex1 =glc(w0=11,m=32,a=5,b=3,n=32)


# Considerando o segundo conjunto de valores:

w.ex3 =glc(w0=20,m=32,a=7,b=13,n=32)


## Sequencia uniforme
glc.u = function(w0,m,a,b,n){
  
  #completar
  
}


## Comparando resultados

w1 = glc.u(w0=13234,m=2^32,a=69069,b=23606797,n=5000)
w2 = glc.u(w0=13234,m=2^32,a=69068,b=23606797,n=5000)



## Olhar histograma


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
