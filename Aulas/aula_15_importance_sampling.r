
## Exercicio 7

w = function(x) x^4*exp(-x)*(x>0)
h = function(x) w(x)/dexp(x,1)

n = 5000
x = rexp(n,1)
y = h(x)
yb = mean(y)
v.y1 = sum((y-yb)^2)/(n^2)
sqrt(v.y1)
plot.band(y,24)



## Exercicio 8

h1 = function(x) 1*(x>4.5)

n = 5000
x = rnorm(n,0,1)
y1 = h1(x)
ver1 = pnorm(4.5,lower.tail=F,log.p = F)
gr1 = plot.band(y=y1,ver=ver1)
yb1 = gr1$est
sd1 = gr1$dp
#log(yb1)
#pnorm(4.5,lower.tail=F,log.p = T)


## Resolução do exercício 8 - item 2

det45 = function(x,la=1) exp(-(x-4.5))*(x>4.5)

n = 5000
u = runif(n,0,1)
z = -log(u)+4.5

hist(z,freq=F)
lines(sort(z),det45(sort(z)))


## Resolução do exercício 8 - item 3

h2 = function(x) dnorm(x,0,1)/det45(x)

y2 = h2(z)
gr2 = plot.band(y2,ver1)

yb2 = gr2$est
sd2 = gr2$dp
#log(yb2)
#pnorm(4.5,lower.tail=F,log.p = T)
