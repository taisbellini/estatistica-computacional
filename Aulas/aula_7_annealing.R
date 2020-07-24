
#################################################
############## Simulated Annealing
#################################################



#################################################
############## Exercício 1
#################################################


f = function(x){(cos(50*x)+sin(20*x))^2}

n.iter = 100
x = accept = rep(0,n.iter)
# chute inicial
x[1] = 0.25

for(i in 2:n.iter){
  temp = 1/log(1+i)
  d = sqrt(temp)
  x1 = runif(1,max(0,x[(i-1)]-d),min(x[(i-1)]+d,0.5))
  u = runif(1)
  prob = min(exp((f(x1)-f(x[(i-1)]))/temp),1)
  x[i] = (u <= prob)*x1 + (u > prob)*x[(i-1)]    
  accept[i] = (u <= prob)
}


### Avaliando a resposta

mean(accept[-1])
pos = which.max(f(x))
x[pos]

par(mfcol=c(2,1))
plot(f(x),type="l")
curve(f,0,0.5)
lines(x,f(x),col=2)
points(x[pos],f(x[pos]),col=4)


#################################################
############## Exercício 2
#################################################

## Simulando dados do modelo

n = 1000
m1 = 0; m2 = 2.5
p1 = 0.25; p2 = 1-p1
x1 = rnorm(p1*n,m1,1)
x2 = rnorm(p2*n,m2,1)

x = c(x1,x2)
hist(x)

#### Implemeta vero

f = function(m,x){
  m1 = m[1]
  m2 = m[2]
  sum(log(p1*dnorm(x-m1) + p2*dnorm(x-m2)))
}


#seta aneealing
n.iter = 500
teta = matrix(0,nrow=n.iter,ncol=2)
teta[1,] = c(4,0)#c(mean(x),mean(x))
f.eval = rep(0,n.iter)
f.eval[1] = f(teta[1,],x)

for(i in 2:n.iter){
  temp = 1/log(1+i)
  d = 10*sqrt(temp)
  e = runif(2,-d,d) #rnorm(2)*d
  te1 = teta[(i-1),]+e
  u = runif(1)
  prob = min(exp((f(te1,x)-f(teta[(i-1),],x))/temp),1)
  teta[i,] = (u <= prob)*te1 + (u > prob)*teta[(i-1),]    
  f.eval[i] = f(teta[i,],x)
}

#encontra o máximo
pos = which.max(f.eval)   
teta.ot = teta[pos,]    
plot.ts(teta[,1]);plot.ts(teta[,2])


## Monta plot para vizualizar otimização
f = function(m1,m2,x){
  sum(log(p1*dnorm(x-m1) + p2*dnorm(x-m2)))
}
l1 = 100
m1e = m2e = seq(from=-2,to=5,length=l1)

A = matrix(0,nrow=l1,ncol=l1)
for(i in 1:l1){
  for(j in 1:l1){
    A[i,j] = f(m1e[i],m2e[j],x) # cuidado aqui
  }
}

contour(m1e,m2e,A,nlevels = 50,xlab="m1",ylab="m2")
lines(teta[,1],teta[,2],col=2)
points(teta[pos,1],teta[pos,2],col=4)