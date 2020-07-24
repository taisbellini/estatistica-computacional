## Resolução do exercício 1


cc = 1
conta = 0
a = 0.025

x0 = 0  # valor inicial

while(cc>0.0001){
  x1 = x0 - (pnorm(x0)-a)/dnorm(x0)
  cc = (x1-x0)^2
  x0 = x1
  conta=conta+1
}

qnorm(a)  # valor função pronta do R.



###########################################################
###########################################################

## Resolução do exercício 2


f = function(x) -x^2+2*x-5
#primeira derivada
f1 = function(x) -2*x + 2
#segunda derivada
f2 = function(x) -2

curve(f,-2,4)

cc = 1
conta = 0
x0 = 2

while(cc>0.0001){
  x1 = x0 - f1(x0)/f2(x0)
  cc = (x1-x0)^2
  x0 = x1
  conta=conta+1
  print(paste("interacao", conta, "x1=", x1))
}


###########################################################
###########################################################


## Código exemplo

f = function(x) 5*sin(5*x) + cos(x)
fl = function(x) 25*cos(5*x) - sin(x)
fl2 = function(x) -25*5*sin(5*x) - cos(x)
#curve(f,-1,1)

cc = 1
conta = 0
#x0 = -0.7 # desce para minimo local
x0 = -0.8 # sobe para maximo local

while(cc>0.0001){
  x1 = x0 - fl(x0)/fl2(x0)
  cc = (x1-x0)^2
  x0 = x1
  conta=conta+1
}
x0



###########################################################
###########################################################

## Resolução exercício 3 - olhando a função

f = function(x,y) (x^2+5*y^2)
x = y = seq(from=-3,to=3,length=50)
A = matrix(0,nrow=50,ncol=50)
for(i in 1:50){
  A[i,] = f(x[i],y)
}
persp(x,y,A,theta=120,phi=-5)
contour(x,y,A,nlevels = 50)



f = function(x,y) x^2+5*y^2
f1 = function(z){
  x = z[1]
  y = z[2]
  rbind(2*x,10*y)
}

f2 = function(z){
  x = z[1]
  y = z[2]
  A = matrix(0,ncol=2,nrow=2)
  A[1,1] = 2
  A[1,2] = A[2,1] = 0
  A[2,2] = 10
  return(A)
}




cc = 1
conta = 0
z = x0 = rbind(0,0)

while(cc>0.0001){
  x1 = x0 - solve(f2(x0))%*%(f1(x0))
  cc = sum((x1-x0)^2)
  x0 = x1
  conta=conta+1
  z = cbind(z,x1)
}
rownames(z) = paste("x",1:nrow(x0),sep="")
colnames(z) = paste("i",1:(conta+1),sep="")
contour(x,y,A,nlevels = 50)
lines(z[1,],z[2,],col=2)


###########################################################
###########################################################

## Resolução exercício 4 - olhando a função

f = function(x,y) (x^4+y^2)
x = y = seq(from=-3,to=3,length=50)
A = matrix(0,nrow=50,ncol=50)
for(i in 1:50){
  A[i,] = f(x[i],y)
}
persp(x,y,A,theta=120,phi=-5)
contour(x,y,A,nlevels = 50)



f = function(x,y) x^4+y^2
f1 = function(z){
  x = z[1]
  y = z[2]
  rbind(4*x^3,2*y)
}

f2 = function(z){
  x = z[1]
  y = z[2]
  A = matrix(0,ncol=2,nrow=2)
  A[1,1] = 12*x^2
  A[1,2] = A[2,1] = 0
  A[2,2] = 2
  return(A)
}

cc = 1
conta = 0
z = x0 = rbind(10,10)
x1 = x0 - solve(f2(x0))%*%(f1(x0))

while(cc>0.0001){
  x1 = x0 - solve(f2(x0))%*%(f1(x0))
  cc = sum((x1-x0)^2)
  x0 = x1
  conta=conta+1
  z = cbind(z,x1)
}
rownames(z) = paste("x",1:nrow(x0),sep="")
colnames(z) = paste("i",1:(conta+1),sep="")
contour(x,y,A,nlevels = 50)
lines(z[1,],z[2,],col=2)


###########################################################
###########################################################

## Resolução exercício 5

f = ~x^4+y^2
f1 = deriv(f, c("x","y"), func=T)
f1(2,2)
attr(f1(2,2), 'gradient')

f = function(x,y) x^4+y^2
f1 = function(z){
  x = z[1]
  y = z[2]
  rbind(4*x^3,2*y)
}

f2 = function(z){
  x = z[1]
  y = z[2]
  A = matrix(0,ncol=2,nrow=2)
  A[1,1] = 12*x^2
  A[1,2] = A[2,1] = 0
  A[2,2] = 2
  return(A)
}


newton_raphson = function(f, f1, f2, epslon, x0=rbind(2,2)) {
  cc = 1
  conta = 0
  x0 = z = x0
  f1 = 
  
  while(cc>0.0001){
    x1 = x0 - solve(f2(x0))%*%(f1(x0))
    cc = sum((x1-x0)^2)
    x0 = x1
    conta=conta+1
    z = cbind(z,x1)
  }
  return(z)
  
}

#test
newton_raphson(f, f1, f2, 0.000001)
