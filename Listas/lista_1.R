install.packages('matrixcalc')
install.packages('Matrix')
install.packages('clusterGeneration')
install.packages('tictoc')
library(matrixcalc)
library(clusterGeneration)
library(tictoc)
library(Matrix)

### Ex 3 ###
#Ref: http://prorum.com/?qa=2064/como-saber-uma-matriz-positiva-definida-negativa-definida
#https://davetang.org/muse/2014/01/22/making-symmetric-matrices-in-r/


# Support functions

#make a symetric normal matrix
make_normal_symetric = function(dim) {
  #make a dim by dim matrix
  mat <- matrix(rnorm(dim*dim), nrow=dim)
  mat <- as.matrix(forceSymmetric(mat))
  return(mat)
}

# test function
symetric_matrix = make_normal_symetric(10)
isSymmetric(symetric_matrix)

is_positive.det = function(X){
  if (ncol(X) != nrow(X)) return(FALSE)
  for(i in 2:nrow(X)){
    m = X[1:i, 1:i]
    if(det(m) < 0 ) return(FALSE)
  }
  return(TRUE)
}

is_positive.eigen = function(X){
  if (ncol(X) != nrow(X)) return(FALSE)
  if (sum(eigen(X)$values < 0) == 0) return(TRUE)
  return(FALSE)
}

#Test functions created (compare to pre-built)

X_sym <- make_normal_symetric(10)
#lib to generate a positive definite matrix
X_pos <- genPositiveDefMat(10)$Sigma
isSymmetric(X_pos)
I <- diag(10)

is_positive.det(X_sym)
is_positive.det(X_pos)
is_positive.det(I)

is_positive.eigen(X_sym)
is_positive.eigen(X_pos)
is_positive.eigen(I)

is.positive.definite(X_sym)
is.positive.definite(round(X_pos, 4))
is.positive.definite(I)


# Make tests from exercise: 

times = matrix(0, nrow = 4, ncol=3)

#a) 1000 matrixes 10x10 

# generate 1000 matrix
X = list(1000)
for (i in 1: 1000) {
  X[[i]] <- make_normal_symetric(10)
}

a = Sys.time()
is_pos = lapply(X, is_positive.det)
times[1,1] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

a = Sys.time()
is_pos = lapply(X, is_positive.eigen)
times[1,2] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

a = Sys.time()
is_pos = lapply(X, is.positive.definite)
times[1,3] = Sys.time()-a
print(sum(is_pos > 0)/1000) 


#b) 1000 matrixes 100x100 

# generate 1000 matrix
X = list(1000)
for (i in 1: 1000) {
  X[[i]] <- make_normal_symetric(100)
}

a = Sys.time()
is_pos = lapply(X, is_positive.det)
times[2,1] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

a = Sys.time()
is_pos = lapply(X, is_positive.eigen)
times[2,2] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

a = Sys.time()
is_pos = lapply(X, is.positive.definite)
times[2,3] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

#c) 1000 matrixes 1000x1000

# generate 1000 matrix
X = list(1000)
for (i in 1: 1000) {
  X[[i]] <- make_normal_symetric(1000)
}

a = Sys.time()
is_pos = lapply(X, is_positive.det)
times[3,1] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

a = Sys.time()
is_pos = lapply(X, is_positive.eigen)
times[3,2] = Sys.time()-a
print(sum(is_pos > 0)/1000) 

a = Sys.time()
is_pos = lapply(X, is.positive.definite)
times[3,3] = Sys.time()-a
print(sum(is_pos > 0)/1000) 


#d) 1000 matrixes 10000x10000 

# generate 1000 matrix and test to not use memory

is_pos_count = 0
a = Sys.time()
for (i in 1: 20) {
  X <- make_normal_symetric(10000)
  if (is_positive.det(X)) { is_pos_count = is_pos_count+1 }
}
# time for 20 comparisons
t = Sys.time()-a
t_mean = t/20
times[4,1] = t_mean*1000
print(is_pos_count/1000) 

is_pos_count = 0
a = Sys.time()
for (i in 1: 20) {
  X <- make_normal_symetric(10000)
  if (is_positive.eigen(X)) { is_pos_count = is_pos_count+1 }
}
t = Sys.time()-a
t_mean = t/20
times[4,2] = t_mean*1000
print(is_pos_count/1000) 

is_pos_count = 0
a = Sys.time()
for (i in 1: 20) {
  X <- make_normal_symetric(10000)
  if (is.positive.definite(X)) { is_pos_count = is_pos_count+1 }
}
t = Sys.time()-a
t_mean = t/20
times[4,3] = t_mean*1000
print(is_pos_count/1000) 


#plot time evolution
colnames(times) <- c("det", "eigen", "pre-build-matrixcalc")
matplot(times, type='l')
legend("left", colnames(times),col=seq_len(ncol(times)),cex=0.8,fill=seq_len(ncol(times)))

#plot time evolution without 10000x10000
colnames(times) <- c("det", "eigen", "pre-build-matrixcalc")
matplot(times[1:3,], type='l')
legend("left", colnames(times),col=seq_len(3),cex=0.8,fill=seq_len(3))


### Ex 5 ###
install.packages("rgl")
library(rgl)

#Visualize function
x = y = seq(from=-2, to=2, by=0.5)

f = function(x, y) {
  x^4+y^2+4*x*y
}
f(sqrt(8)/2, -sqrt(8))
f(-sqrt(8)/2, sqrt(8))

z = outer(x, y, f)

persp(x, y, z, ticktype = 'detailed')


#Newton-Raphson

#queremos minimizar o lagrangeano
f = function(x,y, lambda) x^4+y^2+4*x*y+lambda*(x^2+y^2-1)

grad = function(x, y, lambda){
  rbind(4*x^3+4*y+2*lambda*x,4*x+2*y+2*lambda*y, 2*x+2*y)
}

hess = function(x, y, lambda){
  A = matrix(0, ncol=3, nrow=3)
  A[1,1] = 12*x^2+2*lambda
  A[1,2] = 4
  A[1,3] = 2
  A[2,1] = 4
  A[2,2] = 2+2*lambda
  A[2,3] = 2
  A[3,1] = 2
  A[3,2] = 2
  A[3,3] = 0
  return(A)
} 

cc = 1
conta = 0
x0 = rbind(1,1,1)
z=x0

while(cc>0.0001){
  x1 = x0 - solve(hess(x0[1],x0[2],x0[3]))%*%(grad(x0[1], x0[2], x0[3]))
  cc = sum((x1-x0)^2)
  x0 = x1
  conta=conta+1
  z = cbind(z,x1)
  
}

z
x0
s