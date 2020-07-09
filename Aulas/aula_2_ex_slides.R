library(tictoc)

#ex 1

#a
tic("total")
tic("create matrix")
m = matrix(nrow=1000, ncol=200)
for (i in 1:200){ 
    m[, i] = rnorm(1000, i, 1)
}
toc()
tic("calculate var")
vars = numeric(1000)
for(i in 1:1000){
  vars[i] = var(m[i,])
}
toc()
toc()

#b 
tic("apply")
apply(mapply(rnorm, n=1000, mean=1:200), 1, var)
toc()

#ex 2
tic("total for")
tic("create matrix")
ms = list(10)
for (i in 1:10){
  m = matrix(nrow=20, ncol=100)
  means = list(20)
  for (j in 1:20){
    m[j, ] = rnorm(100, j, i)
    means[[j]] = mean(m[j,])
  }
  ms[[i]] = means
}
toc()
toc()

tic("total apply")
msa = list(10)
for(i in 1:10){
  msa[[i]] = apply(mapply(rnorm, n=100, mean=1:20), 2, mean)
}
toc()

