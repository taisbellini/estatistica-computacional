library(tictoc)

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

tic("apply")
apply(mapply(rnorm, n = 1000, mean = 1:200), 2, var)
toc()