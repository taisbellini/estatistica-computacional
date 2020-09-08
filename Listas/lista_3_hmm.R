
## Exercicio 5 

library(ggplot2)
library(depmixS4)


sigma = c(1, 3, 5, 7)
p_i = rep(1/4, 4)
fda_pi = cumsum(p_i)

normal_gen = function(mu, sigma, N=2){
  u1 = runif(N/2) 
  u2 = runif(N/2)
  norm = sqrt(-2*log(u1))*cos(2*pi*u2)
  return(mu + sigma*norm)
}
normal_gen(0, 3)

n = 100
Y = matrix(nrow = n, ncol = 2)

for (i in 1:n){
  u = runif(1)
  pos = sum(u<= fda_pi)
  Y[i, 1] = normal_gen(0, sigma[pos])
  Y[i, 2] = pos
}

colnames(Y) = c('Obs', 'State')
Y = data.frame(Y)

ggplot(Y, aes(x = State, y = Obs)) + geom_point() 

# b - usando o cod de aula
mod <- depmix(Obs ~ 1, family = gaussian(), nstates = 4, data = Y)
set.seed(205650)
fm2 <- fit(mod, verbose = FALSE)
#
summary(fm2)
print(fm2)

# Classification (inference task)
probs <- round(posterior(fm2),3)             # Compute probability of being in each state
head(probs)

Y <- cbind(Y, probs)
head(Y)
