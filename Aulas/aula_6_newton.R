##Optim_aplicacoes

## Função de otimização - da ultima aula
nr.optim = function(x0,f1,f2,epslon=0.0001,...){
  cc = 1
  conta = 0
  x0 = matrix(x0,ncol=1)
  z = x0
  
  while(cc>epslon && conta<1000){
    x1 = x0 - solve(f2(x0,...))%*%(f1(x0,...))
    cc = sum((x1-x0)^2)
    x0 = x1
    conta=conta+1
    z = cbind(z,x1)
  }
  rownames(z) = paste("x",1:nrow(x0),sep="")
  colnames(z) = paste("i",1:(conta+1),sep="")
  list("arg.ot"=x0,"seq.ot"=z)  
}

################################################
## Exercicio 1 - Otimização com restrições
################################################
f = function(x) x^3*exp(-x)*(x>0)/6

f1 = function(x){
  x = abs(x)+0.01     ## Transforma o x em algo positivo
  (3/x - 1)*(x>0)
}

f2 = function(x){ 
  x = abs(x)+0.01
  (-3/x^2)*(x>0)
}


ot5 = nr.optim(x0=6,f1,f2)
resp = abs(ot5$arg) + 0.01  # importante fazer isso.

################################################
## Exercicio 2 - Otimização com restrições
################################################


f = function(x){ 
  x = exp(x)/(1+exp(x))
  3*x^2*(1-x)*(x>0)*(x<1)
}

f1 = function(x){ 
  x = exp(x)/(1+exp(x))
  6*x-9*x^2*(x>0)*(x<1)
}

f2 = function(x){ 
  x = exp(x)/(1+exp(x))
  (6-18*x)*(x>0)*(x<1)
}
ot7 = nr.optim(x0=0.1,f1,f2)
resp = exp(ot7$arg.ot)/(1+exp(ot7$arg.ot))

################################################
## Método Secante
################################################


sec.optim = function(x0,x1,f,...){
  cc = 1
  conta = 0
  x0 = matrix(x0,ncol=1)
  x1 = matrix(x1,ncol=1)
  z = cbind(x0,x1)
  while(cc>0.0001){
    x2 = x1 - (x1-x0)*f(x1,...)/(f(x1,...)-f(x0,...))
    cc = sum((x2-x1)^2)
    x0 = x1
    x1 = x2
    conta=conta+1
    z = cbind(z,x2)
  }
  rownames(z) = paste("x",1:nrow(x0),sep="")
  colnames(z) = paste("i",1:(conta+2),sep="")
  list("arg.ot"=x2,"seq.ot"=z)  
}

f = function(x) exp(x)*x^2-6
curve(f, -10, 1.5)
res = sec.optim(2,3,f)
res
f(res$arg.ot)
