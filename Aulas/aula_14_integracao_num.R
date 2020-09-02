########################################################
###########  Integração numérica com R
########################################################

#Considere a função:
f=function(x){
  3*x^2*(3+x)
}



curve(f, from=-3, to=1, , xlab="x", ylab="y")  #plotar a curva entre -3 e 1

#########################################
## Qual a área sob a curva entre -3 e 1
#########################################


a=-3 #valor minimo
b=1  #valor máximo
rep= 1000 #numero de repartições

abs=seq(from=a, to=b,length.out=rep)

areas= numeric(rep-1)

for(i in 1:(rep-1)){
  areas[i]=(abs[i+1]-abs[i])*f((abs[i+1]+abs[i])/2)
}

sum(areas)




### Função do R que calcula 
integrate(f, lower = -3, upper = 1)



#### Montar Função que calcula a integral


integral=function(funcao,inicio,fim){
  abs=seq(from=inicio, to=fim,length.out=rep)
  
  areas= numeric(rep-1)
  
  for(i in 1:(rep-1)){
    areas[i]=(abs[i+1]-abs[i])*funcao((abs[i+1]+abs[i])/2)
  }
  
  return(sum(areas))
}



integral(f,-3,1)






#outra função
f=function(x){
  1/(x-3)^2*abs(sin(x))
}




####################outra função - Algumas funções são mais faceis de integrar com precisão. 
f=function(x){
  1/x
}


curve(f, from=0, to=1, , xlab="x", ylab="y")  #plotar a curva entre -3 e 1


integral(f,0.0001,1)
integrate(f, lower = 0.0001, upper = 1)


##########################################################
### Exercício: escrever uma finção do R que calcula integral numérica de uma função de acordo com a regra trapezoidal em que podemo alterar o numero de raparticoes do intervalo. O que acontece quando aumentamos rep?
### Ulilize-a para numericamente calcular a esperança da distribuição normal padrão. 
##########################################################



############ Integração numérica mais D

install.packages("cubature")
library("cubature")
?hcubature


testFn0 <- function(x) {
  prod(cos(x))
}

hcubature(testFn0, lowerLimit=rep(0,2), upperLimit=rep(1,2), tol=1e-4)























