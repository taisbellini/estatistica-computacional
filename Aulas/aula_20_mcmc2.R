install.packages("mcmcse")
install.packages("coda")
library(coda)
library(mcmcse)


funcao_p=function(x){      ## função proporcional a densidade que queremos amostrar (aqui X é o vetor de dados)
   (x[1]+x[2]+x[3])^(5)
}

proposta=function(xi){    # aqui xi eh o elemento que queremos amostrar 

   x_old=100*xi[1]+10*xi[2]+xi[3]
   x_new=x_old+sample(c(-2,-1,1,2),1)

   if(x_new==-1) x_new=999     
   if(x_new==-2) x_new=998     
   if(x_new==1000) x_new=0     
   if(x_new==1001) x_new=1     
   
   res=vector()
   res[1]=floor(x_new/100)
   res[2]=floor((x_new-100*res[1])/10)
   res[3]=x_new-100*res[1]-10*res[2]
   res
   }

#   p_proposta=1/4   

prob_aceita=function(x_old,x_new){
   min(funcao_p(x_new)/funcao_p(x_old), 1)
}

###################  Função MCMC

runmcmc=function(start,n_MCMC=50000){


#n_MCMC=50000  #número de iterações do MCMC


MCMC_d=matrix(ncol=n_MCMC, nrow=3)     #declara a matriz que conterá os valores dos dígitos ao longo do MCMC
#cada coluna eh uma ieração do MCMC

p=vector()                 #feclara vetor que vai guardar resultado da função densidade para cada observação

MCMC_d[,1]=start       #escolhe valores iniciais





for(it in (1:(n_MCMC-1))){    ### inicia o algoritmo
      x_old=MCMC_d[,it]
      x_new=x_old
      x_new=proposta(x_old)                ## cria uma proposta
      
      if(runif(1)<prob_aceita(x_old,x_new)){
         MCMC_d[,it+1]=x_new                         #com probabilidade prob_aceita registra o valor da proposta no MCMV 
      }else{
         MCMC_d[,it+1]=x_old                         #caso contrario repete o valor antigo 
      }
     
   }
   return(MCMC_d)
}

### Utilizando o pacote de diagnostico coda

MCMC_d=runmcmc(start=c(0,0,0),100000)

#monta o numero 
MCMC=rbind(MCMC_d,MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,],(MCMC_d[1,]+MCMC_d[2,]+MCMC_d[3,])^5)
row.names(MCMC)=c("centena","dezena","unidade","valor x","f(x)")

x=mcmc(t(MCMC))   #cria elemento coda.mcmc
summary(x)
plot(x)

### Pontos iniciais diferentes
MCMC_d=runmcmc(start=c(0,0,0),100000)
x1=MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,]
MCMC_d=runmcmc(start=c(4,4,4),100000)
x2=MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,]  #rodar novamente com outro ponto inicial
MCMC_d=runmcmc(start=c(8,8,8),100000)
x3=MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,]  #rodar novamente com outro ponto inicial


ts.plot(x1,ylim=c(0,1000))
lines(x2,col="red")
lines(x3,col="green")

### 

acceptanceRate <- 1 - rejectionRate(x)
acceptanceRate       #ideal próximo de 0.234 p rand walk proposal

effectiveSize(x)      #tamanho amostral efetivo
autocorr.plot(x)


## Compara com gráfico da versão inicial (qual convergiu?)

# Utilizar função mcse.q.mat(x, alfa) para construir intervalos de confiança para a esperança de X.
# qual a incerteza dessas estimativs?



