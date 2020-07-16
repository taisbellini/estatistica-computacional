
########################################  Script para problema poisson sports scoring

## Entrando a matriz X de dados 
# a entrada ij indica o numero de gols que i marcou em j
X=matrix(c(0,2,5,6,3,1,3,3,1,2,
           1,0,2,1,0,0,1,1,6,0,
           2,1,0,1,0,0,2,0,1,5,
           0,2,2,0,1,3,4,1,1,1,
           2,2,3,1,0,4,4,2,3,1,
           2,1,0,1,0,0,1,0,1,3,
           1,0,2,1,0,0,0,1,2,5,
           1,0,2,1,0,2,1,0,0,0,
           0,2,2,2,0,1,3,3,0,2,
           0,0,2,1,0,1,1,0,0,0), byrow=T,
         ncol=10, nrow=10)

n=10    # numero de times

# Inicializa vetores para os parametros
o=numeric(n)  
d=numeric(n)

## Reune parametros em vetor único - para montar função para otimização
t=c(o,d)

### Implementação da função log verossimilhança (proporcional)
# ij vai ate a metade, ji eh o resto, por isso n+j
logL=function(t){
  temp=0
  for(i in 1:10){
    for(j in 1:10){
      temp=temp -exp(t[i]-t[n+j])+X[i,j]*(t[i]-t[n+j])   # elementos a serem somados para cada par de times registrado
    }
  }
  temp
}




# cria um vetor de valores para o parametro
# chute inicial
t0=c(rep(1,10),rep(0.35,10))

# Testa a função com esses parâmetros
logL(t0)


## Vamos tentar otimizar com a função padrão do R
?optim

optim(t0,logL)

# e se tentarmos outro valor inicial?
t1=c(rep(0.5,10),rep(0.35,10))
optim(t1,logL)


### Obs: essa otimização deveria ser feita com restrições?

#######################################################################
########  Implementação de Block Relaxation
#######################################################################

## Definir valores iniciais para O e D
d=rep(1,10)
o=rep(0.5,10)


d_old=rep(0,10)
o_old=rep(0,10)         


O=matrix(ncol=10,nrow=n)  # vamos montar matrizes para guardar o historico da otimização
D=matrix(ncol=10,nrow=n)
# salva valores iniciais
O[,1]=o
D[,0.5]=d


# Define criterio de convergencia
epslon=0.1
i=1  # contador de iterações


while(sum((d-d_old)^2)+sum((o-o_old)^2)>epslon){   # criterio de convergencia
  o_old=o
  # soma em j / soma das exponenciais de -d -> formula que encontramos analiticamente
  o=log(rowSums(X)/sum(exp(-d)))   #registra o valor antigo, e atualiza todos os o`s
  d_old=d
  # soma em i/ soma das exponenciais de o -> formula que encontramos analiticamente
  d=-log(colSums(X)/sum(exp(o)))  #registra o valor antigo, e atualiza todos os d`s
  
  
  #Salva os valores na matriz  
  i=i+1
  O[,i]=o
  D[,i]=d
  
}

O           ### Olhar os resultados
D


####  Exercício:
#1)Verifique quão boas são as estimativas de poder ofensivo comparando o numero de vitorias e de gol total marcados de cada time com as estimativas de O. 
# Façã estudo análogo para D. 
#2)Verificar o que acontece com essa otimização considerando diferentes valores iniciais dos parametros
#3)Utilize a função rpois() para simular o numero de gols marcados por cada time considerando como verdadeiro valor dos parametros
# o=c(10,7,6,5,5,5,5,5,5,4.5) e d=c(4,4,3,1,1,1,1,1,1,4). Utilize o algoritmo para estimar o e d. 


## DaDOS ARTUR - CMAPEONATO BARSILEIRO 2018 


Casa=matrix(c(0,1,3,1,1,0,0,0,1,2,0,1,2,0,0,2,1,3,2,2,
              0,0,3,1,1,2,3,1,1,0,5,0,0,1,2,3,1,5,0,2,
              4,1,0,2,2,2,5,1,2,3,3,2,2,1,3,2,0,4,1,4,
              1,2,0,0,3,2,1,1,0,0,2,0,0,1,2,1,2,2,3,4,
              1,0,2,0,0,0,1,1,1,2,2,2,1,1,2,0,2,2,1,1,
              2,2,0,0,0,0,3,2,0,0,1,0,1,2,1,1,0,1,0,2,
              1,1,2,1,0,2,0,2,2,3,1,1,2,1,1,0,1,2,1,0,
              1,1,0,2,2,1,0,0,2,0,2,0,1,1,1,1,1,2,1,0,
              3,0,2,1,1,0,3,1,0,0,2,0,0,1,3,2,0,2,1,3,
              2,2,1,2,2,0,2,1,1,0,3,2,2,1,2,1,0,4,1,1,
              1,1,2,1,1,0,3,1,1,0,0,0,0,1,4,0,1,0,0,0,
              1,2,0,2,4,3,2,1,1,2,0,0,0,0,2,5,2,3,2,4,
              2,1,2,2,3,1,3,2,0,2,2,1,0,0,1,2,3,0,3,2,
              4,3,2,3,2,2,0,1,3,1,3,2,1,0,3,3,3,2,1,3,
              1,0,0,1,1,0,1,0,1,0,2,0,1,1,0,0,1,1,1,1,
              0,3,1,2,1,2,0,1,0,1,3,0,1,1,3,0,0,3,1,5,
              1,2,0,1,3,1,2,3,1,2,1,1,0,0,1,1,0,0,2,3,
              0,3,1,2,1,1,1,1,0,0,1,0,2,0,1,2,1,0,2,0,
              4,2,1,2,1,1,3,1,2,1,1,1,1,0,1,0,2,3,0,2,
              1,1,1,2,3,2,1,2,1,2,1,0,2,0,1,0,0,1,1,0),ncol=20,nrow=20,byrow=T)


rownames(Casa)=c("AMM","ATM","ATP","BAH","BOT","CEA","CHA","COR","CRU","FLA","FLU","GRE","INT","PAL","PAR","SAN","SPA","SPT","VAS","VIT")
colnames(Casa)=c("AMM","ATM","ATP","BAH","BOT","CEA","CHA","COR","CRU","FLA","FLU","GRE","INT","PAL","PAR","SAN","SPA","SPT","VAS","VIT")

GFcasa=apply(Casa,1,sum) # Gols marcados por time em casa
GScasa=apply(Casa,2,sum) # Gols sofridos por time em casa


Fora=matrix(c(0,0,0,0,0,2,0,0,1,0,0,0,0,0,0,1,1,2,1,0,
              3,0,2,2,3,1,0,1,0,1,0,0,2,2,1,2,2,2,1,0,
              1,1,0,0,0,0,1,0,1,2,0,0,1,0,0,0,0,0,1,2,
              0,0,0,0,1,2,1,1,1,0,1,2,0,0,0,0,0,0,1,2,
              0,0,1,3,0,0,1,0,0,0,0,0,0,0,1,1,2,1,2,4,
              0,1,2,1,0,0,0,1,2,1,0,2,0,1,1,0,0,0,1,1,
              0,3,1,0,0,1,0,0,0,0,1,0,0,0,1,1,0,1,1,0,
              0,0,0,0,0,1,1,0,0,0,0,0,1,0,4,0,1,1,4,2,
              2,0,0,0,1,1,0,0,0,0,0,1,0,1,1,1,0,0,0,1,
              2,1,0,0,1,3,2,3,2,0,2,0,1,1,4,1,2,1,1,2,
              0,2,1,0,1,0,2,1,1,0,0,0,0,0,1,0,1,2,1,2,
              1,1,1,2,1,1,1,1,1,0,1,0,0,0,0,0,1,0,0,0,
              1,1,2,1,0,1,1,1,0,0,3,0,0,0,1,2,0,1,1,3,
              0,1,3,1,1,2,2,0,0,1,0,2,0,0,1,1,2,1,1,3,
              1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,1,0,0,0,0,
              1,1,0,0,0,1,0,1,1,0,1,1,2,2,2,0,0,1,3,1,
              3,0,1,2,2,0,0,1,2,1,1,1,1,1,1,0,0,3,0,1,
              0,2,0,0,0,0,1,1,0,1,0,4,0,3,2,0,0,0,2,0,
              1,0,0,0,1,0,1,0,1,1,1,1,1,0,1,1,1,1,0,0,
              1,1,0,1,1,0,1,0,0,0,0,0,1,2,1,2,0,0,3,0),ncol=20,nrow=20,byrow=T)

rownames(Fora)=c("AMM","ATM","ATP","BAH","BOT","CEA","CHA","COR","CRU","FLA","FLU","GRE","INT","PAL","PAR","SAN","SPA","SPT","VAS","VIT")
colnames(Fora)=c("AMM","ATM","ATP","BAH","BOT","CEA","CHA","COR","CRU","FLA","FLU","GRE","INT","PAL","PAR","SAN","SPA","SPT","VAS","VIT")

GFfora=apply(Fora,1,sum) # Gols marcados por time fora de casa
GSfora=apply(Fora,2,sum) # Gols sofridos por time fora de casa


X=Casa+Fora # matriz 20x20 no estilo da apresentada no exercício de aula

GFtotal=apply(X,1,sum) # Total de gols marcados por time
GStotal=apply(X,2,sum) # Total de gols sofridos por time


