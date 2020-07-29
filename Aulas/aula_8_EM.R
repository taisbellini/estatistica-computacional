## Implementação EM

#simula dados:
na=rbinom(1,150,0.5)
y1=rbinom(na,10,0.7)
y2=rbinom(150-na,10,0.2)
y=c(y1,y2)
hist(y)


### Inicia EM
t=matrix(ncol=1000,nrow=2)
t0=c(0.3,0.7)
t[,1]=t0
cc=1
rep=1
t1=vector()

while(cc>0.0001 & rep<1000){
  #etapa E
  
  #essa E vale apenas para na com 0.5 no param (50% de chance de pegar qualquer uma das moedas)
  pi=t0[1]^y*(1-t0[1])^(10-y)/(t0[1]^y*(1-t0[1])^(10-y)+t0[2]^y*(1-t0[2])^(10-y))
  
  #etapa M
  
  t1[1]=sum(pi*y)/(10*sum(pi))
  t1[2]=sum((1-pi)*y)/(10*sum((1-pi)))
  
  #
  rep=rep+1
  cc=sum((t0-t1)^2)
  t0=t1
  t[,rep]=t0
}

t=t[,1:rep]



## Exercício: construa o grafico da funçao e coloque sobre ele os pontos visitados pelo algoritmo

### ABO





na=186
nb=38
nab=13
no=284

f=function(pa,pb,po){2*no*log(po)+na*log(2*pa*po+pa^2)+
    nb*log(2*pb*po+pb^2)+nab*log(2*pa*pb)}



### Inicia EM
p=matrix(ncol=1000,nrow=3)
p0=c(0.1,0.1,0.3)
p[,1]=p0
cc=1
rep=1
p1=vector()
n=sum(na+no+nab+nb)

while(cc>0.0001 & rep<1000){
  #etapa E
  naa=na*p0[1]^2/(p0[1]^2+2*p0[1]*p0[3])
  nao=na*2*p0[1]*p0[3]/(p0[1]^2+2*p0[1]*p0[3])
  nbb=nb*p0[2]^2/(p0[2]^2+2*p0[2]*p0[3])
  nbo=nb*2*p0[2]*p0[3]/(p0[2]^2+2*p0[2]*p0[3])
  
  #etapa M
  
  p1[1]=(2*naa+nao+nab)/(2*n)
  p1[2]=(2*nbb+nbo+nab)/(2*n)
  p1[3]=(2*no+nao+nbo)/(2*n)
  #
  rep=rep+1
  cc=sum((p0-p1)^2)
  p0=p1
  p[,rep]=p0
}

p=p[,1:rep]

p
