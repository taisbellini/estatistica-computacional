## estimando erros do teste
#Sob h0

mu=0
sigma=1
n=10
RE=1000

ps=numeric(RE)
for(i in 1:RE){
  x=rnorm(n,mu,sigma)
  t=mean(x)/(sd(x)/sqrt(n))
  ps[i]=pt(t,df=n-1)
  
}


hist(ps) #se tem dist t, hist sera uniforme
mean(ps<0.025|ps>0.975)   #erro do tipo I


## poder

mus=(0:20)/10
sigma=1
n=10
RE=1000


power=numeric(length(mus))

for(j in 1:length(mus)){
mu=mus[j]
  ps=numeric(RE)
for(i in 1:RE){
  x=rnorm(n,mu,sigma)
  t=mean(x)/(sd(x)/sqrt(n))
  ps[i]=pt(t,df=n-1)
  
}


power[j]=mean(ps<0.025|ps>0.975)   #Poder
}

# quanto maior o mu, maior o poder (mais longe de mu, mas chance de rejeitar Ho quando Ho eh falsa)
plot(mu, power, type = "o", ylim = c(0,1))




