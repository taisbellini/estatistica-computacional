install.packages("tictoc")
library(tictoc)

#Quando apply é mais rápido

# Create the data frame

col1 <- runif (12^4, 0, 2)
col2 <- rnorm (12^4, 0, 2)
col3 <- rpois (12^4, 3)
col4 <- rchisq (12^4, 2)
df <- data.frame (col1, col2, col3, col4)


#For every row on this data frame (df), check if the sum of all values is greater than 4. If it is, a new 5th variable gets the value “greater_than_4”, else, it gets “lesser_than_4”.

# Original R code: Before vectorization and pre-allocation
tic("for")
for (i in 1:nrow(df)) { # for every row
  if ((df[i, "col1"] + df[i, "col2"] + df[i, "col3"] + df[i, "col4"]) > 4) { # check if > 4
    df[i, 5] <- "greater_than_4" # assign 5th column
  } else {
    df[i, 5] <- "lesser_than_4" # assign 5th column
  }
}
toc()


# after vectorization and pre-allocation
tic("vec and pre alloc")
output <- character(nrow(df)) # initialize output vector
for (i in 1:nrow(df)) {
  if ((df[i, "col1"] + df[i, "col2"] + df[i, "col3"] + df[i, "col4"]) > 4) {
    output[i] <- "greater_than_4"
  } else {
    output[i] <- "lesser_than_4"
  }
}
df$output<-output
toc()


# apply family
tic("apply")
myfunc <- function(x) {
  #if ((x['col1'] + x['col2'] + x['col3'] + x['col4']) > 4) {
  if ((x[1] + x[2] + x[3] + x[4]) > 4) {
    "greater_than_4"
  } else {
    "less_than_4"
  }
}
output <- apply(df[, c(1:4)], 1, FUN=myfunc)  # apply 'myfunc' on every row
df$output <- output
toc()

######################################################################################
######################################################################################
#########      Outra forma de medir tempo #### Pacote tictoc

library(tictoc)

sleep_for_10_sec <- function() { Sys.sleep(10) }       ### Criando uma funçao que pausa por 10 seg

tic("sleeping")           ## nome de interesse
print("falling asleep...")
sleep_for_10_sec()
print("...waking up")
toc()                       ### encerra o tempo do ultimo tic



tic("total")
tic("data generation")
X <- matrix(rnorm(50000*1000), 50000, 1000)
b <- sample(1:1000, 1000)
y <- runif(1) + X %*% b + rnorm(50000)
toc()
tic("model fitting")
model <- lm(y ~ X)
toc()
toc()


##################################################
##################################################

##Exercício

#Construa uma função que testa se a matriz A é positiva definida. 
#Verifique como o tempo computacional aumenta, qiando o tamanho da sua matriz aumenta.

##################################################
##################################################



### Exemplo de uso da funçao empirica ecdf(X)
#exemplo 1

x=rnorm(15)     
F=ecdf(x)     #obtem função de distribuição empirica
F
plot(F)
plot(F, verticals = TRUE,do.points=FALSE, col="gray")  #Observa o gráfico
curve(pnorm, add=TRUE, col="red", lty=2)   # Compara com a distribuição normal

x2=rnorm(300)
F2=ecdf(x2)
plot(F2, verticals = TRUE,do.points=FALSE, add=TRUE)


#F é uma função 
F(0)
F2(0)

################ Exemplo de um conjunto de dados reais 

data("USArrests")  #Um conjunto de dados do R

head(USArrests)   #olhar só as primeiras linhas
names(USArrests)


F=ecdf(USArrests$Murder)
plot(F)

hist(USArrests$Murder, freq = FALSE)
lines(density(USArrests$Murder))

#Exercício: Compare a distribuição empírica de x^2 e x2^2 com a distribuição qui-quadrado. O que você observa?

#Desafio: Escreva sua própria função para fazer o gráfico da função de distribuição empírica, sem usar ecdf.





