
#----------------------------------------------------------------
# Hiden Markov Model of S&P 500 log returns
# See documentation for depmixS4 package 
# http://cran.r-project.org/web/packages/depmixS4/depmixS4.pdf and presentation 
# on Singapore R Users Group Site on HMM February 14, 2014
# http://www.meetup.com/R-User-Group-SG/files/

library(depmixS4)
library(TTR)
library(ggplot2)
library(reshape2)
library(xts)

## Bull and Bear Markets ##
# Load S&P 500 returns from Yahoo
Sys.setenv(tz = "UTC")
sp500 <- getYahooData("^GSPC", start = 19500101, end = 20120909, freq = "daily")
head(sp500)
tail(sp500)

# Preprocessing
# Compute log Returns
ep <- endpoints(sp500, on = "months", k = 1)
sp500LR <- sp500[ep[2:(length(ep)-1)]]
sp500LR$logret <- log(sp500LR$Close) - lag(log(sp500LR$Close))
sp500LR <- na.exclude(sp500LR)
head(sp500LR)

# Build a data frame for ggplot
sp500LRdf <- data.frame(sp500LR)
sp500LRdf$Date <-as.Date(row.names(sp500LRdf),"%Y-%m-%d")

# Plot the S&P 500 returns
ggplot( sp500LRdf, aes(Date) ) + 
  geom_line( aes( y = logret ) ) +
  labs( title = "S&P 500 log Returns")


# Construct and fit a regime switching model
mod <- depmix(logret ~ 1, family = gaussian(), nstates = 4, data = sp500LR)
set.seed(1)
fm2 <- fit(mod, verbose = FALSE)
#
summary(fm2)
print(fm2)

# Classification (inference task)
probs <- posterior(fm2)             # Compute probability of being in each state
head(probs)
rowSums(head(probs)[,2:5])          # Check that probabilities sum to 1

pBear <- probs[,2]                  # Pick out the "Bear" or low volatility state
sp500LRdf$pBear <- pBear            # Put pBear in the data frame for plotting

# Pick out an interesting subset of the data or plotting and
# reshape the data in a form convenient for ggplot
df <- melt(sp500LRdf[400:500,6:8],id="Date",measure=c("logret","pBear"))
#head(df)

# Plot the log return time series along withe the time series of probabilities
qplot(Date,value,data=df,geom="line",
      main = "SP 500 Log returns and 'Bear' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")


## Exercicio 5 

library(ggplot2)
library(depmixS4)


sigma = c(1, 3, 5, 7)
p_i = rep(1/4, 4)
fda_pi = cumsum(p_i)
N=2

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
