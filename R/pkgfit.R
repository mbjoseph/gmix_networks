# mixture modeling packages
source('rwalk.R')

library(mclust)
mod2 <- Mclust(timesteps, G=1:40, modelName='V')
plot(mod2)
1
2
3
4
0
mod2
max(clus)

# tainted timesteps
source('rwalk.R')

library(mclust)
mod2 <- Mclust(timesteps + rnorm(length(timesteps), 0, 2), 
               G=1:40, modelName='V')
plot(mod2)
1
2
3
4
0
mod2
max(clus)




library(mixtools)
k <- 4
mod1 <- normalmixEM(timesteps, 
                    mu=c(seq(min(timesteps), max(timesteps), length.out=k)))
par(mfrow=c(2, 2))
plot(mod1, density=TRUE, breaks=30)

image(mod1$posterior)
title('Posterior assignment probabilities')
image(clusmat)
title('True cluster origins')

