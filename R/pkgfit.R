# mixture modeling packages
source('R/rwalk.R')
library(scales)
library(mclust)
mod <- Mclust(timesteps, G=1:100, modelName='V')
plot(mod)
1
2
3
4
0
mod
max(clus)

# generate bird-to-cluster matrix from observation-to-cluster matrix
Gamma <- mod$z
for (i in 1:nrow(Gamma)){
  Gamma[i, ] <- ifelse(Gamma[i, ] == max(Gamma[i, ]), 1, 0)
}

B <- array(dim=c(n_nodes, mod$G))
for (i in 1:n_nodes){
  B[i, ] <- colSums(Gamma[b == i, ])
}

A <- array(dim=c(n_nodes, n_nodes))
for (i in 1:n_nodes){
  for (j in 1:n_nodes){
    submat <- B[c(i, j), ]
    A[i, j] <- sum(apply(submat, 2, min))
  }
}
diag(A) <- 0

# visualize graph
gA <- graph_from_adjacency_matrix(A, mode='undirected')
par(mfrow=c(2, 2))
par(mar=c(2, 1, .5, .5))
plot(gA, edge.color=alpha('black', .5))
plot(Anet)
image(A, col=gray.colors(12))
image(Atrue, col=gray.colors(12))
par(mar=c(5, 4, 4, 2) + 0.1)

# tainted timesteps
source('R/rwalk.R')

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

