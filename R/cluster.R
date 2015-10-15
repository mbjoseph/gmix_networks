library(mclust)
d <- read.csv('data/RFID9_13.csv')
d$times <- (d$Epoch - mean(d$Epoch)) / 1e5
#times <- c(scale(d$Epoch))

trap <- levels(d$Trap.ID)[3]
d <- subset(d, Trap.ID == trap)
d <- droplevels(d)
str(d)
d <- subset(d, Date == levels(d$Date)[2])
k<- as.numeric(d$RFID)
k_trap <- k
dzmod <- Mclust(d$times, prior=priorControl(), 
              G=1:3, modelName='V')
plot(d$times, col=dzmod$classification)

par(mfrow=c(1, 1))
plot(dzmod)
1
2
4
0
dzmod


dzGamma <- dzmod$z
for (i in 1:nrow(dzGamma)){
  dzGamma[i, ] <- ifelse(dzGamma[i, ] == max(dzGamma[i, ]), 1, 0)
}

nod<-as.numeric(length(unique(d$RFID)))
nod

B <- array(0, dim=c(nod, dzmod$G))
for (i in 1:nod){
  if (i %in% k_trap){
    n_events <- sum(i == k_trap)
    submat <- dzGamma[k_trap == i, ]
    if (n_events == 1){
      B[i, ] <- submat
    } else {
      B[i, ] <- colSums(submat)
    }
  }
}


A <- array(dim=c(nod, nod))
for (i in 1:nod){
  for (j in 1:nod){
    submat <- B[c(i, j), ]
    A[i, j] <- sum(apply(submat, 2, min))
  }
}
diag(A) <- 0

# visualize graph
library(igraph)
library(scales)
nodes_obs <- unique(k_trap)
gA <- graph_from_adjacency_matrix(A[nodes_obs, nodes_obs], mode='undirected')
par(mfrow=c(1, 1))
par(mar=c(2, 1, .5, .5))
plot(gA, edge.color=alpha('black', .5))
image(A[nodes_obs, nodes_obs], col=gray.colors(12))

