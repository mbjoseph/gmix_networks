# simulation of observations at one location
library(igraph)

n_nodes <- sample(5:20, 1)
Anet <- sample_pa(n_nodes, directed=FALSE)
plot(Anet)
Atrue <- as.matrix(as_adj(Anet))
s <- colSums(Atrue)
# maximum number of observations
zmax <- 1000
b <- rep(NA, zmax)
timesteps <- rep(NA, zmax)
clus <- rep(NA, zmax)

# initialize
time <- 1
z <- 1
start_node <- sample(n_nodes, 1)

timesteps[1] <- time
b[1] <- start_node
clus[1] <- 1

# generate observations
while(z < zmax){
  # how many steps in the random walk
  nsteps <- rpois(1, 15)
  focal_node <- b[z]
  while(nsteps > 0){
    if (b[z] == focal_node){
      # probabilities of walking to each node
      p <- Atrue[b[z], ] / s[b[z]]
      visit <- sample(n_nodes, 1, prob=p)
      z <- z + 1
      timesteps[z] <- timesteps[z - 1] + 1
      nsteps <- nsteps - 1
      clus[z] <- clus[z - 1]
      b[z] <- visit
    } else { # the RW is in a neighbor of the focal node
      stopifnot(b[z] %in% which(Atrue[focal_node, ] > 0))
      # define common neighbors
      Ac <- Atrue[focal_node, ] * Atrue[b[z], ]
      if (sum(Ac) == 0){
        visit <- focal_node
      } else { 
        p <- Ac / sum(Ac)
        visit <- sample(n_nodes, 1, prob=p)
      }
      z <- z + 1
      timesteps[z] <- timesteps[z - 1] + 1
      nsteps <- nsteps - 1
      clus[z] <- clus[z - 1]
      b[z] <- visit
    }
  }
  if (z < zmax){
    z <- z + 1
    timesteps[z] <- timesteps[z - 1] + rpois(1, 200)
    clus[z] <- clus[z - 1] + 1
    b[z] <- sample(n_nodes, 1)
  }
}

d <- data.frame(b, timesteps)
par(mfrow=c(3, 2))
plot(timesteps, col=b, pch=19, 
     xlab='z = 1, ..., Z', 
     ylab=expression(paste(t == t[1], ", ...,", t[Z])))
legend('topleft', col=1:n_nodes, pch=19,
       legend = paste('Individual', 1:n_nodes), 
       bty='n')
plot(Anet)

clusmat <- array(dim=c(length(b), max(clus)))
for (i in 1:max(clus)){
  clusmat[, i] <- as.numeric(clus == i)
}