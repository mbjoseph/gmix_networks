# clustering within days and then summing across all networks
library(mclust)
library(dplyr)
d <- read.csv('data/RFID9_13.csv')
d$times <- (d$Epoch - mean(d$Epoch))
nbird <- length(unique(d$RFID))
traps <- levels(d$Trap.ID)
ntrap <- length(traps)
A <- array(0, dim=c(nbird, nbird, ntrap))

# for each trap, cycle through the days and cluster within each,
# generating an A matrix for each day
for (i in 1:ntrap){
  dates <- unique(d$Date[d$Trap.ID == traps[i]])
  for (j in 1:length(dates)){
    subd <- filter(d, Date == dates[j], Trap.ID == traps[i])
    birds <- length(unique(subd$RFID))
    if (birds == 1) {
      # if all observations correspond to one individual, move on
      next
    } else if (length(unique(subd$times)) == 1) {
      # all birds seen at same time implies interaction
      print('Times are all the same - is this an error?')
      next
    } else {
      # cluster and plot
      dzmod <- Mclust(subd$times, prior=priorControl(), 
                      G=1:nrow(subd), modelName='V')
      plot(subd$times, col=dzmod$classification)
      dzGamma <- round(dzmod$z)
      k_trap <- as.numeric(subd$RFID) # bird ids seen
      
      # bird X event matrix
      B <- array(0, dim=c(nbird, dzmod$G))
      for (k in 1:nbird){
        if (k %in% k_trap){
          n_events <- sum(k == k_trap)
          submat <- dzGamma[k_trap == k, ]
          if (n_events == 1){
            B[k, ] <- submat
          } else {
            if (is.vector(submat)){
              # case with just one event
              B[k, ] <- sum(submat)
            } else {
              B[k, ] <- colSums(submat)
            }
          }
        }
      }
      
      # birdXbird matrix
      A_sub <- array(dim=c(nbird, nbird))
      for (k in 1:nbird){
        for (l in 1:nbird){
          submat <- B[c(k, l), ]
          if (is.vector(submat)){
            A_sub[k, l] <- sum(min(submat))
          } else {
            A_sub[k, l] <- sum(apply(submat, 2, min))
          }
        }
      }
      diag(A_sub) <- 0
    }
    # add in new interactions
    A[, , i] <- A[, , i] + A_sub
    }
}

par(mfrow=c(3, 3))
for (i in 1:ntrap) image(A[, , i], main=paste('Trap', i))

A_sum <- matrix(nrow=nbird, ncol=nbird)
for (i in 1:nbird){
  for (j in 1:nbird){
    A_sum[i, j] <- sum(A[i, j, ])
  }
}
par(mfrow=c(1, 1))
image(A_sum)

library(igraph)
library(scales)
gA <- graph_from_adjacency_matrix(A_sum, mode='undirected', weighted=TRUE)
par(mfrow=c(1, 1))
par(mar=c(2, 1, .5, .5))
plot(gA, edge.color=alpha('black', .2), layout=layout.fruchterman.reingold(gA), 
     vertex.size=2, edge.width=E(gA)$weight/2)

