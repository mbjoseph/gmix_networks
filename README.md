# gmix_networks

This is an R implementation of an algorithm developed in Psorakis et al. 2012 that takes spatiotemporal co-occurrence data and constructs social interaction networks. The idea is that events are clusters of observations in space-time, such that each observation can be probabilistically assigned to some event(s), generating an event by observation mapping. Then, individuals are mapped to the observations, generating an individual to event mapping. Finally, the assumption is that individuals that attend the same events are interacting, and individual to individual interaction networks can be inferred. 

This algorithm was originally developed for birds, but it has the potential to broadly apply to situations where entities co-occur in time at different locations, and the interaction network among entities is of interest. 
