#A cartoon example for host connectivity with geohabnet 
#Aaron I. Plex
#Jul 27th, 2024


#The objective of this exercise is to show the underlying assumptions to 
#calculate habitat connectivity in the `geohabnet` package using a simple example.

## Creating a hypothetical host landscape 
# loading libraries required
library(terra)
library(geosphere)
library(igraph)

# loading a host matrix where rows and columns represent locations
host.mat<-read.csv("simple-host-landscape.csv")
host.mat<-as.matrix(host.mat)
# Convert this host matrix into a raster object with specific spatial attributes
host.rast<-rast(host.mat, extent=c(-117,-110,32,37))

# The following code lines calculate the relative physical distance between each pair of locations with host available
nonZero <- which(values(host.rast)>0)
latilongimatr <- xyFromCell(host.rast, cell = nonZero)
distMat <- matrix(-999, nrow(latilongimatr), nrow(latilongimatr))
dvse <- geosphere::distVincentyEllipsoid(c(0,0), cbind(1, 0))
for (i in 1:nrow(latilongimatr)) {
  distMat[i, ] <- distVincentyEllipsoid(latilongimatr[i,], latilongimatr)/dvse
}

# The two code lines below calculates the relative likelihood of pathogen movement between each pair of host locations based on the inverse power law model
beta <- 0.7 #choose a beta value > 0, usually smaller than 3
distPL<-distMat^(-beta)

# Creating a matrix of potential movement between each pair of locations accounting for host availability and physical distance separately
host.density <- values(host.rast)[nonZero]
hostmatr1 <- matrix(host.density, , 1 )
hostmatr2 <- matrix(host.density, 1, )
hostmatrix <- hostmatr1 %*% hostmatr2
hostmatrix <- as.matrix(hostmatrix)

##### Here is the gravity model that incorporates host availability and the dispersal kernel outcomes
hostdistancematr <- distPL * hostmatrix

# Selecting those link weights (or entries) in the adjacency matrix that are below a threshold
logimat<-hostdistancematr>0.015
hostdistancematr<-hostdistancematr*logimat

# Converting the adjacency matrix of relative likelihood of movement into a graph object (or network)
hostnet <- graph_from_adjacency_matrix(hostdistancematr,
                                       mode=c("undirected"),diag=F,weighted=T)
plot(hostnet, edge.width = E(hostnet)$weight*20)

# Transforming link weights so that they can be used to calculate shortest paths appropriately
# These transformation is needed when calculating betweenness and closeness centraliy using igraph because link weights are considered distances
transformed.weights<-1.0001*(max(E(hostnet)$weight))-E(hostnet)$weight
#length(E(hostnet)$weight)/(dim(hostdistancematr)[1]^2-dim(hostdistancematr)[1])*100

# Calculating betweenness centrality as an example of network metric
V(hostnet)$betweenness<-closeness(hostnet, weights = transformed.weights)
plot(hostnet, edge.width = E(hostnet)$weight*20, 
     vertex.size = V(hostnet)$betweenness*20)

# Creating a raster
rast.betweenness<-host.rast
# asigning betweenness centrality of each node to each location in the raster
values(rast.betweenness)[nonZero]<-V(hostnet)$betweenness
plot(rast.betweenness, main = "Map of betweenness centrality")

#Imagine your PI asked you to run the host landscape connectivity for many beta 
#parameter values, using different dispersal kernels, multiple link thresholds 
#and multiple network metrics. Fortunately, geohabnet provides a easy way of 
#conducting that "complicated" analysis in a single function 
#(so just give it five minutes to run).

### The power of geohabnet

#Install geohabnet package from CRAN
#install.packages("geohabnet")
#or alternatively, from GitHub
#devtools::install_github("GarrettLab/HabitatConnectivity", subdir = "geohabnet")
library(geohabnet)

#use the host density map in geohabnet
rrr<-msean(host.rast, res = 1, global = FALSE, 
           geoscale = c(-117,-110,32,37),
           link_threshold = 0.015,
           inv_pl = list(beta = c(0.25, 0.5, 0.7, 1, 1.5), metrics = c("betweeness",
                                                                       "NODE_STRENGTH", "Sum_of_nearest_neighbors", "eigenVector_centrAlitY"), weights = c(50, 15, 15, 20),
                         cutoff = -1),
           neg_exp = NULL)

#CHEERS!!!

# Optional, the function below is another way of running this habitat connectivity analsysi
sensitivity_analysis() #just run this function without changing parameters, wait five minutes, and see what happens
