#Network in R - 
#Romaric Mouafo Tchinda and Aaron Plex Sula
#Jul 27th, 2024


#---------------------------Basics--------------------------#

#The igraph package in R is a useful library for creating and manipulating graphs and analyzing network data. It is widely used for network analysis in various fields, including epidemiology, plant pathology and computer science.

# First, install the package
install.packages("igraph")
# Second, load the package using library()
library(igraph)

# Plot an example of a simple network

Net2 <- graph(edges=c(1,3, 3,2, 2,1, 2,4, 5,4), n=5, directed=F) 
Net2
plot(Net2, edge.arrow.size = .5, vertex.color = "gold")

# Calculate the node degree
igraph::degree(Net2, v = V(Net2), mode = c("all"))
#You can see that node 1 has 2 links or edges, node 2 has 3, node 3 has 2, node 4 has 2 and node 5 has 1.

# Let's 
UndirectNet <- graph(edges = c(1,3, 1,2, 1,4, 2,4), n = 4, directed = F)
DirectNet <- graph(edges = c(1,3, 1,2, 1,4, 2,4), n = 4, directed = T)

# Let's print them
DirectNet
UndirectNet

# Let's plot them
plot(UndirectNet, edge.arrow.size = .5)
plot(DirectNet, edge.arrow.size = .5)

#Network density
#Note: the "igraph::" is just a way to remind R that the function
Densityg2 <- igraph::graph.density(UndirectNet)
Densityg3 <- igraph::graph.density(DirectNet)
Densityg2
Densityg3

#"degree()" comes from the igraph package. This is not necessary
#but helps for reproducible code
degAll <- igraph::degree(DirectNet, mode = "total")
degIn <- igraph::degree(DirectNet, mode = "in")
degOut <- igraph::degree(DirectNet, mode = "out")

hist(degAll, xlab = "Node Degree")
hist(degIn, xlab = "Node in-degree")
hist(degOut, xlab = "Node out-degree")

#Other Useful Network-Level Measures

Net3 <- graph(edges=c(1,2,2,3,3,4,1,4,4,5,5,6,6,7,7,8,6,8,3,1), 
              n=8, directed=F) 
Net3
plot(Net3, edge.arrow.size = .5,vertex.color = "gold")

#Network density
#Network density measures the number of edges in a network in relation to the maximum number of
#possible edges. It gives an indication of the connectivity of the network
graph.density(Net3) 


##Degree centrality (node degree) - the number of links a node has to other nodes in the 
#network (both incoming and outgoing)

igraph::degree(Net3)

## Betweenness Centrality – the number of shortest paths through the network of which a node is a part
igraph::betweenness(Net3)

## Closeness Centrality – the inverse of the average length of the shortest path to/from all 
#the other nodes in the network
igraph::closeness(Net3)

## Transitivity - the probability that two nodes are connected if they share a 
#mutual neighbor(Friends of friends are often friends) 
### (Proportion of triangles over the total number of connected triplets)
igraph::transitivity(Net3)

## Diameter -The diameter of a graph is the length of the longest geodesic distance (shortest path).
### The length of the shortest path of the furthest nodes
igraph::diameter(Net3)

#Average Geodesic - the average geodesic length (also called average shortest path length) of 
#a network is the average length of the shortest paths between all pairs of nodes. 
#This measure gives an indication of the connectivity and overall efficiency of the network.
igraph::mean_distance(Net3)


#---------------------------case study--------------------------#
# informal potato seed trade in Ethiopia between region

SeedETH <- read.csv("Informal-seed-trade-Ethiopia.csv")
head(SeedETH)
class(SeedETH)

# Store the row names
rownames<-c("Tigray","Afar","Amhara",
            "Gumuz","Somalia","Oromia","Gambela","SN",
            "Sidama","SW","Harari","Addis","Diredawa")

SeedETH <- SeedETH[, -1]  # Remove the first column
SeedETH <- as.matrix(SeedETH)

# Add row names to the matrix (if needed for display purposes)
rownames(SeedETH) <- rownames

#Let's set the diagonal values to zero, since we consider intra-regional trade to be a fact
diag(SeedETH) = 0

# Generating a graph object from an adjacency matrix
#ETHnet <- graph.adjacency(SeedETH, mode="undirected", diag=F,weighted=T)

ETHnet <- graph.adjacency(SeedETH, mode="directed", diag=F,weighted=T)

# Checking the content of the graph object
ETHnet

E(ETHnet) #the edges of the graph
V(ETHnet) #the vertices (or nodes) of the graph 

# Visualizing the graph object
plot(ETHnet, edge.width = E(ETHnet)$weight)

plot(ETHnet, edge.width = E(ETHnet)$weight, edge.arrow.size=.2, edge.curved=.5)

plot(ETHnet, edge.width = E(ETHnet)$weight, edge.arrow.size=.2, edge.curved=.5,
     edge.color="grey",
     vertex.color="gold", vertex.frame.color="black", vertex.label.color="darkblue") #, vertex_label_size=150

#Network density
graph.density(ETHnet) 

#Node degree
##Degree centrality - The number of links a node has to other nodes in the network (both incoming and outgoing)

igraph::degree(ETHnet)

## Betweenness Centrality – The number of shortest paths through the network of which a node is a part
igraph::betweenness(ETHnet)

# Note: the Oromia region has the highest value of node degree but the lowest value of 
#Betweenness Centrality. This is not normal, as usually, Betweenness Centrality calculates the 
#number of shortest paths with distance, but in this case, links represent expert confidence, 
#so the higher the value, the greater the confidence.

x<-max(E(ETHnet)$weight)*1.01-E(ETHnet)$weight

Betw <- igraph::betweenness(ETHnet, weights = x, directed = FALSE)

plot(ETHnet, vertex.size = Betw*.5, edge.width = E(ETHnet)$weight, edge.arrow.size=.2, edge.curved=.5, main = "Betweenness Centrality")


#Closeness Centrality – The inverse of the average length of the shortest path to/from all the 
#other nodes in the network
igraph::closeness(ETHnet)

Clos <- igraph::closeness(ETHnet, weights = x)

plot(ETHnet, vertex.size = Clos*500, edge.width = E(ETHnet)$weight, edge.arrow.size=.2, edge.curved=.5, main = "Closeness Centrality")


#################### the art of ploting networks ##############################

ETHnet.strength <-strength(ETHnet, v = V(ETHnet), mode = c("all"), weights = E(ETHnet)$weight)

library(viridis)
palf <- colorRampPalette(c("gray80", "darkred")) 
redPalette<-palf(10)
infernoPalette<-viridis(n=10, option = "inferno")

library(ggraph)
#the syntax of ggraph follows that of ggplot
ETHnet.net<-
  ggraph(ETHnet, layout = "stress")+
  geom_edge_link(aes(edge_width= E(ETHnet)$weight,
                     start_cap = circle(.3), 
                     end_cap = circle(.7)),
                 edge_colour=redPalette[5],
                 arrow = arrow(angle = 50,
                               length = unit(0.15, "inches")))+
  geom_node_point(aes(fill=ETHnet.strength,
                      color=ETHnet.strength,
                      size=ETHnet.strength))+
  geom_node_text(aes(label = name),
                 repel = TRUE)+
  scale_edge_width_continuous(range = c(0.1, 2))+
  scale_size_continuous(range = c(1,9))+
  scale_fill_viridis(option = "viridis")+
  scale_color_viridis(option = "viridis")+
  theme_graph()

ETHnet.net+
  guides(fill = guide_legend(title = "Regional informal \nseed trade strength"),
         colour = guide_legend(title = "Node \nstrength"),
         size = guide_legend(title = "Regional informal \nseed trade strength"),
         edge_width = guide_legend(title = "Informal seed \ntrade level"),
         edge_linetype = guide_legend(title = "Seed \nmovement"))

################ Detecting communities in your network #######################

walktrap.groups <- cluster_walktrap(ETHnet) # one community detection algorithm
membership(walktrap.groups) # which detected community is each node in?
modularity(walktrap.groups) # what is the modularity for this set of communities?

# Use module information for plotting the communities
walktrap.groups$membership<-ifelse(walktrap.groups$membership == 1, "#ff99cc", "#00ffcc")
# For more information on how to plot using the igraph package see the link below:
# https://kateto.net/netscix2016.html
plot(walktrap.groups,ETHnet, vertex.label.color = "black", vertex.frame.color = NA, 
     edge.width = 1.5, edge.color = "grey60", edge.arrow.size=0.4,
     main = "Community detection based on \nWalktrap algorithm")


optimal.groups <- cluster_optimal(ETHnet)
optimal.groups$membership<-ifelse(optimal.groups$membership == 1, "#ff99cc", "#00ffcc")
plot(optimal.groups,ETHnet, vertex.label.color = "black", 
     vertex.label.cex = 2, vertex.frame.color = NA,
     edge.width = 1.5, edge.color = "grey60", edge.arrow.size=0.4,
     main = "Community detection based on \nOptimal algorithm")
#igraph includes a number of community detection algorithms!
#check them out here http://igraph.org/r/doc/communities.html

