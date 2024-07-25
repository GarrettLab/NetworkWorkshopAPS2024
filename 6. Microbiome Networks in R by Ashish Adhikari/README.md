README
================
2024-07-24

## Microbiome network analysis
# This is my title

---
title: "READMe"
output: github_document
date: "2024-07-24"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#Microbiome Network using SparCC, Correlation 

```{r}
getwd()
# Loading required packages
library(igraph)
#library(Hmisc) (for correlation)
library(Matrix)
# Install SpiecEasi package
# install.packages("devtools")
library(devtools)
# install_github("zdk123/SpiecEasi", force = TRUE)
#downloading SpiecEasi from Github
#devtools::install_github("zdk123/SpiecEasi")
library(SpiecEasi)


```

```{r}
# Load the data with the OTU table: otudata.csv
#otu.table<-read.csv(file.choose(), header=T, row.names = 1)

# Read taxonomy file associated with OTU table into new object: otu_taxonomy.csv
#tax<-read.csv(file.choose(),header=T, row.names = 1, check.name = T)

# Check how many OTUs we have
#dim(otu.table)

# Keep the OTUs with more than 10 counts
#otu.table.filter<-otu.table[,colSums(otu.table)>10]

# Check for the decrease in the number of OTUs
#dim(otu.table.filter)
#head(otu.table.filter)

```

```{r}
# SparCC network
#sparcc.matrix <- sparcc(otu.table.filter)

#this will take time may be hours depending on data size

```

```{r}
# Load the sparcc results
sparcc.matrix <- readRDS("sparcc_results.rds")
```

```{r}
##Threshold 
sparcc.cutoff <- 0.5

#changing to 0 and 1
sparcc.adj <- ifelse(abs(sparcc.matrix$Cor) >= sparcc.cutoff, 1, 0)
```

```{r}
# Add OTU names to rows and columns
rownames(sparcc.adj) <- colnames(otu.table.filter)
colnames(sparcc.adj) <- colnames(otu.table.filter)
```

```{r}
# Create an adjacency matrix in igraph format
net.grph=graph.adjacency(sparcc.adj, mode = "undirected",weighted=TRUE,
                         diag=FALSE)

# Calculate edge weight == level of correlation
edgew<-E(net.grph)$weight

# Identify isolated nodes
bad.vs<-V(net.grph)[degree(net.grph) == 0]

# Remove isolated nodes
net.grph <-delete.vertices(net.grph, bad.vs)
```
```{r}
# Plot the graph object
plot(net.grph,
     vertex.size=4,
     vertex.frame.color="black",
     edge.curved=F,
     edge.width=1.5,
     layout=layout.auto,
     edge.color=ifelse(edgew<0,"red","blue"),
     vertex.label=NA,
     vertex.label.color="black",
     vertex.label.family="Times New Roman",
     vertex.label.font=2)

# Plot the graph object
plot(net.grph,
     vertex.size=5,
     vertex.frame.color="black",
     edge.curved=F,
     edge.width=1.5,
     edge.color=ifelse(edgew<0,"red","blue"),
     vertex.label.color="blue",
     vertex.label.family="Times New Roman",
     vertex.label.font=0.5)
```

```{r}

# Load necessary library
library(igraph)

# Assume net.grph is your network graph object
net <- net.grph

# Perform Walktrap community detection
wt <- walktrap.community(net)

# Get cluster memberships
membership <- membership(wt)

# Get the size of each cluster
cluster_sizes <- sizes(wt)

# Identify the 5 largest clusters
top_clusters <- order(cluster_sizes, decreasing = TRUE)[1:5]

# Assign colors to the 5 largest clusters
cluster_colors <- rainbow(5)

# Color nodes based on cluster membership
V(net)$color <- "gray"  # Default color for smaller clusters
for (i in 1:5) {
  cluster_indices <- which(membership == top_clusters[i])
  if (length(cluster_indices) > 0) {
    V(net)$color[cluster_indices] <- cluster_colors[i]
  }
}

# Calculate hub scores for keystone taxa identification
net_hs <- hub_score(net)$vector

# Highlight keystone taxa by increasing their size
V(net)$size <- 5  # Default size
for (i in 1:5) {
  cluster_indices <- which(membership == top_clusters[i])
  if (length(cluster_indices) > 0) {
    keystone_node <- cluster_indices[which.max(net_hs[cluster_indices])]
    V(net)$size[keystone_node] <- 10  # Double the size for keystone taxa
  }
}

# Plot the graph to visualize the node sizes
plot(net, vertex.color = V(net)$color, vertex.size = V(net)$size)

```
