# Ryan Brue 2022

# install.packages("networkR")
library(igraph)
library(networkR)

# memory.size()
# memory.limit()
# memory.limit(size=56000)

f <- read.table(file = "./GitHub/java-discrete-messages-analysis/data/guavagraph.txt")
f2 <- read.table(file = "./GitHub/java-discrete-messages-analysis/data/guavanames.txt")

n <- max(f$V1)
n2 <- max(f$V3)
n3 <- max(n, n2)

g <- make_empty_graph(n = n3, directed = TRUE)

for (e in 1:length(f$V1)){
  g <- g %>% add_edges(c(f$V1[e], f$V3[e]), attr = list(weight = f$V2[e]))
}

V(g)$name <- f2$V1

# Directed graph

g.adj <- get.adjacency(g, attr = "weight")
# g.adj2 <- as.matrix(g.adj)

# plot(g, edge.label = E(g)$weight)

# Undirected graph (sum of weights of directed edges)

g2 <- as.undirected(g, mode = "collapse", edge.attr.comb=list(weight=length))

g2.adj <- get.adjacency(g2, attr = "weight")
# g2.adj2 <- as.matrix(g2.adj)

# plot(g2, edge.label = E(g2)$weight)

# Hyperlink-induced Topic Search

g.hits <- hits(as.matrix(g.adj)) # Yay hits

names(g.hits$hubs) <- names(V(g))
names(g.hits$authorities) <- names(V(g))

# Sorting hub and authority weights

g.hits$sortedhubs <- sort(g.hits$hubs)
g.hits$sortedauthorities <- sort(g.hits$authorities)

# Printing top 10 hubs and authorities

print("Top 10 Hubs")
for (e in (length(g.hits$sortedhubs)-9):(length(g.hits$sortedhubs))){
  print(g.hits$sortedhubs[e])
}

print("Top 10 Authorities")
for (e in (length(g.hits$sortedauthorities)-9):(length(g.hits$sortedauthorities))){
  print(g.hits$sortedauthorities[e])
}



Isolated = which(degree(g2)==0)

g3 = delete.vertices(g2, Isolated)
g3 <- simplify(g3)
Isolated = which(degree(g3)==0)
g3 <- delete.vertices(g3, Isolated)

g3.adj <- get.adjacency(g3, attr = "weight")

sort(degree(g3))

length(V(g3))


# Laplace Spectral Clustering

laplace_cluster_func <- function(G){
  if (length(V(G)) < 5){
    # plot.igraph(G, vertex.size = 7, label=FALSE)
    # title(sprintf("Result Cluster for G"),cex.main=1,col.main="green")
    clust.membership[as.numeric(V(G))] <<- current.clustnum
    current.clustnum <<- current.clustnum + 1
    clusters.graphlist <<- append(clusters.graphlist, list(G))
    return()
  }
  
  if (clusters(G)$no != 1){
    return()
  }
  
  temp.col <- V(g3)$color
  # print(length(V(G)))
  G.adj <- get.adjacency(G, attr = "weight")
  G.deg <- rowSums(as.matrix(G.adj))
  G.Lap <- diag(G.deg) - G.adj
  G.n <- length(V(G))
  
  x <- eigen(G.Lap)$vectors[,G.n-1]
  x_val <- eigen(G.Lap)$values[G.n-1]
  names(x) <- names(V(G))
  
  if(any(x==0.0)){
    # plot.igraph(G, vertex.size = 5)
    # title("Uh oh")
  }
  
  
  if (length(x) < 100){
    # cat("x: ", x, "\n")
  }
  
  if (all(x<=0)){
    x_clusters <- ifelse(x<0,1,-1)
  }
  else if (all(x>=0)){
    x_clusters <- ifelse(x>0, -1, 1)
  }
  else{
    x_clusters <- ifelse(x<0,1,-1)
  }
  
  # V(G)$color <- ifelse(x_clusters>0,rgb(red1, green1, blue1, 255,maxColorValue=255),rgb(red2, green2, blue2, 255,maxColorValue=255))
  
  cluster1 <- which(x_clusters<0)
  cluster2 <- which(x_clusters>0)
  
  
  
  # cat("Cluster 1:", cluster1, "\n")
  # cat("Cluster 2:", cluster2, "\n")
  
  G2 <- induced_subgraph(G, cluster1)
  
  # plot.igraph(G2, vertex.size = 7, label=FALSE)
  # title("This is my first igraph",cex.main=3,col.main="green")
  
  vertices1 <- V(g3)[cluster1]
  vertices2 <- V(g3)[cluster2]
  
  
  E1 = which(sapply(E(g3), function(e) (ends(g3, e)[1] %in% names(V(G2)))))
  E2 = which(sapply(E(g3), function(e) (ends(g3, e)[2] %in% names(V(G2)))))
  
  Internal = intersect(E1, E2)
  External = setdiff(union(E1,E2), Internal)
  
  G3 <- induced_subgraph(G, cluster2)
  
  # plot.igraph(G3, vertex.size = 7, label=FALSE, )
  # title("This is my first igraph",cex.main=3,col.main="green")
  
  
  
  E3 = which(sapply(E(g3), function(e) (ends(g3, e)[1] %in% names(V(G3)))))
  E4 = which(sapply(E(g3), function(e) (ends(g3, e)[2] %in% names(V(G3)))))
  
  Internal2 = intersect(E3, E4)
  External2 = setdiff(union(E3,E4), Internal2)
  
  #cat("Internal ", Internal, "\n")
  #cat("External ", External, "\n")
  
  if (clusters(G2)$no != 1 || clusters(G3)$no != 1){
    # cat("Done G: ", sum(Internal), " <= ", sum(External), "\n")
    # plot.igraph(G, vertex.size = 7, label=FALSE)
    # title(sprintf("Result Cluster for G"),cex.main=1,col.main="green")
    clust.membership[as.numeric(V(G))] <<- current.clustnum
    current.clustnum <<- current.clustnum + 1
    clusters.graphlist <<- append(clusters.graphlist, list(G))
    return()
  }
  
  if (sum(Internal) > 1 && sum(Internal) > sum(External) && clusters(G2)$no == 1){
    # cat("Next step: G2: ", sum(Internal), " > ", sum(External), "\n")
    
    laplace_cluster_func(G2)
  }
  else{
    # cat("Done G2: ", sum(Internal), " <= ", sum(External), "\n")
    # plot.igraph(G2, vertex.size = 7, label=FALSE)
    # title(sprintf("Result Cluster for G2"),cex.main=1,col.main="green")
    clust.membership[cluster1] <<- current.clustnum
    current.clustnum <<- current.clustnum + 1
    clusters.graphlist <<- append(clusters.graphlist, list(G2))
    # V(g3)$color <<- temp.col
  }
  
  # cat("Internal 2", Internal2, "\n")
  # cat("External 2", External2, "\n")
  
  if (sum(Internal2) > 1 && sum(Internal2) > sum(External2)){
    # cat("Next step: G3: ", sum(Internal2), " > ", sum(External2), "\n")
    
    laplace_cluster_func(G3)
  }
  else{
    # cat("Done G3: ", sum(Internal2), " <= ", sum(External2), "\n")
    # plot.igraph(G3, vertex.size = 7, label=FALSE)
    # title(sprintf("Result Cluster for G3"),cex.main=1,col.main="green")
    clust.membership[cluster2] <<- current.clustnum
    
    current.clustnum <<- current.clustnum + 1
    
    clusters.graphlist <<- append(clusters.graphlist, list(G3))
    # V(g3)$color <<- temp.col
  }
  
  
  
  
  
  return ()
  
  
}
  
# library(igraphdata)
# data(karate)
# laplace_cluster_func(karate, karate)

# plot.igraph(karate, vertex.size = 5)

# g3.deg <- rowSums(as.matrix(g3.adj))
# g3.Lap <- diag(g3.deg) - g3.adj

# g3.n <- length(V(g3))  # Number of Nodes

# Getting Fiedler Value / Fiedler Vector

# x <- eigen(g3.Lap)$vectors[,g3.n-1]
# x_val <- eigen(g3.Lap)$values[g3.n-1]
# names(x) <- names(V(g3))
# x
# x_clusters <- ifelse(x>0,1,-1)

# V(g3)$label.cex = 0.05

# plot annotation
# V(g3)$color <- ifelse(x_clusters>0,"green","yellow")

# V(g2)$size <- 50*abs(x)  # node radius by magnitude of v
# plot(g2, edge.label = E(g2)$weight)
g3 <- simplify(g3)

clusters(g3)$no

clusts <- clusters(g3)

clusters.graphlist <- list()


current.clustnum <- 1
clust.membership <- clusts$membership
clust.membership[c(1:length(clust.membership))] <- -1

for(i in c(1:clusts$no)){
  clust_list <- which(clusts$membership == i)
  g3.subgraph <- induced_subgraph(g3, clust_list)
  # plot.igraph(g3.subgraph, vertex.size = 2)
  # title(sprintf("Cluster %s", i))
  laplace_cluster_func(g3.subgraph)
}

library(randomcoloR)
palette <- distinctColorPalette(length(clusters.graphlist))

for(i.2 in c(1:length(clusters.graphlist))){
  g3.subgraph <- clusters.graphlist[[i.2]]
  V(g3.subgraph)$label.cex = 0.2
  # plot.igraph(g3.subgraph, vertex.size = 5)
  # title(sprintf("Cluster %s", i.2))
  common <- intersect(V(g3)$name, V(g3.subgraph)$name)
  V(g3)[common]$color <- palette[i.2]
}

# g3 <- laplace_cluster_func(g3, g3)
V(g3)$label.cex = 0.2

plot.igraph(g3, vertex.size = 5)
length(V(g3))
# V(g3)$color

# as.numeric(V(clusters.graphlist[[5]]))


# which(clusts$membership==2)
# v <- V(g3)[1:10]

# E3 = which(sapply(E(g3), function(e) (ends(g3, e)[1] %in% names(v))))
# E4 = which(sapply(E(g3), function(e) (ends(g3, e)[2] %in% names(v))))

# Internal2 = intersect(E3, E4)
# External2 = setdiff(union(E3,E4), Internal2)

# Scale-Freeness Check

# theoretical poison probs based on observed degrees
p.pois <- dpois(k.seq, mean(kvec), log=F) 
lines(k.seq, p.pois)

# let's comapre poison and scale-free for g2
g2.kvec <- degree(g2)
# is it scale free or is the network too small to tell?
g2.hist <- hist(g2.kvec, freq=FALSE, breaks = 50, main="")
title("Degree distribution of ElasticSearch")
g2.mean <- mean(g2.kvec)
k.seq <- 0:max(g2.kvec)  # x-axis

# make a guess for scale-free alpha for now
alpha <- 1.583422  # the value after simple fitting is 1.74
# get rid of low k.seq values
# 0 and low k can be problematic for 1/k model
k.seq2 <- k.seq[-c(1,2)] # remove first two elements
lines(k.seq2, k.seq2^(-alpha), col="red")

# try to estimate alpha for g2
deg.breaks <- g2.hist$breaks[-c(1,2)] #rm low degrees
deg.probs <- g2.hist$density[-1] # make lengths match

# some of the probabilites are 0. 
# Causes problem with log in lm model
nz.probs.mask <- deg.probs!=0  # boolean of non-zero probs
deg.breaks.clean <- deg.breaks[nz.probs.mask]
deg.probs.clean <- deg.probs[nz.probs.mask]
# linear model, least-squares fit to data
# fit <- lm(y~x)  fits model y = b0 + b1*x
# fit <- lm(y~x+0)  fits model y = b1*x
degree.fit <- lm(log(deg.probs.clean)~log(deg.breaks.clean))
summary(degree.fit)
coef(degree.fit)[2]  # slope of model, scale-free param
# 1.74

# after degree cleaning, let's use the max-log-likelihood
# analytical (formla) estimate for alpha
n<- length(deg.breaks.clean)
kmin <- deg.breaks.clean[1]
#kmin <- 2  #?
alpha.ML <- 1 + n/sum(log(deg.breaks.clean/kmin))
alpha.ML  # 2.13


