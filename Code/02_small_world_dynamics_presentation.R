#Basic setup stuff#
rm(list=ls())

set.seed(1)

setwd("C:/Users/DR/Dropbox/PS239T Project")

### Pacman is a function that seamlessly checks to see if a package is installed.
### If it is, it loads it. If not, it installs it then loads it. It is also useful
### for its ability to simply concatenate all the packages you need as done below.

# install.packages("pacman")
library(pacman)

# igraph is the all-in-one network simulation, analysis, and visualzation package.
# Except for the lack of support for mixed bipartite projection, it is all I needed
# for this project.

# Cairo is the package I use to export high quality pdfs for importing into my
# beamer presentation.
p_load(igraph, Cairo)

# Function for converting converting mixed bipartite network to monopartite network
focus <- TRUE

monopartite <- function(network) {
  #Identify a key to parse, rejoin the network after split
  node_key <- data.frame(node_id=as.character(V(network)),type=V(network)$type)
  
  #Get the edgelist to parse into two
  edge <- get.data.frame(network)
  
  #Identify rows where only individuals tied to individuals (not focuses)
  edge$ind_only <- ifelse(edge$from %in% node_key$node_id[node_key$type!=focus] & edge$to %in% node_key$node_id[node_key$type!=focus],1,0)
  edge_ind <- edge[edge$ind_only==1,1:2] #Remove indicator, only individual-individual ties
  
  edge_bi <- edge[edge$ind_only!=1,1:2] #Remove indicator, only individual-focus ties
  
  #Collapse the edge_bi into one-mode with individuals tied by focus
  mat_bi <- as.matrix(get.adjacency(graph.edgelist(as.matrix(edge_bi),directed=FALSE))) #Get undirected adjacency matrix
  mat_bi <- mat_bi[which(node_key$type!=focus),which(node_key$type==focus)] #Reduce rows to actors, columns to focuses
  row.names(mat_bi) <- node_key$node_id[node_key$type!=focus] #Name rows appropriately for union later
  colnames(mat_bi) <- node_key$node_id[node_key$type==focus] #Name columns appropriately for union later
  mat_ind <- mat_bi%*%t(mat_bi) #Get crossproduct for individual one-mode 
  diag(mat_ind) <- 0 #Remove loops because they're nonsense here!
  
  #Combine with the individual edgelist to omit focuses altogether#
  edge_ind <- apply(edge_ind,2,as.character) #Make sure v-names characters to line up
  network21 <- graph.edgelist(as.matrix(edge_ind)) #Get just the individual network
  network22 <- graph.adjacency(mat_ind) #Get the individual network from collapse above
  network2 <- graph.union(network21,network22) #Join them
  
  # Making it undirected
  network3 <- as.undirected(network2, mode = "collapse")
  return(network3)
}

### Network examples
### Creating network
t1 <- graph_from_literal(A-B:C:D:E)

### Plotting properties
t1$layout <- layout_in_circle
V(t1)$color <- "white"
V(t1)[name=="A"]$color <- "orange"
V(t1)$size <- 40
V(t1)$label.cex <- 3
V(t1)$label <- V(t1)$name
E(t1)$color <- "black"
E(t1)$width <- 3

### Plot 't1' and A's Clustering Coefficient
tr <- transitivity(t1, type="local")[1]

CairoPDF(file = "BasicNetworkPlot1.pdf", width = 10, height = 10)
par(cex.main = 2)
plot(t1, main=paste("Transitivity of 'A':", tr))
dev.off()

### Adding an edge and recalculating transitivity
t2 <- add_edges(t1, V(t1)[name %in% c("C","D")], color="red", width=3)
tr <- transitivity(t2, type="local")[1]

CairoPDF(file = "BasicNetworkPlot2.pdf", width = 10, height = 10)
par(cex.main = 2)
plot(t2, main=paste("Transitivity of 'A':", round(tr,4)))
dev.off()

### Bipartite projection example (from the first network)
t3 <- t1
V(t3)$type <- c(TRUE, rep(FALSE, 4))

CairoPDF(file = "BipartiteConcept.pdf", width = 10, height = 10)
par(cex.main = 2)
plot(bipartite_projection(t3)[[1]], main = "Bipartite Projection")
dev.off()

### Regular Graph
ws1 <- sample_smallworld(1, 50, 3, p=0)
ws1$layout <- layout_in_circle
V(ws1)$size <- 5
E(ws1)$curved <- 1

CairoPDF(file = "RegularGraph.pdf", width = 10, height = 10)
par(cex.main = 2)
plot(ws1, vertex.label=NA, main="Regular Graph")
dev.off()

### Close-up of Regular Graph
CairoPDF(file = "RegularCloseUp.pdf", width = 10, height = 10)
plot(ws1, vertex.label=NA, xlim=c(-0.2, 0.2), ylim=c(0.8,1.1))
dev.off()


### Average shortest path length for regular graph of given size
try.ring.pl <- function(n) {
  g <- sample_smallworld(1, n, 3, p=0)
  mean_distance(g)
}

# Sequence of network sizes
network.size <- seq(100, 1000, by=100)

# Vector of average shortest path lengths of size n for regular graphs
ring.pl <- sapply(network.size, try.ring.pl)

# Plot of the above vector against network size
CairoPDF(file = "RegularPathLength.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(network.size, ring.pl,
     type="b",
     cex = 2,
     lwd = 2,
     main = "Regular Graph (Average Shortest Path Length)",
     xlab = "Network Size",
     ylab = "Path Length")
dev.off()

# Random graph
rg <- sample_gnm(50, 50 * 3)
rg$layout <- layout_in_circle
V(rg)$size <- 5

CairoPDF(file = "RandomGraph.pdf", width = 10, height = 10)
par(cex.main = 2)
plot(rg, vertex.label=NA, main="Random Graph")
dev.off()

# Average shortest path length for random graph of given size
try.random.pl <- function(n) {
  g <- sample_gnm(n, n*3)
  mean_distance(g)
}

# Vector of average shortest path lengths of size n for random graphs
random.pl <- sapply(network.size, try.random.pl)

# Plot of the above vector against network size
CairoPDF(file = "RandomPathLength.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(network.size, random.pl,
     type="b",
     cex = 2,
     lwd = 2,
     main = "Random Graph (Average Shortest Path Length)",
     xlab = "Network Size",
     ylab = "Path Length")
dev.off()

### Regular graph with 10% of ties rewired ("Semi-Random")
ws2 <- sample_smallworld(1, 50, 3, p=0.1)
ws2$layout <- layout_in_circle
V(ws2)$size <- 5

CairoPDF(file = "SemiRandomGraph.pdf", width = 10, height = 10)
par(cex.main = 2)
plot(ws2, vertex.label=NA, main = "Semi-Random Graph (10% of edges rewired)")
dev.off()

# Average shortest path length for a semi-random graph (p = 0.1) of a given size
try.rr.pl <- function(n, p) {
  g <- sample_smallworld(1, n, 3, p=p)
  mean_distance(g)
}

# Vector of average shortest path lengths of size n for semi-random graphs
rr.pl.0.1 <- sapply(network.size, try.rr.pl, p=0.1)

# Plot of the above vector against network size
CairoPDF(file = "SemiRandomPathLength.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(network.size, rr.pl.0.1,
     type="b",
     cex = 2,
     lwd = 2,
     main = "Semi-Random (Average Shortest Path Length)",
     xlab = "Network Size",
     ylab = "Path Length")
dev.off()

### Watts-Strogatz graph
# Logarithmic vector of rewiring probabilities
rewire.prob <- ((1:10)^4)/(10^4)

# Function for creating a sample small world graph for the given rewiring probability
# then calculating the mean average shorted path length for that graph
ws.paper.pl <- function(p, n=1000, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  pl <- mean_distance(g)
  pl
}

# Performing the above function 20 times (for 200 simulated graphs, 20 for each probability)
# then averaging the average shortest path lengths
ws.paper.pl.sims <- replicate(n = 20, sapply(rewire.prob, ws.paper.pl), simplify = "matrix")
ws.paper.pl.means <- rowMeans(ws.paper.pl.sims)

# Same as above, but for calculating transitivity (calculating transitivity for the
# above graphs would've been more efficient but couldn't figure out a way to effectively
# deal with the output that the replicate function gave me, I plan on making this section
# of the code more efficient later if I extend this project)
ws.paper.tr <- function(p, n=1000, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  tr <- transitivity(g, type="localaverage")
  tr
}

ws.paper.tr.sims <- replicate(n = 20, sapply(rewire.prob, ws.paper.tr), simplify = "matrix")
ws.paper.tr.means <- rowMeans(ws.paper.tr.sims)

# Combining the calculated means into a two row matrix
ws.result <- rbind(ws.paper.tr.means, ws.paper.pl.means)

# Plotting the original Watts and Strogatz plot
CairoPDF(file = "WattsStrogatzPlot.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(rewire.prob, ws.result[1,]/ws.result[1,1],
     log="x",
     cex = 2,
     pch=22,
     xlab="Rewiring Probability",
     ylab="",
     main = "Path Length and Clustering by Randomness")

points(rewire.prob, ws.result[2,]/ws.result[2,1], pch=20, cex = 2)

legend("bottomleft", c(expression(C(p)/C(0)), expression(L(p)/L(0))),
       pch=c(22, 20))
dev.off()

### Initial replication with focuses
### All the same as above except now applying focus function
### Path Length
ws.focused.pl <- function(p, n=1110, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  V(g)$type <- c(rep(FALSE, 9), TRUE)
  g <- monopartite(g)
  pl <- mean_distance(g)
  pl
}

ws.focused.pl.sims <- replicate(n = 20, sapply(rewire.prob, ws.focused.pl), simplify = "matrix")
ws.focused.pl.means <- rowMeans(ws.focused.pl.sims)

### Transitivity
ws.focused.tr <- function(p, n=1110, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  V(g)$type <- c(rep(FALSE, 9), TRUE)
  g <- monopartite(g)
  tr <- transitivity(g, type="localaverage")
  tr
}


ws.focused.tr.sims <- replicate(n = 20, sapply(rewire.prob, ws.focused.tr), simplify = "matrix")
ws.focused.tr.means <- rowMeans(ws.focused.tr.sims)

### Combine
ws.focused.result <- rbind(ws.focused.tr.means, ws.focused.pl.means)

### Results on plot (red = focused result as a ratio of the original result's starting point)
CairoPDF(file = "FocusedWattsStrogatzPlot10.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(rewire.prob, ws.focused.result[1,]/ws.result[1,1],
     log="x",
     cex = 2,
     pch=22,
     bg = "red",
     xlab="Rewiring Probability",
     ylab="",
     main = "10% Focused",
     ylim = c(0, 1.1))

points(rewire.prob, ws.focused.result[2,]/ws.result[2,1], pch=20, cex = 2, col = "red")

points(rewire.prob, ws.result[2,]/ws.result[2,1], pch=20, cex = 2)

points(rewire.prob, ws.result[1,]/ws.result[1,1], pch = 22, cex = 2)

legend("bottomleft", c(expression(C(p)/C(0)), expression(L(p)/L(0))),
       pch=c(22, 20))
dev.off()

### Repeating the above analysis, but normalizing for degree
### (giving each node of the unfocused regular graph 2 additional ties)

# Degree normalized path lengths for the new regular plot (k = 6 is the number of neighbors
# on each side the node is tied to, initially this was 5)
ws.paper.pl.sims.degree.norm <- replicate(n = 20, sapply(rewire.prob, ws.paper.pl, k=6), simplify = "matrix")
ws.paper.pl.means.degree.norm <- rowMeans(ws.paper.pl.sims.degree.norm)

# Degree normalized transitivity
ws.paper.tr.sims.degree.norm <- replicate(n = 20, sapply(rewire.prob, ws.paper.tr, k=6), simplify = "matrix")
ws.paper.tr.means.degree.norm <- rowMeans(ws.paper.tr.sims.degree.norm)

# Combining
ws.result.degree.norm <- rbind(ws.paper.tr.means.degree.norm, ws.paper.pl.means.degree.norm)

### Plot (all ratios relative to the degree normalized result's starting point)
CairoPDF(file = "FocusedWattsStrogatzPlot10Normalized.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(rewire.prob, ws.focused.result[1,]/ws.result.degree.norm[1,1],
     log="x",
     cex = 2,
     pch=22,
     bg = "red",
     xlab="Rewiring Probability",
     ylab="",
     main = "10% Focused (Degree Normalized)",
     ylim = c(0, 1.1))

points(rewire.prob, ws.focused.result[2,]/ws.result.degree.norm[2,1], pch=20, cex = 2, col = "red")

points(rewire.prob, ws.result.degree.norm[2,]/ws.result.degree.norm[2,1], pch=20, cex = 2)

points(rewire.prob, ws.result.degree.norm[1,]/ws.result.degree.norm[1,1], pch = 22, cex = 2)

legend("bottomleft", c(expression(C(p)/C(0)), expression(L(p)/L(0))),
       pch=c(22, 20))
dev.off()

### 5 Percent Focused
# Path length
ws.focused.pl <- function(p, n=1060, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  V(g)$type <- c(rep(FALSE, 19), TRUE)
  g <- monopartite(g)
  pl <- mean_distance(g)
  pl
}

ws.focused.pl.sims <- replicate(n = 20, sapply(rewire.prob, ws.focused.pl), simplify = "matrix")
ws.focused.pl.means <- rowMeans(ws.focused.pl.sims)

# Transitivity
ws.focused.tr <- function(p, n=1060, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  V(g)$type <- c(rep(FALSE, 19), TRUE)
  g <- monopartite(g)
  tr <- transitivity(g, type="localaverage")
  tr
}

ws.focused.tr.sims <- replicate(n = 20, sapply(rewire.prob, ws.focused.tr), simplify = "matrix")
ws.focused.tr.means <- rowMeans(ws.focused.tr.sims)

# Combine
ws.focused.result <- rbind(ws.focused.tr.means, ws.focused.pl.means)

# Results on plot
CairoPDF(file = "FocusedWattsStrogatzPlot5.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(rewire.prob, ws.focused.result[1,]/ws.result[1,1],
     log="x",
     cex = 2,
     pch=22,
     bg = "red",
     xlab="Rewiring Probability",
     ylab="",
     main = "5% Focused",
     ylim = c(0, 1.1))

points(rewire.prob, ws.focused.result[2,]/ws.result[2,1], pch=20, cex = 2, col = "red")

points(rewire.prob, ws.result[2,]/ws.result[2,1], pch=20, cex = 2)

points(rewire.prob, ws.result[1,]/ws.result[1,1], pch = 22, cex = 2)

legend("bottomleft", c(expression(C(p)/C(0)), expression(L(p)/L(0))),
       pch=c(22, 20))
dev.off()

### 1 Percent Focused
# Path length
ws.focused.pl <- function(p, n=1000, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  V(g)$type <- c(rep(FALSE, 99), TRUE)
  g <- monopartite(g)
  pl <- mean_distance(g)
  pl
}

ws.focused.pl.sims <- replicate(n = 20, sapply(rewire.prob, ws.focused.pl), simplify = "matrix")
ws.focused.pl.means <- rowMeans(ws.focused.pl.sims)

# Transitivity
ws.focused.tr <- function(p, n=1000, k=5) {
  g <- sample_smallworld(1, n, k, p=p)
  V(g)$type <- c(rep(FALSE, 99), TRUE)
  g <- monopartite(g)
  tr <- transitivity(g, type="localaverage")
  tr
}

ws.focused.tr.sims <- replicate(n = 20, sapply(rewire.prob, ws.focused.tr), simplify = "matrix")
ws.focused.tr.means <- rowMeans(ws.focused.tr.sims)

# Combine
ws.focused.result <- rbind(ws.focused.tr.means, ws.focused.pl.means)

# Results on plot
CairoPDF(file = "FocusedWattsStrogatzPlot1.pdf", width = 10, height = 10)
par(cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(rewire.prob, ws.focused.result[1,]/ws.result[1,1],
     log="x",
     cex = 2,
     pch=22,
     bg = "red",
     xlab="Rewiring Probability",
     ylab="",
     main = "1% Focused",
     ylim = c(0, 1.1))

points(rewire.prob, ws.focused.result[2,]/ws.result[2,1], pch=20, cex = 2, col = "red")

points(rewire.prob, ws.result[2,]/ws.result[2,1], pch=20, cex = 2)

points(rewire.prob, ws.result[1,]/ws.result[1,1], pch = 22, cex = 2)

legend("bottomleft", c(expression(C(p)/C(0)), expression(L(p)/L(0))),
       pch=c(22, 20))
dev.off()