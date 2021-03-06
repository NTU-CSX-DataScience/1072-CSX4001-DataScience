data <- read.table(header=T, row.names=1, text=
                     "    the fat cat
                   one   1   1   0
                   two   1   0   1
                   three 1   2   1")

total_occurrences <- colSums(data)
data_matrix <- as.matrix(data)

co_occurrence <- t(data_matrix) %*% data_matrix


library(igraph)
graph <- graph.adjacency(co_occurrence,
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)

plot(graph,
     vertex.label=names(data),
     vertex.size=total_occurrences*18,
     edge.width=E(graph)$weight*8)
