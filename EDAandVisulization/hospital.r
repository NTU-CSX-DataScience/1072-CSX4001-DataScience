isfar <- load("data.RData")
factordf <- all.data[,which(colnames(all.data) %in% c1)]
factordf <- t(factordf)
factordf <- data.frame(factordf)
drawnames = as.character(all.data$.rownames)
names(factordf) = drawnames

factordf = as.matrix(factordf)
co_occurrence <- t(factordf) %*% factordf
total_occurrences <- colSums(factordf)
smallid = total_occurrences[which(total_occurrences < median(total_occurrences))]
co_occurrence_d = co_occurrence / total_occurrences
co_occurrence_s = co_occurrence_d[-as.vector(smallid),-as.vector(smallid)]

require(igraph)
graph <- graph.adjacency(round(co_occurrence_s),
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)

plot(graph,
     vertex.label=names(data),
     edge.arrow.mode=0,
     vertex.size=1,
     edge.width=E(graph)$weight,
     layout=layout_with_fr)

heatmap(co_occurrence_s, scale="column", col = cm.colors(256))

library(reshape2)
melted_cormat <- melt(co_occurrence_s)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

  
