# Network Analysis Seminar - FLAMES
# 3rd May 2021

#### Load packages and data ####
library(data.table)
library(tidyverse)
library(igraph)
library(igraphdata)
library(visNetwork)
library(ggraph)

#### Import infections data ####

infect <- fread("data/infect.csv") %>%
  mutate(t = as.character(t))

View(infect) #View data

infect <- infect %>%
  graph_from_data_frame(direct = TRUE)

# Add degree as label
infect_deg <- set_vertex_attr(infect, "degree", index = V(infect),
                          degree(infect))

# Add eigenvector as label
infect_eg <- set_vertex_attr(infect, "eigenvector", index = V(infect),
                          eigen_centrality(infect)$vector)

# Plot degree
quantile(V(infect_deg)$degree)
g_deg <- subgraph.edges(infect_deg, V(infect_deg)[degree>500])
plot(g_deg, edge.arrow.size=.2,
     vertex.size = sqrt(V(g_deg)$degree/100)*4)

max_g <- induced_subgraph(g_deg, V(g_deg)[degree == max(V(g_deg)$degree)])
plot(max_g)


# Plot eigenvector
g_eg <- subgraph.edges(infect_eg, V(infect_eg)[eigenvector<100])
plot(g_eg, edge.arrow.size=.2,vertex.size = sqrt(V(g_eg)$eigenvector*1000))

max_g <- induced_subgraph(g_eg, V(g_eg)[eigenvector == max(V(g_eg)$eigenvector)])
plot(max_g)



#### News dataset ####

# Loading and cleaning data
edges <- fread("data/edges.csv")
nodes <- fread("data/nodes.csv")


# Create graph
g <- graph_from_data_frame(edges, nodes, directed=TRUE)

# Plot
plot(g, edge.arrow.size=.4,vertex.label=NA)

# Remove loops
g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
plot(g, edge.arrow.size=.4,vertex.label=NA)

# Set edge color to gray, and the node color to orange.
# Replace the vertex label with the node names stored in "media"

plot(g, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(g)$media, vertex.label.color="black",
     vertex.label.cex=.7)


# Diameter
diameter(g, directed = TRUE, weights = E(g)$weight)
diameter(g, directed = FALSE, weights=NA)

# Degree
deg <- degree(g, mode="all")

plot(g, vertex.size=deg*3,
     vertex.label=V(g)$media, edge.arrow.size=.2, vertex.label.cex=.7)

eig <- eigen_centrality(g)$vector

plot(g, vertex.size=eig*25,
     vertex.label=V(g)$media, edge.arrow.size=.2, vertex.label.cex=.7)

#par(mar = c(0,0,0,0))

# Plot using ggraph
ggraph(g, layout = "fr") +
  geom_edge_fan(color = "#1DACE8", alpha = .5) +
  geom_node_point(aes(size = deg), color = "#1DACE8", show.legend = FALSE) +
  geom_node_text(aes(label = media, size = deg)) +
  theme_graph(fg_text_colour = 'white')

# NOTE:
# Layouts 'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds',
# 'randomly', 'fr', 'kk', 'drl', 'lgl'
# https://www.data-imaginist.com/2017/ggraph-introduction-layouts/



# Cool movable plots (although not that useful)

nodes$label <- nodes$media
nodes$groups <- nodes$type.label

nodes <- nodes %>%
  mutate(color.background = case_when(type.label == "Newspaper" ~ "#1DACE8",
                                      type.label == "TV" ~ "#F24D29",
                                      type.label == "Online" ~ "#76A08A"))

visNetwork(nodes, edges) %>%
  visEdges(color = "#1DACE8") %>%
  visNodes(color = list(border = "#1DACE8"))


# Clustering

# Edge betweenness
ceb <- cluster_edge_betweenness(g)
dendPlot(ceb, mode="hclust")

plot(ceb, g,
     vertex.label=V(g)$media, edge.arrow.size=.2, vertex.label.cex=.7)

# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, g,
     vertex.label=V(g)$media, edge.arrow.size=.2, vertex.label.cex=.7)

V(g)$community <- cfg$membership
colrs <- adjustcolor( c("#1DACE8", "#F24D29", "#76A08A", "#1C366B"), alpha=.6)
plot(g, vertex.color=colrs[V(g)$community],
     vertex.label=V(g)$media, edge.arrow.size=.2, vertex.label.cex=.7)


