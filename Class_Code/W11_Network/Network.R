rm(list=ls())

library(igraph)

g <- graph.empty(directed=FALSE)

g <- g + vertex("Philip")
g <- g + vertex("Charles")
g <- g + vertex("Diana")
g <- g + vertex("William")
g <- g + vertex("Harry")
g <- g + vertex("Catherine")
g <- g + vertex("George")
g <- g + vertex("Elizabeth II")

##edges are connections between vertices

g <- g + edges("Elizabeth II", "Charles")
g <- g + edges("Philip", "Charles")
g <- g + edges("Charles", "William")
g <- g + edges("Diana", "William")
g <- g + edges("Charles", "Harry")
g <- g + edges("Diana", "Harry")
g <- g + edges("William", "George")
g <- g + edges("Catherine", "George")

g


plot.igraph(g, layout=layout_as_tree)
plot.igraph(g, layout=layout_in_circle)
plot.igraph(g, layout=layout.auto)
plot.igraph(g, layout=layout_nicely)


# The simplest centrality measure is degree centrality. Vertices are ranked only on
# the number of edges to which it is connected. In citation networks, degree centrality
# is analogous to just counting the number of citations a document has. We have
# already argued that this is a reasonable first solution, butmay miss the larger picture

degree(g)

# A refinement known as eigenvector centrality assigns a higher weight to a vertex for
# being connected to vertices which are themselves important. Formally, it assigns a
# weight to every vertex such that the weight of a particular vertex is proportional to
# the sum of the weights of its neighbors.

evcent(g)$vector

# The concept behind eigenvector centrality is that the most important vertices
# should be connected to many other well-connected vertices. A slightly different
# notion of importance comes from identifying those vertices which connect disjoint
# parts of the graph. The betweenness centrality measures this property of a graph by
# (approximately) determining the shortest path between every pair of nodes and calculating
# how many of these run through each vertex

betweenness(g)

##high-betweeness with low centrality -- "gatekeepers"


g <- g + vertex("Ieyasu")
g <- g + vertex("Hidetada")
g <- g + vertex("Yorinobu")
g <- g + vertex("Yorifusa")
g <- g + vertex("Yorimoto")
g <- g + vertex("Yorikatsu")
g <- g + vertex("Mitsusada")
g <- g + vertex("Yoshimune")
g <- g + vertex("Iemitsu")
g <- g + vertex("Saigō-no-Tsubone")

g <- g + vertex("Ricky")
g <- g + vertex("Tony")
g <- g + vertex("Shirley")
g <- g + vertex("Joey")


g <- g + edges("Ieyasu", "Hidetada")
g <- g + edges("Ieyasu", "Yorinobu")
g <- g + edges("Ieyasu", "Yorifusa")
g <- g + edges("Ieyasu", "Yorimoto")
g <- g + edges("Ieyasu", "Yorikatsu")
g <- g + edges("Hidetada", "Iemitsu")

g <- g + edges("Yorinobu", "Mitsusada")
g <- g + edges("Mitsusada", "Yoshimune")
g <- g + edges("Saigō-no-Tsubone", "Hidetada")


g <- g + edges("Ricky", "Yoshimune")
g <- g + edges("Ricky", "Joey")
g <- g + edges("Joey", "Shirley")
g <- g + edges("Joey", "Tony")

plot.igraph(g, layout=layout_nicely)


g <- g + edges("Ricky", "Catherine")


plot.igraph(g, layout=layout_nicely)
plot.igraph(g, layout=layout_as_tree)
plot.igraph(g, layout=layout_in_circle)
plot.igraph(g, layout=layout.auto)

betweenness(g)
evcent(g)$vector
degree(g)

##compare with directed edges

h <- graph.empty(directed=TRUE)

h <- h + vertex("Philip")
h <- h + vertex("Charles")
h <- h + vertex("Diana")
h <- h + vertex("William")
h <- h + vertex("Harry")
h <- h + vertex("Catherine")
h <- h + vertex("George")
h <- h + vertex("Elizabeth II")

##edges are connections between vertices

h <- h + edges("Elizabeth II", "Charles")
h <- h + edges("Philip", "Charles")
h <- h + edges("Charles", "William")
h <- h + edges("Diana", "William")
h <- h + edges("Charles", "Harry")
h <- h + edges("Diana", "Harry")
h <- h + edges("William", "George")
h <- h + edges("Catherine", "George")

h


plot.igraph(h, layout=layout_as_tree)
plot.igraph(h, layout=layout_in_circle)
plot.igraph(h, layout=layout.auto)
plot.igraph(h, layout=layout_nicely)


# The simplest centrality measure is degree centrality. Vertices are ranked only on
# the number of edges to which it is connected. In citation networks, degree centrality
# is analogous to just counting the number of citations a document has. We have
# already argued that this is a reasonable first solution, butmay miss the larger picture

degree(h)

# A refinement known as eigenvector centrality assigns a higher weight to a vertex for
# being connected to vertices which are themselves important. Formally, it assigns a
# weight to every vertex such that the weight of a particular vertex is proportional to
# the sum of the weights of its neighbors.

evcent(h)$vector

# The concept behind eigenvector centrality is that the most important vertices
# should be connected to many other well-connected vertices. A slightly different
# notion of importance comes from identifying those vertices which connect disjoint
# parts of the graph. The betweenness centrality measures this property of a graph by
# (approximately) determining the shortest path between every pair of nodes and calculating
# how many of these run through each vertex

betweenness(h)

##high-betweeness with low centrality -- "gatekeepers"


h <- h + vertex("Ieyasu")
h <- h + vertex("Hidetada")
h <- h + vertex("Yorinobu")
h <- h + vertex("Yorifusa")
h <- h + vertex("Yorimoto")
h <- h + vertex("Yorikatsu")
h <- h + vertex("Mitsusada")
h <- h + vertex("Yoshimune")
h <- h + vertex("Iemitsu")
h <- h + vertex("Saigō-no-Tsubone")

h <- h + vertex("Ricky")
h <- h + vertex("Tony")
h <- h + vertex("Shirley")
h <- h + vertex("Joey")


h <- h + edges("Ieyasu", "Hidetada")
h <- h + edges("Ieyasu", "Yorinobu")
h <- h + edges("Ieyasu", "Yorifusa")
h <- h + edges("Ieyasu", "Yorimoto")
h <- h + edges("Ieyasu", "Yorikatsu")
h <- h + edges("Hidetada", "Iemitsu")

h <- h + edges("Yorinobu", "Mitsusada")
h <- h + edges("Mitsusada", "Yoshimune")
h <- h + edges("Saigō-no-Tsubone", "Hidetada")


h <- h + edges("Ricky", "Yoshimune")
h <- h + edges("Ricky", "Joey")
h <- h + edges("Joey", "Shirley")
h <- h + edges("Joey", "Tony")

plot.igraph(h, layout=layout_nicely)


h <- h + edges("Ricky", "Catherine")


plot.igraph(h, layout=layout_nicely)
plot.igraph(h, layout=layout_as_tree)
plot.igraph(h, layout=layout_in_circle)
plot.igraph(h, layout=layout.auto)

betweenness(h)
betweenness(h, directed=FALSE, normalized = TRUE)
betweenness(g)
betweenness(g, directed=FALSE, normalized = TRUE)

evcent(h)$vector
evcent(g)$vector
degree(h)
degree(g)

