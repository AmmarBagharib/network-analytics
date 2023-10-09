library(igraph)
library(here)
library(data.table)
library(ggplot2)
library(hrbrthemes)


# Qn 1b
# Step 1: Read the edge list from the .txt file
file_path_2000 <- here("indiv-project/trade-network/2000.net")
g2000 <- read_graph(file_path_2000,format="pajek")

file_path_2005 <- here("indiv-project/trade-network/2005.net")
g2005 <- read_graph(file_path_2005,format="pajek")

file_path_2010 <- here("indiv-project/trade-network/2010.net")
g2010 <- read_graph(file_path_2010,format="pajek")

file_path_2015 <- here("indiv-project/trade-network/2015.net")
g2015 <- read_graph(file_path_2015,format="pajek")

file_path_2018 <- here("indiv-project/trade-network/2018.net")
g2018 <- read_graph(file_path_2018,format="pajek")

#sum weights in g2018 to remove multi-edges
g2018 <- simplify(g2018, edge.attr.comb = "sum") #sums weights of repeated edges


graph_att <- function(graph, year){
  #this function returns a graph attributes in a data.table object
  df <- data.table::data.table(
    number_of_nodes=igraph::vcount(graph),
    number_of_edges=igraph::ecount(graph),
    density=igraph::graph.density(graph),
    diameter=igraph::diameter(graph),
    largest_component_size=max(igraph::components(graph)$csize),
    global_clustering_coeff=igraph::transitivity(graph,type="global"),
    ave_clustering_coeff=igraph::transitivity(graph,type="average"),
    number_of_cliques=igraph::clique_num(graph),
    year=year
  )
  return(df)
}


vertex_att <- function(graph, year){
  #this function returns some vertex attributes of a graph in a data.table object
  df <- data.table::data.table(
    vertex_id=igraph::V(graph),
    vertex_name=igraph::V(graph)$name,
    degree=igraph::degree(graph),
    closeness=igraph::closeness(graph),
    betweenness=igraph::betweenness(graph,normalized=T),
    transitivity=igraph::transitivity(graph,type="local"),
    eigen_centrality=igraph::eigen_centrality(graph)$vector,
    year=year
  )
  return(df)
}

get_top_5 <- function(df, col_of_interest){
  x <- order(data, na.last = TRUE, decreasing = FALSE)
  return(x[1:5, c("vertex_name", col_of_interest)])
}
getwd()

## Vertex Level Attributes

v_2000 <- vertex_att(g2000, "2000")
v_2005 <- vertex_att(g2005, "2005")
v_2010 <- vertex_att(g2010, "2010")
v_2015 <- vertex_att(g2000, "2015")
v_2018 <- vertex_att(g2000, "2018")






