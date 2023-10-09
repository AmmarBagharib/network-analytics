library(igraph)
library(here)
library(data.table)
library(ggplot2)
library(hrbrthemes)

file_path_2018 <- here("indiv-project/trade-network/2018.net")
g2018 <- read_graph(file_path_2018,format="pajek")

#sum weights in g2018 to remove multi-edges
g2018 <- simplify(g2018, edge.attr.comb = "sum") #sums weights of repeated edges

# Function to compute diameter after node removal
graph_deletion <- function(graph, removal_fraction, approach) {
  
  #this function takes in a graph object, the percentage of nodes to be removed, and the approach to deletion, namely "random" or "targeted"
  #targeted approach is using centrality 
  
  num_nodes_to_remove <- floor(vcount(graph) * removal_fraction) #round down number of ndoes to be removed
  
  # Create a copy of the original graph
  g_copy <- copy(graph)
  
  # Sort nodes by centrality measure if using targeted deletion
  if (approach == "targeted") {
    close_centr <- igraph::closeness(graph)
    nodes_to_remove <- order(close_centr, decreasing = TRUE)[1:num_nodes_to_remove]
  } else {
    # Randomly select nodes for removal
    nodes_to_remove <- sample(1:vcount(graph), num_nodes_to_remove)
  }
  
  # Remove selected nodes
  g_copy <- delete_vertices(g_copy, nodes_to_remove)
  
  return(g_copy)
}

set.seed(32)
# Define a range of fractions for node removal
removal_fractions <- seq(0, 0.9, by = 0.1)

# Initialize vectors to store results
diameter_random <- numeric(length(removal_fractions))
diameter_targeted <- numeric(length(removal_fractions))

# Perform node removal for each fraction
for (i in 1:length(removal_fractions)) {
  fraction <- removal_fractions[i]
  
  # Random node deletion approach
  diameter_random[i] <- diameter(compute_diameter_after_removal(g2018, fraction, "random"))
  
  # Targeted node deletion approach
  diameter_targeted[i] <- diameter(compute_diameter_after_removal(g2018, fraction, "targeted"))
}

# Calculate the minimum and maximum values of diameter_random and diameter_targeted
min_diameter <- min(min(diameter_random), min(diameter_targeted))
max_diameter <- max(max(diameter_random), max(diameter_targeted))

plot(removal_fractions, diameter_random, type = "l", col = "blue", xlab = "Fraction of Nodes Removed", 
     ylab = "Diameter", main = "Change in Diameter vs. Node Removal", ylim = c(min_diameter, max_diameter))
lines(removal_fractions, diameter_targeted, type = "l", col = "red")
legend("topright", legend = c("Random Deletion", "Targeted Deletion"), col = c("blue", "red"), lty = 1)
```