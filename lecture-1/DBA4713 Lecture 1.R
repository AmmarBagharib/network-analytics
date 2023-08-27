#install.packages("statnet")
{
library(statnet)

netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))

rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency")
class(net1)
summary(net1)

netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2 <- network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")
summary(net2)

gplot(net1, vertex.col = 2, displaylabels = TRUE)

set.vertex.attribute(net1, "gender", c("F", "F", "M",
                                       "F", "M"))
net1 %v% "alldeg" <- degree(net1)

list.vertex.attributes(net1)

get.vertex.attribute(net1, "gender")
get.vertex.attribute(net1, "alldeg")
get.vertex.attribute(net1, "vertex.names")

netval1 <- rbind(c(0,2,3,0,0),
                 c(0,0,3,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,2,0,0))

netval1 <- network(netval1,matrix.type="adjacency",
                   ignore.eval=FALSE,names.eval="like")

#The key here are the ignore.eval and names.eval options. These two
#options tell the network function to evaluate the actual values in the sociomatrix,
#and store those values in a new edge attribute called 'like.' Once values
#are stored in an edge attribute, the original valued matrix can be restored using 
#as.sociomatrix coercion function.

network.vertex.names(netval1) <- c("A","B","C","D","E")
get.edge.attribute(netval1, "like")
as.sociomatrix(netval1)
as.sociomatrix(netval1,"like")
network.vertex.names(netval1)

detach(package:statnet)
#install.packages("igraph")
}


library(igraph)

netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")

inet1 <- graph.adjacency(netmat1)
summary(inet1)
str(inet1)


netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
inet2 <- graph.edgelist(netmat2)

V(inet2)$name <- c("A","B","C","D","E")
E(inet2)$val <- c(1:6)


netval2<- rbind(c("A","B",2),
                 c("A","C",1),
                 c("B","C",3),
                 c("B","D",3),
                 c("C","B",2),
                 c("E","C",1))
colnames(netval2) <- c("from","to","like")
inet3 <- graph.edgelist(netval2[,1:2])
E(inet3)$weight <- netval2[,3]

#str(inet3)
plot.igraph(inet3,vertex.label=V(inet3)$name,
            edge.width=E(inet3)$weight,edge.color="black",
            layout=layout.fruchterman.reingold)

plot.igraph(inet2,vertex.label=V(inet2)$name,
            edge.width=E(inet2)$weight,edge.color="blue",
            layout=layout.fruchterman.reingold)

#Filtering
detach(package:igraph)
library(statnet)
netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency")
set.vertex.attribute(net1, "gender", c("F", "F", "M",
                                       "F", "M"))
n1F <- get.inducedSubgraph(net1,
                           which(net1 %v% "gender" == "F"))
gplot(n1F,displaylabels=TRUE)


detach(package:statnet)
library(igraph)
netval2<- as.data.frame(rbind(c("A","B",2),
                              c("A","C",1),
                              c("B","C",3),
                              c("B","D",3),
                              c("C","B",2),
                              c("E","C",1)))
from=c("A","A","B","B","C","E")
to=c("B","C","C","D","B","C")
like=c(2,1,3,3,2,1)
netval2=data.frame(from,to,like,stringsAsFactors = FALSE)

inet3 <- graph.edgelist(as.matrix(netval2[,1:2]))
E(inet3)$weight <- netval2$like

n2F<-subgraph.edges(inet3,which(E(inet3)$weight>1),delete.vertices = TRUE)  
plot.igraph(n2F,vertex.label=V(n2F)$name,
            edge.width=E(n2F)$weight,edge.color="black",
            layout=layout.fruchterman.reingold)



