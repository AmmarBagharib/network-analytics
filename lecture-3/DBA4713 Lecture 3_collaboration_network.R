## DBA4713 Lecture 3
## Demo: Collaboration Network

## 1) why to use igraph? ####
library(data.table)
data = fread("collaboration.edgelist.txt",colClasses = "character") # 93439 x 2 : V1, V2

# exploring the input data
nrow(unique(data))
head(data)
data[V1=="1680"]
data[V2=="1680"]
data[V1=="4131"]
data[V2=="4131"]

# exploring degree using basic data manipulation
length(unique(c(data$V1,data$V2))) # 23133 values, from 0 to 23132
data[,.(cnt=.N),by=.(V1)][order(-cnt)][1:10]
rbind(data[,.(cnt=.N),by="V1"],data[,.(cnt=.N),by="V2"],use.names=FALSE)[,.(degree=sum(cnt)),by="V1"]

# converting edge.list to matrix
library(reshape2)
mat=dcast(data[1:20,],V1~V2,fun.aggregate=length,value.var="V2")
rownames(mat)=mat$V1
mat=mat[,-1]
rowSums(mat)

# converting edge.list to adjacency matrix
library(igraph)
g=graph.edgelist(as.matrix(data),directed=F)
adj_matrix=get.adjacency(g,type="both",sparse=TRUE)

# exploring different metrics
degree(g)
closeness(g)
betweenness(g)


## 2) how to play with the collaboration network? ####
library(data.table)
library(igraph)
data = fread("collaboration.edgelist.txt",colClasses = "character") # 93439 x 2 : V1, V2
g=graph.edgelist(as.matrix(data))

# graph-level attributes
summary(g)
vcount(g)
ecount(g)
graph.density(g)
hist(degree(g),main="Degree Distribution",xlab="Degree",ylab="Number of Vertices")
hist(degree(g),breaks=seq(0,max(degree(g)),by=1),xlim=c(0, 50),
     main="Degree Distribution",xlab="Degree",ylab="Number of Vertices")
shortest_paths(g,from="0", to="15750")$vpath[[1]]
diameter(g)
components(g)
max(components(g)$csize)
largest_component(g)
transitivity(g,type="global")
transitivity(g,type="average") 

# vertex-level attributes
transitivity(g,type="local")
mean(transitivity(g,type="local"),na.rm=T)
degree(g)
closeness(g)
betweenness(g,normalized=T)
eigen_centrality(g)$vector

# edge-level attributes
edge.betweenness(g)
#E(g)$weight

# store attributes into a data.frame
df=data.frame(vertex_id=V(g),
              vertex_name=V(g)$name,
              degree=degree(g),
              closeness=closeness(g),
              betweenness=betweenness(g,normalized=T),
              transitivity=transitivity(g,type="local"))
summary(df[-c(1,2)])
cor(df[-c(1,2)],use="complete.obs")

dt=data.table(df[-c(1)])
cor(dt[vertex_name%in%V(largest_component(g))$name,-c(1)],use="complete.obs")

# a simple regression by randomly creating additional variables
set.seed(4713)
dt[,age:=sample(20:40,nrow(dt),replace=T)]
dt[,tenure:=runif(nrow(dt),1,6)]
dt[,rating:=sample(1:5,nrow(dt),replace=T)]
summary(dt)

summary(lm(rating~
             age+tenure+degree+closeness+betweenness+transitivity,
           data=dt))


# visualization
#plot(g)
sub_g=induced_subgraph(g, vids=V(g)[1:10])
plot(sub_g)
plot(sub_g,
     vertex.size=50,vertex.color="green",vertex.label.color="black", 
     edge.color="grey",edge.width=2)
plot(sub_g,
     layout=layout.fruchterman.reingold)
plot(sub_g,
     layout=layout.circle)
