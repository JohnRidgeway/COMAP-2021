library(igraph)
library(tidyverse)
library(dplyr)

influence_data <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/influence_data2.csv",header=TRUE)
data_by_artist <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/data_by_artist2.csv",header=TRUE)

edge <- influence_data[, c('influencer_name','follower_name')]

#get original adjacency matrix
adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))
network <- graph_from_adjacency_matrix(adjacency)
#plot(network)

#breadth first search
#-----------------------------------------------------

#find row index for the given name & retrieve row from matrix
index <- which(colnames(adjacency) == "The Killers")
row <- adjacency[index,]
names <- as.data.frame(row)
#get the names for the artists with nonzero entries in their columns
rownames <- rownames(filter(names,row > 0))
#make queue equal to rownames
queue <- rownames
#add starting artist to rownames
rownames <- append(rownames,"The Killers",0)

#while condition
queue.length = length(queue)
while(!is_empty(queue)){
  for (i in 1:queue.length) {
    #same process as above
    index <- which(colnames(adjacency) == queue[1])
    row <- adjacency[index,]
    names <- as.data.frame(row)
    followers <- rownames(filter(names,row > 0))
    #subtract current artist from queue
    queue = queue[-1]
    #if artist has followers, append them to row names and to the queue
    if (!is_empty(followers)) {
      rownames = append(rownames,followers,length(rownames))
      queue = append(queue,followers,length(queue))
    }
  }
  queue.length = length(queue)
}

#-----------------------------------------------------

#plotting the subnetwork

#create data frame with starting artist data
subnet.influence.data <- subset(influence_data, influence_data$influencer_name == "The Killers")
for (i in 2:length(rownames)) {
  #append each consecutive artist data in rownames list
  subnet.influence.data2 <- subset(influence_data, influence_data$influencer_name == rownames[i])
  subnet.influence.data <- rbind(subnet.influence.data,subnet.influence.data2)
}

#create list of edges
subnet.edge <- subnet.influence.data[, c('influencer_name','follower_name')]

#make new adjacency matrix and plot the resultant network
subnet.adjacency <- get.adjacency(graph.edgelist(as.matrix(subnet.edge), directed=TRUE))
subnetwork <- graph_from_adjacency_matrix(subnet.adjacency)
plot(subnetwork)

#retrieve specific data
#selectedData <- data_by_artist[grep("Bill Evans", data_by_artist$artist_name), ]

