library(igraph)
library(tidyverse)
library(dplyr)
library(rlang)


setwd("C:/Users/judef/OneDrive/Documents/School/Spring 2021/MCM")
influence_data <- read.csv("influence_data.csv",header=TRUE)
data_by_artist <- read.csv("data_by_artist.csv",header=TRUE)

edge <- influence_data[, c('influencer_name','follower_name')]

adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))
network <- graph_from_adjacency_matrix(adjacency)
#plot(network)

index <- which(colnames(adjacency) == "The Killers")
row <- adjacency[index,]
names <- as.data.frame(row)
rownames <- rownames(filter(names,row > 0))
queue <- rownames
rownames <- append(rownames,"The Killers",0)

#while condition
while(!is_empty(queue)){
  for (i in 1:queue.length) {
    index <- which(colnames(adjacency) == queue[1])
    row <- adjacency[index,]
    names <- as.data.frame(row)
    followers <- rownames(filter(names,row > 0))
    queue = queue[-1]
    if (!is_empty(followers)) {
      rownames = append(rownames,followers,length(rownames))
      queue = append(queue,followers,length(queue))
    }
  }
  queue.length = length(queue)
}
#selectedData <- data_by_artist[grep("Bill Evans", data_by_artist$artist_name), ]

subnet.influence.data <- subset(influence_data, influence_data$influencer_name == "The Killers")
for (i in 2:length(rownames)) {
  subnet.influence.data2 <- subset(influence_data, influence_data$influencer_name == rownames[i])
  subnet.influence.data <- rbind(subnet.influence.data,subnet.influence.data2)
}

subnet.edge <- subnet.influence.data[, c('influencer_name','follower_name')]

subnet.adjacency <- get.adjacency(graph.edgelist(as.matrix(subnet.edge), directed=TRUE))

subnetwork <- graph_from_adjacency_matrix(subnet.adjacency)
plot(subnetwork)





data_by_artist <- data_by_artist[-1,-c(2,6:9,14:16)]
subnet.artist.data <- subset(data_by_artist, data_by_artist$artist_name == "The Killers")
for (i in 2:length(rownames)) {
  subnet.artist.data2 <- subset(data_by_artist, data_by_artist$artist_name == rownames[i])
  subnet.artist.data <- rbind(subnet.artist.data,subnet.artist.data2)
}

similarity <- data.frame(matrix(NA, nrow = nrow(subnet.adjacency), ncol = ncol(subnet.adjacency)))
rownames(similarity) <- rownames(subnet.adjacency)
colnames(similarity) <- colnames(subnet.adjacency)

similaritynon <- data.frame(matrix(NA, nrow = nrow(subnet.adjacency), ncol = ncol(subnet.adjacency)))
rownames(similaritynon) <- rownames(subnet.adjacency)
colnames(similaritynon) <- colnames(subnet.adjacency)

for (i in 1:length(rownames(subnet.adjacency))) {
  for (j in i:length(colnames(subnet.adjacency))) {
    if(subnet.adjacency[i,j] == 1){similarity[i,j] <- sum(abs(subnet.artist.data[i,2:8]-subnet.artist.data[j,2:8]))}
  }
}

for (i in 1:length(rownames(subnet.adjacency))) {
  for (j in i:length(colnames(subnet.adjacency))) {
    if(subnet.adjacency[i,j] == 0){similaritynon[i,j] <- sum(abs(subnet.artist.data[i,2:8]-subnet.artist.data[j,2:8]))}
    if(i == j){similaritynon[i,j] <- NA}
  }
}
