setwd("C:/Users/judef/OneDrive/Documents/School/Spring 2021/MCM")
library(data.table)
library(dplyr)
library(network)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(babynames)
library(igraph)

full <- fread("full_music_data.csv",select=c(1:19),header = TRUE)
influence_data <- fread("influence_data.csv", select=c(1:8), header = TRUE)

edge <- influence_data[, c('influencer_name','follower_name')]
adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))
test <- apply(adjacency[1:2,2:ncol(adjacency)], 1, function(x)which(x>0))
testv <- as.data.frame(unlist(test))



artist <- fread("data_by_artist.csv", select=c(1:16), header = FALSE)
artistnames <- artist[,1]
names <- rbind("Metric", artistnames, fill= TRUE)
artist <- artist[,-1]
row.names(artist) <- names


adjacency[41,]
InfluenceA <- adjacency[1,]
InfluenceA.df <- as.data.frame(InfluenceA)
AFollowers <- rownames(filter(InfluenceA.df, InfluenceA > 0))

#adjacency[row for each of names in AFollowers]
which(rownames(adjacency) %in% "AFollowers") 




