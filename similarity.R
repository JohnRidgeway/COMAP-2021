setwd("C:/Users/judef/OneDrive/Documents/Github/comap-2021")
library(data.table)
library(plyr)
library(dplyr)
library(network)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(babynames)
library(igraph)
library(nortest)
library(moments)


artist <- fread("data_by_artist2.csv", select=c(1:16), header = FALSE)
#artistnames <- artist[-1,1]
#artistnames <- artistnames$V1
#artistnames <- as.character(artistnames)
metrics <- artist[1,-c(2,6:9,14:16)]
metrics <- as.character(metrics)
#remove metric names1 / names1, IDs2, tempo6, loudness7, mode8, key9, duration14, popularity15, counts16
artist <- artist[-1,-c(2,6:9,14:16)]
colnames(artist) <- metrics
tempnames <- artist[,1]
artist <- artist[,-1]
artist <- mutate_all(artist, function(x) as.numeric(as.character(x)))
artist <- cbind(tempnames, artist)
#rownames(artist) <- artistnames

influence_data <- read.csv("influence_data2.csv",header=TRUE)
edge <- influence_data[, c('influencer_name','follower_name')]
adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))


#sum(abs(artist[1,-1] - artist[2,-1]))


similarity <- data.frame(matrix(NA, nrow = nrow(artist), ncol = ncol(artist)))

#for (i in 1:nrow(artist)) {
  #for (j in i:nrow(artist)) {
    #similarity[i,j] <- sum(abs(artist[i,-1] - artist[j,-1]))
  #}
#}


head(adjacency)
rownames(adjacency)

artist["Cheems",]


#artistrows <- data.frame(matrix(NA, nrow = 1, ncol = ncol(artist)))


artistrows <- subset(artist, artist$artist_name == "112")
for (i in 2:length(rownames(adjacency))) {
  artistrowi <- subset(artist, artist$artist_name == rownames(adjacency)[i])
  artistrows <- rbind(artistrows,artistrowi)
}

write.csv(artistrows, "C:/Users/judef/OneDrive/Documents/School/Spring 2021/MCM/artistrows.csv", row.names = FALSE)







tempos <- artist[-1,6]
tempos <- unlist(tempos)
tempos <- as.numeric(tempos)
tempos <- as.data.frame(tempos)
hist(tempos)

tempos <- tempos[tempos < 154.36591665]
tempos <- tempos[90.50812857 < tempos]


skewness(tempos)
kurtosis(tempos)

















full <- fread("full_music_data.csv",select=c(1:19),header = TRUE)
influence_data <- fread("influence_data.csv", select=c(1:8), header = TRUE)

edge <- influence_data[, c('influencer_name','follower_name')]
adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))
test <- apply(adjacency[1:2,2:ncol(adjacency)], 1, function(x)which(x>0))
testv <- as.data.frame(unlist(test))

adjacency[41,]
InfluenceA <- adjacency[1,]
InfluenceA.df <- as.data.frame(InfluenceA)
AFollowers <- rownames(filter(InfluenceA.df, InfluenceA > 0))

#adjacency[row for each of names in AFollowers]
which(rownames(adjacency) %in% "AFollowers") 


##all pairwise

