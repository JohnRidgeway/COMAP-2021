#Code section from Julia major_influencers.R
library(dplyr)
library(tidyr)
library(readxl)  
library(ggplot2)
library(ggpubr)
library(convertr)

gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)


#Read in data frames
data_by_artist <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/data_by_artist2.csv")
full_music_data <- read.csv("/Users/johnridgeway/Desktop/2021_ICM_Problem_D_Data/full_music_data.csv")
data_by_year <- read.csv("/Users/johnridgeway/Desktop/2021_ICM_Problem_D_Data/data_by_year.csv")
influence_data <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/influence_data2.csv")


edge <- influence_data[, c('influencer_name','follower_name')]

#get original adjacency matrix
adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))
network <- graph_from_adjacency_matrix(adjacency)
#plot(network)

#calculate how many people influenced--------------------------
#when given the matrix, we just need to sum each row
#then we might want to give this to the data frame
row <- adjacency[1,]
names <- rownames(as.data.frame(row))
sum <- rep(0,nrow(data_by_artist))
data_by_artist$total_influenced <- sum
for (i in 1:ncol(adjacency)) {
  sum[i] <- sum(adjacency[i,])
  index <- which(data_by_artist$artist_name == names[i])
  data_by_artist[index,"total_influenced"] <- sum[i]
}

influence_data$artist_id <- influence_data$influencer_id

data_by_artist_genre <- inner_join(data_by_artist, influence_data, by = "artist_id" )
data_by_artist_genre <- data_by_artist_genre %>% distinct(artist_name, .keep_all = TRUE)

data_by_artist_genre <-  data_by_artist_genre %>% select("artist_name","count","total_influenced", "influencer_main_genre")


#Get correlation and make scatterplot of number of songs released by influencer and number of people influenced in Pop/Rock
data_by_artist_pop_rock <- data_by_artist_genre %>% filter(influencer_main_genre == "Pop/Rock")
cor(data_by_artist_pop_rock$count, data_by_artist_pop_rock$total_influenced,  method = c("spearman"))

pr <- ggplot(data_by_artist_pop_rock, aes(x=count, y=total_influenced)) +
  geom_point(size=1, shape=23)  + xlab("Number of Songs") + ylab("Number of Artists Influenced") 



#Get correlation and make scatterplot of number of songs released by influencer and number of people influenced
cor(data_by_artist_genre$count, data_by_artist_genre$total_influenced, method = c("spearman"))
allgenre <- ggplot(data_by_artist_genre, aes(x=count, y=total_influenced)) +
  geom_point(size=1, shape=23)    + xlab("Number of Songs") + ylab("Number of Artists Influenced") 


ggarrange(pr,allgenre,labels = c("Pop Rock", "All Genres"))
  
  