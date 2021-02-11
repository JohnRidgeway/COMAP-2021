#want to look at popularity
#how many people they have influenced
#areas of major change across years
#difference of artists from year and previous years
library(igraph)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

#import data
influence_data <- read.csv("/Users/Franck/R/comap_2021/influence_data2.csv",header=TRUE)
data_by_artist <- read.csv("/Users/Franck/R/comap_2021/data_by_artist2.csv",header=TRUE)
data_by_year <- read.csv("/Users/Franck/R/comap_2021/data_by_year.csv",header=TRUE)
full_music_data <- read.csv("/Users/Franck/R/comap_2021/full_music_data.csv",header=TRUE)

edge <- influence_data[, c('influencer_name','follower_name')]

#get original adjacency matrix
adjacency <- get.adjacency(graph.edgelist(as.matrix(edge), directed=TRUE))
network <- graph_from_adjacency_matrix(adjacency)
#plot(network)

#calculate how many people influenced----------------------------------------------------------
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

#major_influencers <- data_by_artist$total_influenced > 200
#major_infl <- subset(data_by_artist, major_influencers)

data_by_artist <- subset(data_by_artist,select=c(artist_id,danceability, energy, valence, acousticness, 
                                                 instrumentalness, liveness, speechiness,total_influenced))

#Copy follower id column and rename to artists id so that we can join dataframes
influence_data$artist_id <- influence_data$follower_id

#
infl_start_date <- subset(influence_data, select=c(artist_id,influencer_active_start,influencer_main_genre,follower_active_start))
full_data_by_artist <- inner_join(data_by_artist, infl_start_date, by = "artist_id")

rp.artists <- full_data_by_artist %>%
  filter(influencer_main_genre == "Pop/Rock")
follower.dates <- aggregate(x = rp.artists, by = rp.artists[c("artist_id")], FUN = mean)
follower.dates <- subset(follower.dates,select=c(follower_active_start,artist_id))
rp.artists <- inner_join(rp.artists, follower.dates, by = "artist_id")
#remove duplicates
rp.artists <- rp.artists %>% distinct(artist_id, .keep_all = TRUE)
ninties.pop <- rp.artists %>% filter(influencer_active_start == 1990)
ninties.pop <- ninties.pop[-13]

top.ninties.pop <- ninties.pop[order(-ninties.pop$total_influenced),] %>%
  filter(ninties.pop$total_influenced > 15)
next.ninties.pop <- ninties.pop %>% filter(ninties.pop$total_influenced >10)
less.ninties.pop <- ninties.pop %>% filter(ninties.pop$total_influenced >7)
less2.ninties.pop <- ninties.pop %>% filter(ninties.pop$total_influenced >5)
less3.ninties.pop <- ninties.pop %>% filter(ninties.pop$total_influenced >2)
less4.ninties.pop <- ninties.pop %>% filter(ninties.pop$total_influenced >1)
nin.pop.mean <- rep(0,7)
nin.pop.mean[1] <- mean(top.ninties.pop$energy)
nin.pop.mean[2] <- mean(next.ninties.pop$energy)
nin.pop.mean[3] <- mean(less.ninties.pop$energy)
nin.pop.mean[4] <- mean(less2.ninties.pop$energy)
nin.pop.mean[5] <- mean(less3.ninties.pop$energy)
nin.pop.mean[6] <- mean(less4.ninties.pop$energy)
nin.pop.mean[7] <- mean(ninties.pop$energy)
no.influenced <- c(15,10,7,5,2,1,0)

nin.pop.mean.follow.date <- rep(0,7)
nin.pop.mean.follow.date[1] <- mean(top.ninties.pop$follower_active_start)
nin.pop.mean.follow.date[2] <- mean(next.ninties.pop$follower_active_start)
nin.pop.mean.follow.date[3] <- mean(less.ninties.pop$follower_active_start)
nin.pop.mean.follow.date[4] <- mean(less2.ninties.pop$follower_active_start)
nin.pop.mean.follow.date[5] <- mean(less3.ninties.pop$follower_active_start)
nin.pop.mean.follow.date[6] <- mean(less4.ninties.pop$follower_active_start)
nin.pop.mean.follow.date[7] <- mean(ninties.pop$follower_active_start)

nin.pop.data <- rbind(no.influenced,nin.pop.mean)
nin.pop.data <- t(nin.pop.data)
nin.pop.data <- as.data.frame(nin.pop.data)

nin.pop.follower.data <-rbind(no.influenced,nin.pop.mean.follow.date)
nin.pop.follower.data<- t(nin.pop.follower.data)
nin.pop.follower.data<- as.data.frame(nin.pop.follower.data)

p <- ggplot() +
  geom_line(data=nin.pop.data, aes(x = no.influenced, y = nin.pop.mean) ,size=1.5, color = "#D55E00") +
  geom_point(data=nin.pop.data, aes(x = no.influenced, y = nin.pop.mean) ,size=2.5, color = "#D55E00") +
  theme_bw() + labs(x="Minimum Number of Followers", y="Energy", title="Pop/Rock Artist Average Energy by\nMinimum # of Followers in 1990s")
ggarrange(p)

p<-ggplot() +
  geom_line(data=nin.pop.follower.data, aes(x = no.influenced, y = nin.pop.mean.follow.date) ,size=1.5, color = "#F0E442") +
  geom_point(data=nin.pop.follower.data, aes(x = no.influenced, y = nin.pop.mean.follow.date) ,size=2.5, color = "#F0E442") +
  theme_bw() + labs(x="Minimum Number of Followers", y="Average Follower Active Start Date", title="Pop/Rock Artist Average Follower\nActive Start date by Minimum \n# of Followers in 1990s")
ggarrange(p)
#------------------------------------------------------------------------------------------------------------
#data by year graphs

#
data_by_artist <- subset(data_by_artist,select=c(artist_id,danceability, energy, valence, acousticness, 
                                               instrumentalness, liveness, speechiness,total_influenced))

#Copy follower id column and rename to artists id so that we can join dataframes
influence_data$artist_id <- influence_data$follower_id

#
infl_start_date <- subset(influence_data, select=c(artist_id,influencer_active_start))
full_data_by_artist <- inner_join(data_by_artist, infl_start_date, by = "artist_id")

#
means <- aggregate( x = full_data_by_artist,
                            by = full_data_by_artist[c("influencer_active_start")],
                            FUN = mean)
#get rid of duplicates
means <- means[-1]

gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)

p <- ggplot() + 
  geom_line(data=means, aes(x = influencer_active_start, y = means[[2]], color = "Danceability"),size=1.5) +
  geom_line(data=means, aes(x = influencer_active_start, y = means[[6]], color = "Instrumentalness"),size=1.5) + 
  geom_line(data=means, aes(x = influencer_active_start, y = means[[3]], color = "Energy"),size=1.5) + 
  geom_line(data=means, aes(x = influencer_active_start, y = means[[8]], color = "Speechiness"),size=1.5) + 
  geom_line(data=means, aes(x = influencer_active_start, y = means[[4]], color = "Valence"),size=1.5) + 
  geom_line(data=means, aes(x = influencer_active_start, y = means[[7]], color = "Liveness"),size=1.5) + 
  geom_line(data=means, aes(x = influencer_active_start, y = means[[5]], color = "Acousticness"),size=1.5) + 
  xlim(1930, 2010) + ylim(0,1) + labs(x="Year", y="Value", title="Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)

ggarrange(p)

#remove unneeded variables
data_by_year <- subset(data_by_year,select=c(year, danceability, energy, valence, acousticness, 
                                             instrumentalness, liveness, speechiness))
#gray scale hex values
gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)

p <- ggplot() + 
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[2]], color = "Danceability"),size=1.5) +
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[6]], color = "Instrumentalness"),size=1.5) + 
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[3]], color = "Energy"),size=1.5) + 
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[8]], color = "Speechiness"),size=1.5) + 
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[4]], color = "Valence"),size=1.5) + 
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[7]], color = "Liveness"),size=1.5) + 
  geom_line(data=data_by_year, aes(x = year, y = data_by_year[[5]], color = "Acousticness"),size=1.5) + 
  xlim(1920, 2021) + ylim(0,1) + labs(x="Year", y="Value", title="Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)

ggarrange(p)

#lets do a weighted mean
#we need to changed influencer data, and need to subset with the 7 attributes above