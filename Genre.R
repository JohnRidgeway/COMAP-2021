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

#Create vector of unique genres of influencers
unique_genre_dataframe <- influence_data %>% distinct(influencer_main_genre, .keep_all = TRUE)


#Count unique instances of influencers in each genre and store plots in list
array_influencer <- list()
plots_influencer <- list()
for(i in 1:20){
  array_influencer[[i]] <- influence_data %>%
    filter(influencer_main_genre == unique_genre_dataframe[i,"influencer_main_genre"]) %>%
    distinct(influencer_name, .keep_all = TRUE) %>%
    group_by(influencer_active_start) %>%
    dplyr::summarize(Total = n()) 
  
  
  
  plots_influencer[[i]] <- ggplot(data = array_influencer[[i]], aes(x = influencer_active_start, y = Total)) + 
    geom_bar(stat = "identity", fill = "steelblue") +
    xlab("Influencer Start Year") + ylab(" Number of Influencers") 
}




p <- ggplot() + geom_line(data=array_influencer[[1]], aes(x = influencer_active_start, y = Total, color = "gold1"),size=1) + 
  geom_line(data=array_influencer[[2]], aes(x = influencer_active_start, y = Total, color = "blueviolet"),size=1) + 
  geom_line(data=array_influencer[[3]], aes(x = influencer_active_start, y = Total, color = "darkgreen"),size=1) + 
  geom_line(data=array_influencer[[4]], aes(x = influencer_active_start, y = Total, color = "firebrick4"),size=1) + 
  geom_line(data=array_influencer[[5]], aes(x = influencer_active_start, y = Total, color = "slateblue"),size=1) + 
  xlim(1930, 2010) + ylim(0,500) + 
  scale_color_manual(values = gs.vals)

m <- ggplot() + geom_line(data=array_influencer[[6]], aes(x = influencer_active_start, y = Total, color = "gold1"),size=1) + 
  geom_line(data=array_influencer[[7]], aes(x = influencer_active_start, y = Total, color = "blueviolet"),size=1) + 
  geom_line(data=array_influencer[[8]], aes(x = influencer_active_start, y = Total, color = "darkgreen"),size=1) + 
  geom_line(data=array_influencer[[9]], aes(x = influencer_active_start, y = Total, color = "firebrick4"),size=1) + 
  geom_line(data=array_influencer[[10]], aes(x = influencer_active_start, y = Total, color = "slateblue"),size=1) + 
  xlim(1930, 2010) + ylim(0,500) + 
  scale_color_manual(values = gs.vals)
  
q <- ggplot() + geom_line(data= array_influencer[[11]], aes(x = influencer_active_start, y = Total, color= "red"),size=1) + 
  geom_line(data=array_influencer[[12]], aes(x = influencer_active_start, y = Total, color = "orange"),size=1) + 
  geom_line(data=array_influencer[[13]], aes(x = influencer_active_start, y = Total, color = "yellow"),size=1) +
  geom_line(data=array_influencer[[14]], aes(x = influencer_active_start, y = Total, color = "green"),size=1) + 
  geom_line(data=array_influencer[[15]], aes(x = influencer_active_start, y = Total, color = "blue"),size=1) + 
  xlim(1930, 2010) + ylim(0,500) + 
  scale_color_manual(values = gs.vals)

n <- ggplot() + geom_line(data=array_influencer[[16]], aes(x = influencer_active_start, y = Total, color = "gold1"),size=1) + 
  geom_line(data=array_influencer[[17]], aes(x = influencer_active_start, y = Total, color = "blueviolet"),size=1) + 
  geom_line(data=array_influencer[[18]], aes(x = influencer_active_start, y = Total, color = "darkgreen"),size=1) + 
  geom_line(data=array_influencer[[19]], aes(x = influencer_active_start, y = Total, color = "firebrick4"),size=1) + 
  geom_line(data=array_influencer[[20]], aes(x = influencer_active_start, y = Total, color = "slateblue"),size=1) + 
  xlim(1930, 2010) + ylim(0,500) + 
  scale_color_manual(values = gs.vals)

ggarrange(p,q,m,n)


#Create bar plots of total number of influencers of each genre
ggarrange(plots_influencer[[1]], plots_influencer[[2]], plots_influencer[[3]], plots_influencer[[4]], plots_influencer[[5]], plots_influencer[[6]], plots_influencer[[7]],
          labels = c(unique_genre_dataframe[1,"influencer_main_genre"],unique_genre_dataframe[2,"influencer_main_genre"], unique_genre_dataframe[3,"influencer_main_genre"],
                     unique_genre_dataframe[4,"influencer_main_genre"], unique_genre_dataframe[5,"influencer_main_genre"],unique_genre_dataframe[6,"influencer_main_genre"], unique_genre_dataframe[7,"influencer_main_genre"]))

ggarrange(plots_influencer[[8]], plots_influencer[[9]], plots_influencer[[10]], plots_influencer[[11]], plots_influencer[[12]], plots_influencer[[13]], plots_influencer[[14]], 
          labels = c(unique_genre_dataframe[8,"influencer_main_genre"], unique_genre_dataframe[9,"influencer_main_genre"],unique_genre_dataframe[10,"influencer_main_genre"], unique_genre_dataframe[11,"influencer_main_genre"],
                     unique_genre_dataframe[12,"influencer_main_genre"], unique_genre_dataframe[13,"influencer_main_genre"],unique_genre_dataframe[14,"influencer_main_genre"]))
          

ggarrange(plots_influencer[[15]], plots_influencer[[16]], plots_influencer[[17]], plots_influencer[[18]], plots_influencer[[19]], plots_influencer[[20]], 
          labels = c(unique_genre_dataframe[15,"influencer_main_genre"], unique_genre_dataframe[16,"influencer_main_genre"], unique_genre_dataframe[17,"influencer_main_genre"],unique_genre_dataframe[18,"influencer_main_genre"],
                     unique_genre_dataframe[19,"influencer_main_genre"], unique_genre_dataframe[20,"influencer_main_genre"]))




#Create vector of unique genres of followers
unique_genre_dataframe_f <- influence_data %>% distinct(follower_main_genre, .keep_all = TRUE)


#Count unique instances of folowers in each genre and store plots in list
array_follower <- list()
plots_follower <- list()
for(i in 1:20){
  array_follower[[i]] <- influence_data %>%
    filter(follower_main_genre == unique_genre_dataframe_f[i,"follower_main_genre"]) %>%
    distinct(follower_name, .keep_all = TRUE) %>%
    group_by(follower_active_start) %>%
    dplyr::summarize(Total = n()) 
  
  plots_follower[[i]] <- ggplot(data = array_follower[[i]], aes(x = follower_active_start, y = Total)) + 
    geom_bar(stat = "identity")
}


a <- ggplot() + geom_line(data=array_follower[[2]], aes(x = follower_active_start, y = Total, color = "Pop/Rock"),size=2) +  xlim(1940, 2000) + ylim(0,500) + labs(x="Year", y="Total") + 
  scale_color_manual(values = gs.vals) 

p <- ggplot() + geom_line(data=array_follower[[1]], aes(x = follower_active_start, y = Total, color = "R&B;"),size=2) + 
  geom_line(data=array_follower[[3]], aes(x = follower_active_start, y = Total, color = "Comedy/Spoken"),size=2) + 
  geom_line(data=array_follower[[4]], aes(x = follower_active_start, y = Total, color = "Religious"),size=2) + 
  geom_line(data=array_follower[[5]], aes(x = follower_active_start, y = Total, color = "Electronic"),size=2) + 
  xlim(1940, 2000) + ylim(0,125) + labs(x="Year", y="Total") + 
  scale_color_manual(values = gs.vals) 

m <- ggplot() + geom_line(data=array_follower[[6]], aes(x = follower_active_start, y = Total, color = "Country"),size=2) + 
  geom_line(data=array_follower[[7]], aes(x = follower_active_start, y = Total, color = "Blues"),size=2) + 
  geom_line(data=array_follower[[8]], aes(x = follower_active_start, y = Total, color = "Latin"),size=2) + 
  geom_line(data=array_follower[[9]], aes(x = follower_active_start, y = Total, color = "Vocal"),size=2) + 
  geom_line(data=array_follower[[10]], aes(x = follower_active_start, y = Total, color = "Jazz"),size=2) + 
  xlim(1940, 2000) + ylim(0,125) + 
  scale_color_manual(values = gs.vals)
  
q <- ggplot() + geom_line(data= array_follower[[11]], aes(x = follower_active_start, y = Total, color= "Easy Listening"),size=2) + 
  geom_line(data=array_follower[[12]], aes(x = follower_active_start, y = Total, color = "Reggae"),size=2) + 
  geom_line(data=array_follower[[13]], aes(x = follower_active_start, y = Total, color = "International"),size=2) +
  geom_line(data=array_follower[[14]], aes(x = follower_active_start, y = Total, color = "Folk"),size=2) + 
  geom_line(data=array_follower[[15]], aes(x = follower_active_start, y = Total, color = "New Age"),size=2) + 
  xlim(1940, 2000) + ylim(0,125) + 
  scale_color_manual(values = gs.vals)
  
n <- ggplot() + geom_line(data=array_follower[[16]], aes(x = follower_active_start, y = Total, color = "Unknown"),size=2) + 
  geom_line(data=array_follower[[17]], aes(x = follower_active_start, y = Total, color = "Classical"),size=2) + 
  geom_line(data=array_follower[[18]], aes(x = follower_active_start, y = Total, color = "Stage & Screen"),size=2) + 
  geom_line(data=array_follower[[19]], aes(x = follower_active_start, y = Total, color = "Avant-Garde"),size=2) + 
  geom_line(data=array_follower[[20]], aes(x = follower_active_start, y = Total, color = "Children's"),size=2) + 
  xlim(1940, 2000) + ylim(0,125) + 
  scale_color_manual(values = gs.vals)
  
ggarrange(p,q,m,n)


#Create plots of total number of followers of each genre
ggarrange(plots_follower[[1]], plots_follower[[2]], plots_follower[[3]], plots_follower[[4]], plots_follower[[5]], plots_follower[[6]], plots_follower[[7]],
          labels = c(unique_genre_dataframe_f[1,"follower_main_genre"],unique_genre_dataframe_f[2,"follower_main_genre"], unique_genre_dataframe_f[3,"follower_main_genre"],
                     unique_genre_dataframe_f[4,"follower_main_genre"], unique_genre_dataframe_f[5,"follower_main_genre"],unique_genre_dataframe_f[6,"follower_main_genre"], unique_genre_dataframe_f[7,"follower_main_genre"]))

ggarrange(plots_follower[[8]], plots_follower[[9]], plots_follower[[10]], plots_follower[[11]], plots_follower[[12]], plots_follower[[13]], plots_follower[[14]], 
          labels = c(unique_genre_dataframe_f[8,"follower_main_genre"],unique_genre_dataframe_f[9,"follower_main_genre"], unique_genre_dataframe_f[10,"follower_main_genre"],
                     unique_genre_dataframe_f[11,"follower_main_genre"], unique_genre_dataframe_f[12,"follower_main_genre"],unique_genre_dataframe_f[13,"follower_main_genre"], unique_genre_dataframe_f[14,"follower_main_genre"]))

ggarrange(plots_follower[[15]], plots_follower[[16]], plots_follower[[17]], plots_follower[[18]], plots_follower[[19]], plots_follower[[20]], 
          labels = c(unique_genre_dataframe_f[15,"follower_main_genre"],unique_genre_dataframe_f[16,"follower_main_genre"], unique_genre_dataframe_f[17,"follower_main_genre"],
                     unique_genre_dataframe_f[18,"follower_main_genre"], unique_genre_dataframe_f[19,"follower_main_genre"],unique_genre_dataframe_f[20,"follower_main_genre"]))


#Copy follower id column and rename to artists id so that we can join dataframes
influence_data$artists_id <- influence_data$follower_id

#Create new column in influence_data with follower_id as artists_id, cast to string, and add brackets
influence_data$artists_id <- as.character(influence_data$artists_id)
influence_data$artists_id <-  with(influence_data , paste0("[", influence_data$artists_id, "]"))

#Join together full_music_data and influence_data on follower_id
full_influence <- inner_join(full_music_data, influence_data, by = "artists_id")


#Select columns that contain genre and quantitative musical data
full_influence_1 <- full_influence %>% select("follower_main_genre", "danceability", "energy", "valence", "tempo", "loudness", "acousticness", "instrumentalness", "liveness", "speechiness")

#Get mean of each quantitative metric for each genre
genre_data_mean <- aggregate( x = full_influence,
                         by = full_influence[c("follower_main_genre")],
                         FUN = mean)


#Get median of each quantitative metric for each genre
genre_data_median <- aggregate( x = full_influence,
                             by = full_influence[c("follower_main_genre")],
                              FUN = median)

#Get standard deviation of each quantitative metric for each genre
genre_data_sd <- aggregate( x = full_influence,
                                by = full_influence[c("follower_main_genre")],
                                FUN = sd)

genre_data_mean <- cbind(genre_data_mean[,1], genre_data_mean[,3:11])




ggarrange(ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],danceability), y = danceability)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(danceability))) ,
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],energy), y = energy)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(energy))),
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],valence), y = valence)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(valence))),
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],tempo), y = tempo)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(tempo))) + coord_cartesian(ylim = c(80, 130)) ,
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],loudness), y = loudness)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(loudness))),
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],acousticness), y = acousticness)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(acousticness))),
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],speechiness), y = speechiness)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(speechiness))),
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],instrumentalness), y = instrumentalness)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(instrumentalness))),
          ggplot(data = genre_data_mean, aes(x = reorder(genre_data_mean[,1],liveness), y = liveness)) + geom_bar(stat = "identity") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(aes(yintercept = mean(liveness))))
          
          











 genre_data_bounds <- genre_data_mean

#Create upper and lower bound columns for 95% CI
genre_data_bounds$danceabilityUpper <- genre_data_mean$danceability + genre_data_sd$danceability + genre_data_sd$danceability
genre_data_bounds$danceabilityLower <- genre_data_mean$danceability - genre_data_sd$danceability - genre_data_sd$danceability

genre_data_bounds$energyUpper <- genre_data_mean$energy + genre_data_sd$energy + genre_data_sd$energy
genre_data_bounds$energyLower <- genre_data_mean$energy - genre_data_sd$energy - genre_data_sd$energy

genre_data_bounds$valenceUpper <- genre_data_mean$valence + genre_data_sd$valence + genre_data_sd$valence
genre_data_bounds$valenceLower <- genre_data_mean$valence - genre_data_sd$valence - genre_data_sd$valence

genre_data_bounds$tempoUpper <- genre_data_mean$tempo + genre_data_sd$tempo + genre_data_sd$tempo
genre_data_bounds$tempoLower <- genre_data_mean$tempo - genre_data_sd$tempo - genre_data_sd$tempo

genre_data_bounds$loudnessUpper <- genre_data_mean$loudness + genre_data_sd$loudness + genre_data_sd$loudness
genre_data_bounds$loudnessLower <- genre_data_mean$loudness - genre_data_sd$loudness - genre_data_sd$loudness

genre_data_bounds$acousticnessUpper <- genre_data_mean$acousticness + genre_data_sd$acousticness + genre_data_sd$acousticness
genre_data_bounds$acousticnessLower <- genre_data_mean$acousticness - genre_data_sd$acousticness - genre_data_sd$acousticness

genre_data_bounds$instrumentalnessUpper <- genre_data_mean$instrumentalness + genre_data_sd$instrumentalness + genre_data_sd$instrumentalness
genre_data_bounds$instrumentalnessLower <- genre_data_mean$instrumentalness - genre_data_sd$instrumentalness - genre_data_sd$instrumentalness

genre_data_bounds$livenessUpper <- genre_data_mean$liveness + genre_data_sd$liveness + genre_data_sd$liveness
genre_data_bounds$livenessLower <- genre_data_mean$liveness - genre_data_sd$liveness - genre_data_sd$liveness

genre_data_bounds$speechinessUpper <- genre_data_mean$speechiness + genre_data_sd$speechiness + genre_data_sd$speechiness
genre_data_bounds$speechinessLower <- genre_data_mean$speechiness - genre_data_sd$speechiness - genre_data_sd$speechiness


#Remove unnecessary columns
genre_data_mode_key <- full_influence %>% select("follower_main_genre", "mode", "key")

#Get mean mode for each genre
mode <- genre_data_mode_key %>%
  group_by(follower_main_genre) %>%
  summarize(mean = mean(mode))
  

  
#Make bar graph  
ggplot(data = mode, aes(x = reorder(follower_main_genre,mean), y = mean)) + 
  geom_bar(stat = "identity") +
  xlab("Genre") + ylab("Average Mode") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_hline(aes(yintercept = mean(mean)))


#Finds number of followers who have the same and different genres as their influencers
matrix <- vector()
for(i in 1:dim(influence_data)[1]){
  if (influence_data[i,3] == influence_data[i,7]) {
    matrix[i] = 0
  }
  else {
    matrix[i] = 1
  }
}
matrix_table <- table(matrix)

#Get dataframe with only those followers whose genre is different than their influencers
influence_dif_follow <- influence_data %>%
  select("influencer_main_genre", "follower_main_genre") %>%
  filter(influence_data$influencer_main_genre != influence_data$follower_main_genre)

#Get total number of followers in each genre with a different genre than influencer
influence_num <- influence_dif_follow  %>%
  group_by(influencer_main_genre) %>%
  summarize(Total_Influenced_Follow_Dif_Genre = n())

#Get totol number of followers in each genre with the same genre as their influencer
influence_num2 <- influence_data %>%
  select("influencer_main_genre", "follower_main_genre") %>%
  group_by(influencer_main_genre) %>%
  summarize(Total_Influenced = n())

#Combine these data frames and calculate the proportion of their followers who have the same genre
influence_num <- cbind(influence_num2, influence_num$Total_Influenced_Follow_Dif_Genre)
influence_num$Proportion <-  influence_num$`influence_num$Total_Influenced_Follow_Dif_Genre` /influence_num$Total_Influenced


#Plot proportion
ggplot(data = influence_num, aes(x = reorder(influencer_main_genre,Proportion), y = Proportion)) + 
  geom_bar(stat = "identity") +
  xlab("Genre") + ylab("Proportion of Followers in same Genre") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_hline(aes(yintercept = mean(Proportion)))


#Look at where pop/rock influences besides pop/rock
poprock_to_othergenre <- influence_dif_follow %>%
  filter(influence_dif_follow$influencer_main_genre == "Pop/Rock") %>%
  group_by(follower_main_genre) %>%
  summarize(Total = n())

ggplot(data = poprock_to_othergenre, aes(x = reorder(follower_main_genre,Total), y = Total)) + 
  geom_bar(stat = "identity") +
  xlab("Genre") + ylab("Number of Followers in Genre") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
