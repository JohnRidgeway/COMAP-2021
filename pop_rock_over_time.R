#use John code to create plots showing the changes of year values across
#genres over time
#start with pop/rock

library(igraph)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

#import data
influence_data <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/influence_data2.csv",header=TRUE)
data_by_artist <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/data_by_artist2.csv",header=TRUE)
data_by_year <- read.csv("/Users/johnridgeway/Desktop/2021_ICM_Problem_D_Data/data_by_year.csv",header=TRUE)
full_music_data <- read.csv("/Users/johnridgeway/Desktop/2021_ICM_Problem_D_Data/full_music_data.csv",header=TRUE)

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

#Copy follower id column and rename to artists id so that we can join dataframes
influence_data$artists_id <- influence_data$follower_id

#Create new column in influence_data with follower_id as artists_id, cast to string, and add brackets
influence_data$artists_id <- as.character(influence_data$artists_id)
influence_data$artists_id <-  with(influence_data , paste0("[", influence_data$artists_id, "]"))

#Join together full_music_data and influence_data on follower_id
full_influence <- inner_join(full_music_data, influence_data, by = "artists_id")


#Select columns that contain genre and quantitative musical data
full_influence <- full_influence %>% select("follower_main_genre", "danceability", "energy", "valence", "acousticness", "instrumentalness", "liveness", "speechiness", "follower_active_start")



#do for rock/pop genre
rock.pop <- full_influence %>%
  filter(follower_main_genre == "Pop/Rock")

rock.pop.mean <- aggregate( x = rock.pop,
                              by = rock.pop[c("follower_active_start")],
                              FUN = mean)
rock.pop.mean <- rock.pop.mean %>% select(1,4,5,6,11,12,13,14)
                                          
#gray scale hex values
gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)

p <- ggplot() + 
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = follower_active_start, y = rock.pop.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Pop/Rock Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)



#do for R&B genre
r.b <- full_influence %>%
  filter(follower_main_genre == "R&B;")

r.b.mean <- aggregate( x = r.b,
                            by = r.b[c("follower_active_start")],
                            FUN = mean)
#remove duplicates
r.b.mean <- r.b.mean %>% select(1,4,5,6,11,12,13,14)

q <- ggplot() + 
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=r.b.mean, aes(x = follower_active_start, y = r.b.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="R&B Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)



#do for Electronic genre
electronic <- full_influence %>%
  filter(follower_main_genre == "Electronic")

electronic.mean <- aggregate( x = electronic,
                       by = electronic[c("follower_active_start")],
                       FUN = mean)
#remove duplicates
electronic.mean <- electronic.mean %>% select(1,4,5,6,11,12,13,14)

m <- ggplot() + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Electronic Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)

#do for Country genre
country <- full_influence %>%
  filter(follower_main_genre == "Country")

country.mean <- aggregate( x = country,
                              by = country[c("follower_active_start")],
                              FUN = mean)
#remove duplicates
country.mean <- country.mean %>% select(1,4,5,6,11,12,13,14)

n <- ggplot() + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Country Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)



#do for Electronic genre
electronic <- full_influence %>%
  filter(follower_main_genre == "Electronic")

electronic.mean <- aggregate( x = electronic,
                              by = electronic[c("follower_active_start")],
                              FUN = mean)
#remove duplicates
electronic.mean <- electronic.mean %>% select(1,4,5,6,11,12,13,14)

m <- ggplot() + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=electronic.mean, aes(x = follower_active_start, y = electronic.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Electronic Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)

#do for Country genre
country <- full_influence %>%
  filter(follower_main_genre == "Country")

country.mean <- aggregate( x = country,
                           by = country[c("follower_active_start")],
                           FUN = mean)
#remove duplicates
country.mean <- country.mean %>% select(1,4,5,6,11,12,13,14)

n <- ggplot() + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=country.mean, aes(x = follower_active_start, y = country.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Country Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)










#do for Vocal genre
vocal <- full_influence %>%
  filter(follower_main_genre == "Vocal")

vocal.mean <- aggregate( x = vocal,
                              by = vocal[c("follower_active_start")],
                              FUN = mean)
#remove duplicates
vocal.mean <- vocal.mean %>% select(1,4,5,6,11,12,13,14)

a <- ggplot() + 
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=vocal.mean, aes(x = follower_active_start, y = vocal.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Vocal Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)











#do for Folk genre
folk <- full_influence %>%
  filter(follower_main_genre == "Folk")

folk.mean <- aggregate( x = folk,
                           by = folk[c("follower_active_start")],
                           FUN = mean)
#remove duplicates
folk.mean <- folk.mean %>% select(1,4,5,6,11,12,13,14)

b <- ggplot() + 
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[2]], color = "Danceability"),size=2) +
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[6]], color = "Instrumentalness"),size=2) + 
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[3]], color = "Energy"),size=2) + 
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[8]], color = "Speechiness"),size=2) + 
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[4]], color = "Valence"),size=2) + 
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[7]], color = "Liveness"),size=2) + 
  geom_line(data=folk.mean, aes(x = follower_active_start, y = folk.mean[[5]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Folk Measures by Year") +theme_bw() +
  scale_color_manual(values = gs.vals)








ggarrange(p,q,m,n,a,b)

#compare to yearly trends next
