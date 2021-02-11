#rock pop including influence

#Create vector of unique genres of followers
unique_genre_dataframe_f <- influence_data %>% distinct(influencer_main_genre, .keep_all = TRUE)

#Copy follower id column and rename to artists id so that we can join dataframes
influence_data$artist_id <- influence_data$influencer_id

#Join together full_music_data and influence_data on influencer_id
full_influence <- inner_join(influence_data, data_by_artist, by = "artist_id")

#Select columns that contain genre and quantitative musical data
full_influence <- full_influence %>% select("influencer_main_genre", "danceability", "energy", "valence", "acousticness", "instrumentalness", "liveness", "speechiness", "influencer_active_start")

#do for rock/pop genre
rock.pop <- full_influence %>%
  filter(influencer_main_genre == "Pop/Rock")

rock.pop.mean <- aggregate( x = rock.pop,
                            by = rock.pop[c("influencer_active_start")],
                            FUN = mean)
#remove duplicates
rock.pop.mean <- rock.pop.mean[-1]
rock.pop.mean <- rock.pop.mean[-1]

#-------------------------------------------------

#gray scale hex values
gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)

p <- ggplot() + 
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[1]], color = "Danceability"),size=2) +
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[5]], color = "Instrumentalness"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[2]], color = "Energy"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[7]], color = "Speechiness"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[3]], color = "Valence"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[6]], color = "Liveness"),size=2) + 
  geom_line(data=rock.pop.mean, aes(x = influencer_active_start, y = rock.pop.mean[[4]], color = "Acousticness"),size=2) + 
  xlim(1930, 2020) + ylim(0,1) + labs(x="Year", y="Value", title="Pop/Rock Measures by Artist Weighted") +theme_bw() +
  scale_color_manual(values = gs.vals)

ggarrange(p)

#-------------------------------------------------

#rock pop means deviation from year data
rock.pop.deviate <- means
rock.pop.deviate[,2:8] <- rock.pop.mean[,2:8] - means[2:8]

gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)

p <- ggplot() + 
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[2]], color = "Danceability"),size=1.5) +
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[6]], color = "Instrumentalness"),size=1.5) + 
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[3]], color = "Energy"),size=1.5) + 
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[8]], color = "Speechiness"),size=1.5) + 
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[4]], color = "Valence"),size=1.5) + 
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[7]], color = "Liveness"),size=1.5) + 
  geom_line(data=rock.pop.deviate, aes(x = influencer_active_start, y = rock.pop.deviate[[5]], color = "Acousticness"),size=1.5) + 
  xlim(1930, 2010) + ylim(-0.3,0.3) + labs(x="Year", y="Value", title="Difference from Yearly means Pop/Rock") +theme_bw() +
  scale_color_manual(values = gs.vals)

ggarrange(p)

#-------------------------------------------------
#pop/rock artist data

pruned_influence_data <- influence_data[,c(2:4,9)]

#Join together full_music_data and influence_data on influencer_id
pruned_influence <- inner_join(pruned_influence_data, data_by_artist, by = "artist_id")

#Create vector of unique genres of followers
unique_influence <- pruned_influence %>% distinct(artist_id, .keep_all = TRUE)

#Select columns that contain genre and quantitative musical data
unique_influence <- unique_influence %>% select("influencer_main_genre", "danceability", "energy", "valence", "acousticness", "instrumentalness", "liveness", "speechiness", "influencer_active_start")

#do for rock/pop genre
rock.pop.artist <- unique_influence %>%
  filter(influencer_main_genre == "Pop/Rock")

rock.pop.artist.mean <- aggregate( x = rock.pop.artist,
                            by = rock.pop.artist[c("influencer_active_start")],
                            FUN = mean)
#eliminate duplicates
rock.pop.artist.mean <- rock.pop.artist.mean[-1]

gs.vals <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gs.vals <- sort(gs.vals,decreasing = TRUE)

p <- ggplot() + 
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[2]], color = "Danceability"),size=1.5) +
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[6]], color = "Instrumentalness"),size=1.5) + 
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[3]], color = "Energy"),size=1.5) + 
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[8]], color = "Speechiness"),size=1.5) + 
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[4]], color = "Valence"),size=1.5) + 
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[7]], color = "Liveness"),size=1.5) + 
  geom_line(data=rock.pop.artist.mean, aes(x = influencer_active_start, y = rock.pop.artist.mean[[5]], color = "Acousticness"),size=1.5) + 
  xlim(1930, 2010) + ylim(0,1) + labs(x="Year", y="Value", title="Pop/Rock Measures by Artist Unweighted") +theme_bw() +
  scale_color_manual(values = gs.vals)

ggarrange(p)
