library(dplyr)
library(tidyr)
library(readxl)  
library(ggplot2)
library(ggpubr)
library(convertr)
library(neuralnet)


data_by_artist <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/data_by_artist2.csv")
full_music_data <- read.csv("/Users/johnridgeway/Desktop/2021_ICM_Problem_D_Data/full_music_data.csv")
data_by_year <- read.csv("/Users/johnridgeway/Desktop/2021_ICM_Problem_D_Data/data_by_year.csv")
influence_data <- read.csv("/Users/johnridgeway/Desktop/comap-2021-master/influence_data2.csv")



## By influencer
influence_data$artist_id <- influence_data$follower_id

data_by_artist_genre <- inner_join(data_by_artist, influence_data, by = "artist_id" )


data_by_artist_pop_rock <- data_by_artist_genre %>% 
  filter(influencer_main_genre == "Pop/Rock") %>%
  filter((influencer_active_start >= 1990) & (influencer_active_start < 2000))

data_by_artist1 <- data_by_artist_pop_rock %>% filter(influencer_name == "Radiohead")
data_by_artist2 <- data_by_artist_pop_rock %>% filter(influencer_name != "Radiohead")
count_wrong = 0
numruns = 0


tc <- data_by_artist2[sample(nrow(data_by_artist2), 55),]


#Randomize training set?
artist_data_teach <- data_by_artist1[1:45,]
artist_data_test <- data_by_artist1[46:55,]
tc_teach <- tc[1:45, ]
tc_test <- tc[46:55, ]

danceability_teach <- c(artist_data_teach$danceability, tc_teach$danceability)
energy_teach <- c(artist_data_teach$energy, tc_teach$energy)
valence_teach <- c(artist_data_teach$valence, tc_teach$valence)
tempo_teach <- c(artist_data_teach$tempo, tc_teach$tempo)
loudness_teach <- c(artist_data_teach$loudness, tc_teach$loudness)
mode_teach <- c(artist_data_teach$mode, tc_teach$mode)                   
key_teach <- c(artist_data_teach$key, tc_teach$key)
acousticness_teach <- c(artist_data_teach$acousticness, tc_teach$acousticness)
instrumentalness_teach <- c(artist_data_teach$instrumentalness, tc_teach$instrumentalness)
liveness_teach <- c(artist_data_teach$liveness, tc_teach$liveness)               
speechiness_teach <- c(artist_data_teach$speechiness, tc_teach$speechiness)

placed <- vector()
for (i in 1:45){
  placed[i] <- 1
}
for (i in 46:90){
  placed[i] <- 0
}

df_teach = data.frame(danceability_teach,energy_teach,valence_teach,tempo_teach,loudness_teach,acousticness_teach,instrumentalness_teach,liveness_teach,speechiness_teach,placed)


require(neuralnet)

# fit neural network
nn=neuralnet(placed~danceability_teach + energy_teach+valence_teach+tempo_teach+loudness_teach+acousticness_teach+instrumentalness_teach+liveness_teach+speechiness_teach,data=df_teach, hidden=6,act.fct = "logistic",
             linear.output = FALSE)


#plot(nn)


danceability_test <- c(artist_data_test$danceability, tc_test$danceability)
energy_test <- c(artist_data_test$energy, tc_test$energy)
valence_test <- c(artist_data_test$valence, tc_test$valence)
tempo_test <- c(artist_data_test$tempo, tc_test$tempo)
loudness_test <- c(artist_data_test$loudness, tc_test$loudness)
mode_test <- c(artist_data_test$mode, tc_test$mode)                   
key_test <- c(artist_data_test$key, tc_test$key)
acousticness_test <- c(artist_data_test$acousticness, tc_test$acousticness)
instrumentalness_test <- c(artist_data_test$instrumentalness, tc_test$instrumentalness)
liveness_test <- c(artist_data_test$liveness, tc_test$liveness)               
speechiness_test <- c(artist_data_test$speechiness, tc_test$speechiness)

df_test = data.frame(danceability_test,energy_test,valence_test,tempo_test,loudness_test,acousticness_test,instrumentalness_test,liveness_test,speechiness_test)


Predict = compute(nn,df_test)

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred


for (i in 1:10){
if(pred[i] == 0){
  count_wrong = count_wrong + 1
}}
for(i in 11:20){
if(pred[i] == 1){
  count_wrong = count_wrong + 1
}}
count_wrong
numruns = numruns + 1


















## By genre
influence_data$artist_id <- influence_data$follower_id
influence_data2 <- influence_data[,c("artist_id","follower_main_genre","follower_active_start")]

data_by_artist_genre2 <- inner_join(data_by_artist, influence_data2, by = "artist_id" )
data_by_artist_genre2 <- data_by_artist_genre2 %>%  distinct(artist_name, .keep_all = TRUE)

data_by_artist_pop_rock2 <- data_by_artist_genre2 %>% 
  filter((follower_active_start >= 1990) & (follower_active_start < 2000))


data_by_genre1 <- data_by_artist_pop_rock2 %>% filter(follower_main_genre == "Pop/Rock")
data_by_genre2 <- data_by_artist_pop_rock2 %>% filter(follower_main_genre != "Pop/Rock")
count_wrong = 0
numruns = 0


tc <- data_by_genre2[sample(nrow(data_by_genre2), 60),]


#Randomize training set?
artist_data_teach <- data_by_genre1[1:50,]
artist_data_test <- data_by_genre1[51: 60,]
tc_teach <- tc[1:50, ]
tc_test <- tc[51:60, ]

danceability  <- c(artist_data_teach$danceability, tc_teach$danceability)
energy  <- c(artist_data_teach$energy, tc_teach$energy)
valence  <- c(artist_data_teach$valence, tc_teach$valence)
tempo  <- c(artist_data_teach$tempo, tc_teach$tempo)
loudness  <- c(artist_data_teach$loudness, tc_teach$loudness)
mode  <- c(artist_data_teach$mode, tc_teach$mode)                   
key  <- c(artist_data_teach$key, tc_teach$key)
acousticness  <- c(artist_data_teach$acousticness, tc_teach$acousticness)
instrumentalness  <- c(artist_data_teach$instrumentalness, tc_teach$instrumentalness)
liveness  <- c(artist_data_teach$liveness, tc_teach$liveness)               
speechiness  <- c(artist_data_teach$speechiness, tc_teach$speechiness)

output <- vector()
for (i in 1:50){
  output[i] <- 1
}
for (i in 51:100){
  output[i] <- 0
}

df_teach = data.frame(danceability ,energy ,valence ,tempo ,loudness ,acousticness ,instrumentalness ,liveness ,speechiness ,output)


require(neuralnet)

# fit neural network
nn=neuralnet(output~danceability  + energy +valence +tempo +loudness +acousticness +instrumentalness +liveness +speechiness ,data=df_teach, hidden=4,act.fct = "logistic",
             linear.output = FALSE)


#plot(nn, information = FALSE,show.weights =FALSE)


danceability_test <- c(artist_data_test$danceability, tc_test$danceability)
energy_test <- c(artist_data_test$energy, tc_test$energy)
valence_test <- c(artist_data_test$valence, tc_test$valence)
tempo_test <- c(artist_data_test$tempo, tc_test$tempo)
loudness_test <- c(artist_data_test$loudness, tc_test$loudness)
mode_test <- c(artist_data_test$mode, tc_test$mode)                   
key_test <- c(artist_data_test$key, tc_test$key)
acousticness_test <- c(artist_data_test$acousticness, tc_test$acousticness)
instrumentalness_test <- c(artist_data_test$instrumentalness, tc_test$instrumentalness)
liveness_test <- c(artist_data_test$liveness, tc_test$liveness)               
speechiness_test <- c(artist_data_test$speechiness, tc_test$speechiness)

df_test = data.frame(danceability_test,energy_test,valence_test,tempo_test,loudness_test,acousticness_test,instrumentalness_test,liveness_test,speechiness_test)


Predict = compute(nn,df_test)

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred


for (i in 1:10){
  if(pred[i] == 0){
    count_wrong = count_wrong + 1
  }}
for(i in 11:20){
  if(pred[i] == 1){
    count_wrong = count_wrong + 1
  }}
count_wrong
numruns = numruns + 1






#Only country

data_by_genre1 <- data_by_artist_pop_rock2 %>% filter(follower_main_genre == "Country")



danceability_test <- data_by_genre1$danceability
energy_test <-  data_by_genre1$energy
valence_test <- data_by_genre1$valence
tempo_test <- data_by_genre1$tempo
loudness_test <- data_by_genre1$loudness
mode_test <-  data_by_genre1$mode
key_test <- data_by_genre1$key
acousticness_test <- data_by_genre1$acousticness
instrumentalness_test <- data_by_genre1$instrumentalness
liveness_test <- data_by_genre1$liveness
speechiness_test <- data_by_genre1$speechiness



df_test = data.frame(danceability_test,energy_test,valence_test,tempo_test,loudness_test,acousticness_test,instrumentalness_test,liveness_test,speechiness_test)


Predict = compute(nn,df_test)

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

count_wrong = 0
for (i in 1:71){
  if(pred[i] == 1){
    count_wrong = count_wrong + 1
  }}
count_wrong


