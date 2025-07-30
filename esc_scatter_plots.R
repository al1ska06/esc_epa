library(tidyverse)
library(dplyr)
library (patchwork) #this was for finalizing plots together


#start by separating and filtering each year's data
song_2015 <- filter(song_data, year == 2015, energy != "-", final_televote_points !="-")
song_2015$energy <- as.numeric(as.character(song_2015$energy))
song_2015$final_televote_points <- as.numeric(as.character(song_2015$final_televote_points))

song_2016 <- filter(song_data, year == 2016, energy != "-", final_televote_points !="-")
song_2016$energy <- as.numeric(as.character(song_2016$energy))
song_2016$final_televote_points <- as.numeric(as.character(song_2016$final_televote_points))

song_2017 <- filter(song_data, year == 2017, energy != "-", final_televote_points !="-")
song_2017$energy <- as.numeric(as.character(song_2017$energy))
song_2017$final_televote_points <- as.numeric(as.character(song_2017$final_televote_points))

song_2018 <- filter(song_data, year == 2018, energy != "-", final_televote_points !="-")
song_2018$energy <- as.numeric(as.character(song_2018$energy))
song_2018$final_televote_points <- as.numeric(as.character(song_2018$final_televote_points))

song_2019 <- filter(song_data, year == 2019, energy != "-", final_televote_points !="-")
song_2019$energy <- as.numeric(as.character(song_2019$energy))
song_2019$final_televote_points <- as.numeric(as.character(song_2019$final_televote_points))


#plot each 

p1 <- ggplot(song_2015, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2015)"
  )

p2 <- ggplot(song_2016, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2016)"
  )

p3 <- ggplot(song_2017, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2017)"
  )

p4 <- ggplot(song_2018, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2018)"
  )


#visualize together using patchwork
((p1|p2)/(p3|p4))


#-------------------scatter plots 2: third independent variable through color

#filter 1: all energy, #backup dancers, and final total points 2015-2018
song2 <- filter(song_data, year %in% c(2015,2016,2017,2018), energy != "-", backing_dancers != "-", final_total_points !="-")

song2$energy <- as.numeric(as.character(song2$energy))
song2$final_total_points <- as.numeric(as.character(song2$final_total_points))
song2$backing_dancers <- as.numeric(as.character(song2$backing_dancers))
song2$dancers <- song2$backing_dancers > 0

song2_15 <- filter(song2, year==2015) 
song2_16 <- filter(song2, year==2016) 
song2_17 <- filter(song2, year==2017) 
song2_18 <- filter(song2, year==2018) 

p2_15 <- ggplot(song2_15, aes(x = energy, y = final_total_points, color = dancers)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final total points",
    color = "has backing dancers?",
    title = "Points by energy level (2015)"
  )+
  scale_color_brewer(palette = "Dark2")

p2_16 <- ggplot(song2_16, aes(x = energy, y = final_total_points, color = dancers)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final total points",
    color = "has backing dancers?",
    title = "Points by energy level (2015)"
  )+
  scale_color_brewer(palette = "Dark2")

p2_17 <- ggplot(song2_17, aes(x = energy, y = final_total_points, color = dancers)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final total points",
    color = "has backing dancers?",
    title = "Points by energy level (2015)"
  )+
  scale_color_brewer(palette = "Dark2")

p2_18 <- ggplot(song2_18, aes(x = energy, y = final_total_points, color = dancers)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits = c(10, 90)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "final total points",
    color = "has backing dancers?",
    title = "Points by energy level (2015)"
  )+
  scale_color_brewer(palette = "Dark2")

((p2_15 | p2_16)/(p2_17 | p2_18))


#-----------------plots 3: regression line
#comparing impacts of energy on jury and televote outcomes

ggplot(song2, aes(x = energy)) + 
  geom_point(aes(y = final_jury_points, color = "Jury")) +
  geom_point(aes(y = final_televote_points, color = "Televote")) +
  geom_smooth(aes(y = final_jury_points, color = "Jury"), method = "lm") +
  geom_smooth(aes(y = final_televote_points, color = "Televote"), method = "lm") +
  scale_x_continuous(breaks = seq(10, 100, by = 10),limits = c(10, 100)) +
  scale_y_continuous(breaks = seq(0,300, by = 100), limits = c(0,400))+
  labs(
    y = "Points awarded",
    title = "Final jury points (2015-2018)"
  )+
  scale_color_brewer(palette = "Dark2")



