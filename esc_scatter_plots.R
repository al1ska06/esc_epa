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
attach(mtcars)
par(mfrow = c(2,2))

p1 <- ggplot(song_2015, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0,300, by = 100))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2015)"
  )

p2 <- ggplot(song_2016, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0,300, by = 100))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2016)"
  )

p3 <- ggplot(song_2017, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0,300, by = 100))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2017)"
  )

p4 <- ggplot(song_2018, aes(x = energy, y = final_televote_points)) + 
  geom_point()+
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0,300, by = 100))+
  labs(
    y = "final televote points",
    title = "Points by energy level (2018)"
  )


#visualize together using patchwork
((p1|p2)/(p3|p4))


