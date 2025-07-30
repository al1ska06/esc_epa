#scatter plots 2: third independent variable through color

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
