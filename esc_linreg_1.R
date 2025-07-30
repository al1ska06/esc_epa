#plots 3: regression line
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
