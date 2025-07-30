#histograms: points distributions

songs <- filter(song_data,!if_any(c(final_jury_points, final_televote_points, final_total_points), ~ .x == "-") )
songs <- select(songs, c('year', 'style', 'final_jury_points', 'final_televote_points', 'final_total_points'))

songs09 <- filter(songs, year %in% 2009:2016)
songs17 <- filter(songs, year %in% 2017:2024)

s09 <- ggplot(songs09, aes(x = final_total_points))+
  geom_histogram(bins = 20)+
  scale_x_continuous( limits = c(0, 800))+
  scale_y_continuous( limits = c(0, 40))+
  labs(title = "Final points awarded from 2009-2016",
       x = "points awarded")

s17<- ggplot(songs17, aes(x = final_total_points))+
  geom_histogram(bins = 20)+
  scale_x_continuous( limits = c(0, 800))+
  scale_y_continuous( limits = c(0, 40))+
  labs(title = "Final points awarded from 2017-2024",
       x = "points awarded")

(s09 | s17)

#histogram: represented styles by year
h_song <- ggplot(songs, aes(x = style, fill = style))+
  geom_bar(show.legend = FALSE)+
  labs( x = "Style of music", title = "Number of entries by music style, 2009-2024", color = "Style")+
  scale_fill_brewer(palette = "Set1")+
  theme_minimal()
  
#boxplot to support findings of feedback loop:
b_song <- ggplot(songs, aes(x = style, y = final_total_points))+
  labs(x = "Style of music", y = "final points awarded (jury and televote)", title = "Points awarded by style, 2009-2024")+
  geom_boxplot()+
  theme_minimal()

(h_song | b_song)
