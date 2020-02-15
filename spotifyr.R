#spotifyr

library(tidyverse)
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs %>%
    count(playlist_genre, playlist_subgenre) %>%
    View()

hist(spotify_songs$track_popularity)

spotify_songs %>%
    ggplot(aes(x = track_popularity)) +
    geom_histogram() +
    facet_wrap(. ~ playlist_genre)


install.packages("DataExplorer")
library(DataExplorer)


