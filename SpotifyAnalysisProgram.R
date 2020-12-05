# Install all the packages and their dependencies 
library(dplyr) 
library(ggplot2) 
library(plotly)
library(tidyverse) 
library(kableExtra) 
library(DT) 
library(corrplot) 
library(gridExtra) 
library(treemap)
library(viridisLite) 
library(fmsb) 
library(cowplot) 
library(factoextra)  
library(formattable) 

# get the data, structure and summary of the data as well
songs <- read.csv("spotify_songs.csv")
head(songs)
str(songs)
summary(songs)

# we will drop the columns track_name, track_artist and track_album_name since there are only five missing values out of almost 33k values
songs_clean <- songs %>% filter(!is.na(track_name) & !is.na(track_artist) & !is.na(track_album_name))
# we will look for duplicate values if any 
songs_clean[duplicated(songs_clean$Names) | duplicated(songs_clean$Names, fromLast = TRUE), ]
# we will now extract year from track_album_release_date
songs_clean$year <- as.numeric(substring(songs_clean$track_album_release_date,1,4))
# we will drop unimportant columns: track_id, track_album_id and playlist_id. We will drop this since they are of no use during analysis
songs_clean <- songs_clean%>%dplyr::select(-track_id,-track_album_id,-playlist_id)
# final numbers of the data set after cleaning it
dim(songs_clean)

head(songs_clean, 20) %>%
  datatable(options = list(scrollCollapse = TRUE,scrollX = TRUE,
                           columnDefs = list(list(className = 'dt-center', targets = 1:4))
  ))



# data exploration 
# we will plot a pie chart to depict different playlist genres
songs_clean_pie_data <- songs_clean %>% 
  group_by(playlist_genre) %>% 
  summarise(Total_number_of_tracks = length(playlist_genre))

piechart1 <- ggplot(songs_clean_pie_data, aes(x=" ", y=Total_number_of_tracks, fill=playlist_genre)) + 
  geom_bar(width = 1, stat = "identity") + scale_fill_brewer(palette="Blues")+
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))

print(piechart1)

# correlation between different variables and then we also make a correlation plot 
songs_correlation <- cor(songs_clean[,-c(1,2,4,5,6,7,8)])
corrplot(songs_correlation, type = "upper", tl.srt = 45)

# density plot of variables 
correlated_density <- ggplot(songs_clean) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(acousticness, fill ="acousticness", alpha = 0.1)) + 
  geom_density(aes(speechiness, fill ="speechiness", alpha = 0.1)) + 
  geom_density(aes(liveness, fill ="liveness", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Danceability, Valence, Acousticness, Speechiness and Liveness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Danceability, Valence, Acousticness, Speechiness and Liveness") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        text = element_text(size = 10)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Pastel2")
print(correlated_density)

# density plots of various characteristics of music
loudness_density <- ggplot(songs_clean) +
  geom_density(aes(loudness, fill ="loudness")) + 
  scale_x_continuous(name = "Loudness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Loudness") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="BrBG")
print(loudness_density)

duration_ms_density <- ggplot(songs_clean) +
  geom_density(aes(duration_ms, fill ="duration_ms")) + 
  scale_x_continuous(name = "duration_ms") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of duration_ms") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="PiYG")
print(duration_ms_density)

track_popularity_density <- ggplot(songs_clean) +
  geom_density(aes(track_popularity, fill ="track_popularity")) + 
  scale_x_continuous(name = "track_popularity") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of track_popularity") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="PRGn")
print(track_popularity_density)

grid.arrange(loudness_density, duration_ms_density,track_popularity_density, nrow = 3)

# We will generate a treemap for top artists in each genre
top_genre <- songs_clean %>% select(playlist_genre, track_artist, track_popularity) %>% group_by(playlist_genre,track_artist) %>% summarise(n = n()) %>% top_n(15, n)
tm <- treemap(top_genre, index = c("playlist_genre", "track_artist"), vSize = "n", vColor = 'playlist_genre', palette = viridis::magma(6) ,title="Top 15 Track Artists within each Playlist Genre")

# we will make a radar chart to see track characteristics vary across different genres
radar_chart <- function(arg){
  songs_clean_filtered <- songs_clean %>% filter(playlist_genre==arg)
  radar_data_v1 <- songs_clean_filtered %>%
    select(danceability,energy,loudness,speechiness,valence,instrumentalness,acousticness)
  radar_data_v2 <- apply(radar_data_v1,2,function(x){(x-min(x)) / diff(range(x))})
  radar_data_v3 <- apply(radar_data_v2,2,mean)
  radar_data_v4 <- rbind(rep(1,6) , rep(0,6) , radar_data_v3)
  return(radarchart(as.data.frame(radar_data_v4),title=arg))
}

par(mfrow = c(2, 3))
Chart_pop<-radar_chart("pop")
Chart_rb<-radar_chart("r&b")
Chart_edm<-radar_chart("edm")
Chart_latin<-radar_chart("latin")
Chart_rap<-radar_chart("rap")
Chart_rock<-radar_chart("rock")

# checking how have music preferences changed over 10 years 
trend_chart <- function(arg){
  trend_change <- songs_clean %>% filter(year>2010) %>% group_by(year) %>% summarize_at(vars(all_of(arg)), funs(Average = mean))
  chart<- ggplot(data = trend_change, aes(x = year, y = Average)) + 
    geom_line(color = "#000000", size = 1) +
    scale_x_continuous(breaks=seq(2011, 2020, 1)) + scale_y_continuous(name=paste("",arg,sep=""))
  return(chart)
}

trend_chart_track_popularity<-trend_chart("track_popularity")
trend_chart_danceability<-trend_chart("danceability")
trend_chart_energy<-trend_chart("energy")
trend_chart_loudness<-trend_chart("loudness")
trend_chart_duration_ms<-trend_chart("duration_ms")
trend_chart_speechiness<-trend_chart("speechiness")

all_trends <- plot_grid(trend_chart_track_popularity, trend_chart_danceability, trend_chart_energy, trend_chart_loudness, trend_chart_duration_ms, trend_chart_speechiness,ncol = 2, label_size = 1)
print(all_trends)

# Machine Learning Algorithm - K Means Clustering
# we will select the required song characteristics for clustering
cluster.input <- songs_clean[, c('energy', 'liveness','tempo', 'speechiness', 'acousticness',
                                 'instrumentalness', 'danceability', 'duration_ms' ,'loudness','valence')]

# we will scale these characteristics for input to clustering 
cluster.input.scaled <- scale(cluster.input[, c('energy', 'liveness', 'tempo', 'speechiness'
                                                , 'acousticness', 'instrumentalness', 'danceability'
                                                , 'duration_ms' ,'loudness', 'valence')])


# we will visualize clusters for different values of k 
k2 <- kmeans(cluster.input.scaled, centers = 2, nstart = 25)
k3 <- kmeans(cluster.input.scaled, centers = 3, nstart = 25)
k4 <- kmeans(cluster.input.scaled, centers = 4, nstart = 25)
k5 <- kmeans(cluster.input.scaled, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 5")

k_cluster_plot <- grid.arrange(p1, p2, p3, p4, nrow = 2)
print(k_cluster_plot)

# now we will identify the optimal number of clusters. For this we will make use of elbow method
# 1. we will make a plot
set.seed(100)
value_of_k <- fviz_nbclust(cluster.input[1:1000,], kmeans, method = "wss", linecolor = "darkgreen")
print(value_of_k)

# 2. we will see a tabular form
n_clust<-fviz_nbclust(cluster.input[1:1000,], kmeans, method = "wss")
n_clust<-n_clust$data

cluster_data <- n_clust %>% rename(Number_of_clusters=clusters,Within_sum_of_squared_error=y) %>% 
  mutate(Within_sum_of_squared_error = color_tile("white", "darkgreen")(Within_sum_of_squared_error)) %>% 
  kable("html", escape = F) %>% 
  kable_styling("hover", full_width = F) %>% 
  column_spec(2, width = "5cm") %>%
  row_spec(3:3, bold = T, color = "white", background = "grey")
print(cluster_data)















