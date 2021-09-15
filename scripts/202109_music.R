# Music!
library(tidyverse)
library(extrafont)
library(ggtext)

# Get the data ready ----
tt_data <- tidytuesdayR::tt_load(2021, week = 38)

music_data <- tt_data$billboard %>%
  group_by(song_id) %>%
  mutate(entry_week = min(lubridate::as_date(week_id, format = "%m/%d/%Y")),
         total_weeks = max(weeks_on_chart, na.rm = T)) %>%
  select(song, performer, song_id, entry_week, total_weeks) %>%
  unique() %>%
  left_join(select(tt_data$audio_features,
                   song_id, spotify_track_popularity)) %>%
  na.omit() %>%
  mutate(song_group = factor(
    case_when(
    grepl("\\byou\\b|\\byour\\b|\\byours\\b|\byou'\b", song, ignore.case = T) &
      grepl("\\bme\\b|\\bI\\b|\\bmy\\b", song, ignore.case = T) ~ "Us",
    grepl("\\byou\\b|\\byour\\b|\\byours\\b|\byou'\b", song, ignore.case = T) ~ "You",
    grepl("\\bme\\b|\\bI\\b|\\bmy\\b", song, ignore.case = T) ~ "Me",
    grepl("\\bwe\\b|\\bus\\b", song, ignore.case = T) ~ "Us",
    # required so that I isn't interpreted as I when it's used for "1"
    grepl("Part I", song) ~ "Everything else",
   TRUE ~ "Everything else"), levels = c("You", "Me", "Us", "Everything else"))
  )

# Plot it! ----
ggplot(music_data) +
  geom_segment(aes(x = entry_week, xend = entry_week,
                   y = -total_weeks/2, yend = total_weeks/2,  
                 alpha = spotify_track_popularity,
                 colour = song_group),
               size = 0.3,
               show.legend = F) +
#  scale_size(range = c(0.001, 1.5)) +
  scale_color_manual(values = c("#8d75d7", "#75d775", "#75cad7", "#6d7878")) +
  theme_minimal() %+replace%
  theme(plot.background = element_rect(fill = "#061214", colour = "#061214"),
        panel.grid = element_line(color = "#061214"),
        panel.background = element_rect(fill = "#061214", colour = "#061214"),
        text = element_text(colour = "#eff5f6", family = "Corbel Light"), 
        plot.title = element_markdown(hjust = 0.5, size = 20, family = "Gotham Medium"),
        axis.text = element_text(color = "#eff5f6", size = 10),
        axis.text.y = element_blank(),
        plot.subtitle = element_markdown(hjust = 0.5, size = 13, lineheight = 1),
        axis.title = element_text(color = "#eff5f6", size = 10),
        axis.ticks = element_blank()) +
  labs(title = "<br><span style='color:#8d75d7'>You</span>, <span style='color:#75d775'>Me</span> & <span style='color:#75cad7'>Us</span>: Popularity and Longevity in the Charts",
       subtitle = "<br>Each bar represents a song, colour-coded according to whether its title makes<br>
       reference, from the performer's perspective, to <span style='color:#75d775'>me</span>, <span style='color:#8d75d7'>you</span>, <span style='color:#75cad7'>us</span>, or <span style='color:#6d7878'>anything else</span>. 
       <br>The longer the bar, the more weeks it spent in the Billboard Hot 100. 
       <br>The brigher the bar, the more popular it is on Spotify.",
       x = "Date of first entry into the charts",
       y = "Total number of weeks in the charts",
       caption = "Graphic: @cararthompson | #TidyTuesday\nSource: Sean Miller, Billboard.com and Spotify, via Data.World")


# Export to create making-of gif
ggsave(filename = file.path("../making-of/temp", paste0("202109_music-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", dpi = 400, width = 16, height = 8)

ggsave(filename = file.path("../plots/", paste0("202109_music.png")), 
       type = "cairo", dpi = 400, width = 16, height = 8)
