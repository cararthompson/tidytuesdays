# Superbowl
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(gganimate)
library(ggimage)
library(ggtext)

## Get the data ----
tt_data <- tt_load(2021, week = 10)
# readme(tt_data)

## Build theme ----
theme_superbowl <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#166b22", colour = "#166b22"),
          panel.grid = element_line(colour = "white"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.background = element_rect(fill = "#166b22", colour = "#166b22"),
          text = element_text(colour = "white", family = "Highway Gothic", size = 12), 
          plot.title = element_text(hjust = 0.5, size = 30, colour = "#da0f14", family = "JACKPORT COLLEGE NCV"),
          plot.subtitle = element_text(hjust = 0.5, size = 25, colour = "#041aa8", family = "JACKPORT COLLEGE NCV"),
          axis.text = element_blank(),
          legend.position = "none",
          plot.caption = element_text(color = "white", size = 10))
}

brand_logos <- tibble(brand = sort(unique(tt_data$youtube$brand)),
                      image = c("../making-of/temp-logos/Bud-Light-logo-1.jpg",
                                "../making-of/temp-logos/Budweiser-Logo.png",
                                "../making-of/temp-logos/Coca-Cola-Logo.png",
                                "../making-of/temp-logos/Doritos-Logo.png",
                                "../making-of/temp-logos/E-Trade-Logo.png",
                                "../making-of/temp-logos/Hyundai-logo.jpg",
                                "../making-of/temp-logos/Kia-logo.jpg",
                                "../making-of/temp-logos/NFL-logo.png",
                                "../making-of/temp-logos/Pepsi-Logo-2003.jpg",
                                "../making-of/temp-logos/logo-toyota.png"))

## Manipulate data ----
sb <- tt_data$youtube %>%
  group_by(year, brand) %>%
  summarise(likes = mean(like_count, na.rm = T),
            celebrities = mean(celebrity, na.rm = T),
            animals = mean(animals, na.rm = T)) %>%
  ungroup() %>%
  left_join(brand_logos) %>%
  group_by(year) %>%
  mutate(max_likes = max(likes, na.rm = T))

max_likes <- sb %>%
  filter(likes == max_likes)


## Plot it ----
p <- ggplot() +
  geom_hline(yintercept = 1, colour = "white", size = 2) +
  geom_hline(yintercept = 0, colour = "white", size = 2) +
  geom_hline(yintercept = 0.42, colour = "white", linetype = 3, size = 1.5) +
  geom_hline(yintercept = 0.58, colour = "white", linetype = 3, size = 1.5) +
  geom_vline(xintercept = 1, colour = "white", size = 2) +
  geom_vline(xintercept = 0, colour = "white", size = 2) +
  geom_vline(xintercept = 0.5, colour = "white", size = 1.5) +
  geom_textbox(data = NULL, aes(0.5, 1.075),
               label = "The brands move around the pitch as they vary the proportion of their ads containing 
               celebrities and animals across the seasons. The brand holding the ball is the one with the highest mean
               number of likes per ad in that season.",
               colour = "white",
               box.colour = "#166b22",
               size = 5, fill = "#166b22",
               family = "Highway Gothic",
               width = 1, height = 0.225,
               hjust = 0.5, vjust = 0,
               halign = 0.5, valign = 0.5,
               show.legend = F) +
  ylim(c(NA, 1.3)) +
  scale_x_continuous(limits = c(0, 1), 
                     minor_breaks = seq(0, 1, .05)) +
  labs(title = "\nSuperbowl   Ads",
       subtitle = "Animals vs. Celebrities \n{closest_state}",
       caption = "Graphic: @cararthompson | #TidyTuesday | Data source: FiveThirtyEight",
       x = "Proportion of ads with animals",
       y = "Proportion of ads with celebrities") +
  geom_image(data = sb, 
             aes(x = animals, y = celebrities, image = image),
             height = 0.2) +
  geom_image(data = max_likes,
             aes(x = animals, y = celebrities,
                 image = image),
             height = 0.2) +  
  geom_image(data = max_likes, 
             aes(x = animals, y = celebrities + 0.05),
             image = "../making-of/temp-logos/ball.png",
             size = 0.05) +
  # so that winning logo is top layer 
  theme_superbowl() +
  transition_states(year, wrap = F, transition_length = 20, 
                    state_length = 20) +
  shadow_mark(max_frames = 15, alpha = 0.5,
              exclude_layer = c(1:9)) +
  shadow_trail(exclude_layer = c(1:11),
               distance = 1, max_frames = 20, 
               alpha = 0.5) +
  exit_fade() +
  ease_aes('cubic-in-out')


## Export to create making-of gif ----
ggsave(filename = file.path("../making-of/temp", 
                            paste0("202103_superbowl-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
       dpi = 400, width = 12, height = 12.5)

## Export final plot ----
anim_save(animate(p, nframes = 300),
          filename = file.path("../plots", "202103_superbowl.gif"),
          res = 300, width = 6, height = 6.25, units = "in",
          end_pause = 10, rewind = F, fps = 15)
