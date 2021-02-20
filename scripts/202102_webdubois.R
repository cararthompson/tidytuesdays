# DuBois Challenge
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(gganimate)

## Get the data ----
tt_data <- tt_load(2021, week = 8)
readme(tt_data)

## Build theme ----
theme_webdb <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = colour_values[7], colour = colour_values[7]),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = colour_values[7], colour = colour_values[7]),
          text = element_text(colour = "#373125", family = "Highway Gothic", size = 8), 
          plot.title = element_text(hjust = 0.5, size = 15),
          axis.text = element_blank(),
          legend.position = "none",
          plot.caption = element_text(color = "#373125", size = 8),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

## Set up spiral trigonometry
a <- 10
b <- 5
theta <- seq(7, 6*pi, length.out = max(furniture$`Houshold Value (Dollars)`/100))

## Set up loop to create data
spiral_values <- c(1, 1.4, 1.8, 2.2, 2.6, 3)
colour_values <- c("#d42a42", # red
                   "#d5c2b1",
                   "#f2b131", 
                   "#c09d80",
                   "#afb0c1",
                   "#ebb2a7", # pink
                   "#e8d8c7") # background
r <- list()
xs <- list()
ys <- list()
colour <- list()
text_val <- list()

furniture <- tt_data$furniture %>%
  arrange(desc(`Houshold Value (Dollars)`))

years <- furniture$Year
hvds <- furniture$`Houshold Value (Dollars)`

# Create plotting data
for(i in 1:length(years)){
  r[[paste0("r_", years[i])]] <- (a * spiral_values[[i]]) + 
    (b *spiral_values[[i]])*(theta * 1/spiral_values[[i]])
  ys[[paste0("ys_", years[i])]] <- r[[i]]*cos(theta)
  xs[[paste0("xs_", years[i])]] <- r[[i]]*-sin(theta)
  colour[[paste0("colour_", years[i])]] <- c(rep("no", length(theta) - ceiling(hvds[[i]]/100)), 
                                             rep(colour_values[[i]], ceiling(hvds[[i]]/100)))
  # Create text_val in reverse to this
  text_val[[paste0("text_val_", years[i])]] <- c(rep(hvds[[i]], sum(colour[[i]] == "no")),
                                                 round(seq(hvds[[i]], 0, length.out = sum(colour[[i]] != "no"))))
}

spiral_data <- as_tibble(c(r, xs, ys, colour, text_val, 
                           as_tibble_col(seq(length(theta), 1), 
                                         column_name = "increment")))

static_text_data <- tibble("year" = c(paste(years[1:4], "---- `` "),
                                      paste(years[5], "---- $ "),
                                      paste(years[6], "-------- $")),
                    "x_year" = rep(-47, 6),
                    "hjust_year" = rep(0, 6),
                    "y" = c(max(spiral_data$ys_1899),
                            max(spiral_data$ys_1895),
                            max(spiral_data$ys_1890),
                            max(spiral_data$ys_1885),
                            max(spiral_data$ys_1880),
                            max(spiral_data$ys_1875)),
                    "x_value" = rep(-4, 6),
                    "hjust_value" = rep(1, 6),
                    "static_text" = c(format(furniture$`Houshold Value (Dollars)`, big.mark = ",")))
# 
# spiral_data <- spiral_data %>%
#   left_join(static_text_data[c("year", "y")])
#                                       

## Plot it! ----
p <- ggplot(data = spiral_data) +
  coord_fixed() +
  scale_colour_manual(breaks = colour_values,
                      values = colour_values) +
  theme_webdb() +
  xlim(c(-125, 125)) 

for(i in length(years):1) {
  p <- p + 
    geom_point(aes_string(paste0("xs_", years[i]),
                          paste0("ys_", years[i]),
                          col = paste0("colour_", years[i])), 
               shape = 18, size = 3.48) 
}

static_spiral <- p + 
  geom_text(data = static_text_data,
            aes(x = -4, y = y, label = static_text),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_segment(data = NULL, aes(x = -1, xend = -1, y = 100, yend = 160), 
              size = 3, colour = colour_values[7]) +
  geom_text(data = static_text_data,
            aes(x = x_year, y = y, label = year),
                hjust = 0, size = 2.5, family = "Highway Gothic Wide") +
  labs(title = "\nASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES.",
       caption = "\n\nGraphic: @cararthompson | #TidyTuesday #DuBoisChallenge\nData: Anthony Starks, Allen Hillery and Sekou Tyler")


## Export static plot ----
ggsave(plot = static_spiral, filename = file.path("../plots", "202102_webdubois.png"), 
       dpi = 600, width = 6.15, height = 7.69)


anim_spiral <- p + 
  # this is layer 7
  geom_text(aes(x = -4, y = max(ys_1899), label = text_val_1899),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_text(aes(x = -4, y = max(ys_1895), label = text_val_1895),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_text(aes(x = -4, y = max(ys_1890), label = text_val_1890),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_text(aes(x = -4, y = max(ys_1885), label = text_val_1885),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_text(aes(x = -4, y = max(ys_1880), label = text_val_1880),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_text(aes(x = -4, y = max(ys_1875), label = text_val_1875),
            hjust = 1, size = 2.5, family = "Highway Gothic Wide") +
  geom_segment(data = NULL, aes(x = -1, xend = -1, y = 100, yend = 160), 
               size = 3, colour = colour_values[7]) +
  geom_text(data = static_text_data,
            aes(x = x_year, y = y, label = year),
            hjust = 0, size = 2.5, family = "Highway Gothic Wide") +
  labs(title = "\nASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES.",
       caption = "\n\nGraphic: @cararthompson | #TidyTuesday #DuBoisChallenge\nData: Anthony Starks, Allen Hillery and Sekou Tyler") +
  transition_components(increment) +
  enter_fade() +
  # Exclude the layers of text
  shadow_mark(past = T, exclude_layer = c(7:12)) 


## Export gif ----
anim_save(animate(plot = anim_spiral,  nframes = 2000), 
          filename = file.path("../plots", "202102_webdubois.gif"),
          res = 600, width = 6.15, height = 7.69, units = "in",
          end_pause = 300, rewind = F, fps = 100)
# I the cropped and modified the speed of the gif with an online tool
# because I couldn't get the frame rate needed through gganimate
# and there was white space around the gif despite the size being the same
# as the static image.


