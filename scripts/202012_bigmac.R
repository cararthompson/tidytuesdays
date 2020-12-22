# Big Mac
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggtext)
library(gganimate)
library(emoGG)

# Get data ----
tt_data <- tidytuesdayR::tt_load(2020, 52)
readme(tt_data)

# Create theme ----
theme_bigmac <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#66000e", colour = "#66000e"),
          panel.grid = element_line(color = "#66000e"),
          panel.background = element_rect(fill = "#66000e", colour = "#66000e"),
          text = element_text(colour = "white", family = "Raleway Black"), 
          plot.title = element_text(hjust = 0.5, size = 24, family = "Raleway Black", 
                                    face = "bold", colour = "#fda929"),
          axis.text.x = element_text(color = "white", size = 12),
          axis.text.y = element_blank(),
          plot.caption = element_text(hjust = 0.5, size = 10),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.ticks = element_blank())
}

# Plot the data ---- 
tt_data$`big-mac`%>%
  # remove extreme points
  filter(name %in% c("Euro area", "Britain", "United States")) %>%
  mutate(name = factor(name, levels = c("Britain", "United States", "Euro area"))) %>%
  mutate(year = str_extract(date, "20[0-9]{2}")) %>%
  ggplot() +
  geom_point(aes(x = name, y = dollar_price, 
                 colour = name, size = dollar_ex),
             alpha = 0.6, show.legend = F) +
  geom_emoji(aes(x = name, y = dollar_price),
             emoji = "1f354") +  
  scale_colour_manual(values = c("#e77139", "#fda929", "#6b9d3e")) +
  scale_size(range = c(10, 30)) +
  geom_hline(data = filter(tt_data$`big-mac`, name == "United States"),
             aes(yintercept = dollar_price),
             linetype = "dotted", 
             colour = "#fda929") +
  geom_text(aes(y = dollar_price + 0.2, x = name,
                label = paste0(round(dollar_price, 2), "$")), lineheight = .8,
            family = "Raleway Black", fontface = "bold",
            colour = "white",
            size = 4) +
  geom_text(aes(y = 6.5 + 0.2, x = 2,
                label = year), lineheight = .8,
            family = "Raleway Black", fontface = "bold",
            colour = "#fda929",
            size = 6) +
  labs(title = "How much was a Big Mac worth?",
       subtitle = "\n
Each burger represents the USD price of Big Mac in the region  
named at the bottom of the graph. The bigger the halo around it,
the higher the exchange rate (so the lower the relative value  
of the Big Mac in of the region).",
       x = "Region",
       y = "Price in USD") +
  theme_bigmac() +
  labs(x = "",
       y = "",
       caption = "@cararthompson | #TidyTuesday | Source: The Economist") +
  transition_time(date) +
  ease_aes('cubic-in-out')
  
# Export gif ----
anim_save(filename = "../plots/202012_bigmac.gif")