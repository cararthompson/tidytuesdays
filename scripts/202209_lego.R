library(tidyverse)
library(extrafont)
library(ggtext)

# Get the data

colours <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')

set.seed(124)

full_set <- inventory_parts %>% 
  left_join(colours, by = c("color_id" = "id")) %>%
  left_join(inventories, by = c("inventory_id" = "id")) %>%
  left_join(sets %>% 
              rename(set_name = name) %>%
              select(set_num, set_name, year, theme_id), by = "set_num") %>%
  left_join(tibble(name = c(colours %>% pull("name")),
                   basic_name = c(colours %>% pull("name") %>% 
                                    stringr::word(., -1) %>% 
                                    gsub("Trans-|Black-|DBGray|-", "", .))) %>%
              left_join(tibble(basic_name = c(colours %>% pull("name") %>% 
                                                stringr::word(., -1) %>% 
                                                gsub("Trans-|Black-|DBGray|-", "", .) %>%
                                                unique()),
                               direction = sample(c("up", "down"), c(colours %>% 
                                                                       pull("name") %>% 
                                                                       stringr::word(., -1) %>% 
                                                                       gsub("Trans-|Black-|DBGray|-", "", .) %>%
                                                                       unique() %>%
                                                                       length()), 
                                                  replace = TRUE)))) %>%
  mutate(hex = paste0("#", rgb),
         # see https://www.alanzucconi.com/2015/09/30/colour-sorting/
         luminosity = sqrt(col2rgb(hex)[1, ] * 0.241 + col2rgb(hex)[2, ] * 0.691 + col2rgb(hex)[3, ] * 0.068)) %>%
  group_by(basic_name) %>%
  arrange(luminosity) %>%
  ungroup() %>%
  mutate(hex = factor(hex, levels = unique(hex)))

decades <- tibble(x = c(1955, 1965, 1975, 1985, 1995, 2005, 2015, 2025),
                  label = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"),
                  y = rep(0, 8),
                  hex = monochromeR::generate_palette("#0c1a2f", blend_colour = "#5b667a", n_colours = 8))


ggplot() +
  geom_bar(data = decades, 
           aes(x = x, y = 1.8*10^5, 
               fill = hex),
           stat = "identity",
           position = 'dodge', width = 10) +
  geom_bar(data = decades, 
           aes(x = x, y = -1.4*10^5, 
               fill = hex),
           stat = "identity",
           position = 'dodge', width = 10) +
  geom_bar(data = full_set %>% filter(direction == "up"),
           aes(x = year, y = quantity, fill = hex),
           stat = "identity") +
  geom_bar(data = full_set %>% filter(direction == "down"),
           aes(x = year, y = - quantity, 
               fill = hex),
           stat = "identity") +
  geom_textbox(data = decades,
               aes(x = x, y = 1.7*10^5,
                   label = label),
               family = "Dosis",
               colour = "#FFFFFF",
               box.colour = NA,
               size = 8,
               alpha = 0.4,
               hjust = 0.5, halign = 0.5,
               fill = NA) +
  scale_fill_identity() +
  # Inverted the axis by mistake but decided to keep it that way - it looked nicer!
  ylim(c(1.9*10^5, -1.5*10^5)) +
  xlim(c(1948, 2032)) +
  geom_textbox(aes(y = -1.1*10^5,
                   x = 1952,
                   label = "**COLOUREVOLUTION**"),
               halign = 0, hjust = 0,
               valign = 0, vjust = 0,
               fill = NA,
               box.colour = NA,
               width = unit(20, "lines"),
               size = 9,
               colour = "#FFFFFF",
            #   alpha = 0.8,
               family = "Poppins") +
  geom_textbox(aes(y = -1.1*10^5,
                   x = 1952,
                   label = "**The number of different colours of LEGO bricks has grown from 10 in 1949 to 65 in 2022.**<br><br>
                   The adoption of a greater range of colours over time is tied to the company's launch of story-driven themes, such as the 
                   <span style=\"color:#fe810e\">*StarWars*</span> and <span style=\"color:#BBE90B\">*Bionicle*</span> ranges in the late 1990s and early 2000s.
                   The emergence of turquoise pieces in 2012 can be tied to the launch of the <span style=\"color:#aeefec\">*LEGO Friends*</span> product range.<br><br>
                   Whatever we make of the choice of colours and product ranges, the growth in the number of unique sets produced over the last 20 years would suggest the shift to 
                   story-driven sets was a smart move.<br>
                   A diversity success story? Discuss."),
               halign = 0, hjust = 0,
               valign = 1, vjust = 1,
               fill = NA,
               lineheight = 1.3,
               box.colour = NA,
               width = unit(20, "lines"),
               size = 2.85,
               colour = "#FFFFFF",
               alpha = 0.8,
               family = "Poppins") +
  geom_textbox(aes(y = 1.55*10^5,
                   x = 2029,
                   label = "@cararthompson | #TidyTuesday<br>Sources: rebrickable, wikipedia\n"),
               halign = 1, hjust = 1,
               valign = 0.5, vjust = 0.5,
               fill = NA,
               box.colour = NA,
               width = unit(20, "lines"),
               size = 2,
               colour = "#FFFFFF",
               alpha = 0.6,
               family = "Poppins") +
  theme_void()

# Exports for Making of
ggsave(filename = file.path("making-of/temp", 
                            paste0("202209_lego-", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 9, height = 8, bg = "#ced1d5")

ggsave(filename = file.path("plots/", 
                            "202209_lego.png"), 
       dpi = 400, width = 9, height = 8, bg = "#ced1d5")



