# Programming languages

library(tidyverse)


# Data prep
coding <- tidytuesdayR::tt_load(x = "2023-03-21")
tidytuesdayR::readme(coding)


select_languages <- coding$languages %>%
  mutate(longevity = last_activity - appeared,
         still_around = case_when(last_activity > "2021" ~ 1,
                                  last_activity < "2022" ~ 0)) %>%
  filter(!is.na(line_comment_token)) %>%
  top_n(150, wt = number_of_users) %>%
  group_by(is_open_source) %>%
  mutate(rank = rank(-number_of_users, ties.method = "min"),
         open_closed = case_when(is_open_source == "TRUE" ~ "open source",
                                 is_open_source == "FALSE" ~ "closed source",
                                 is.na(is_open_source) ~ "unspecified"))

# Plot
ggplot(select_languages) +
  geom_text(data = filter(select_languages,
                          still_around == 1,
                          rank > 20),
            aes(x = 1970,
                y = title,
                alpha = still_around,
                colour = open_closed,
                label = paste(title, line_comment_token)),
            hjust = 0,
            family = "Consolas",
            size = 2) +
  geom_segment(aes(x = appeared, xend = last_activity,
                   y = title, yend = title,
                   colour = open_closed,
                   alpha = still_around)) +
  geom_point(aes(x = appeared,
                 y = title,
                 size = number_of_users,
                 colour = open_closed,
                 alpha = still_around),
             shape = 21,
             fill = "#3d4037") +
  geom_point(data = filter(select_languages,
                           still_around == 0),
             aes(x = last_activity,
                 y = title,
                 colour = open_closed,
                 size = number_of_users,
                 alpha = still_around)) +
  geom_text(data = filter(select_languages,
                          still_around == 1,
                          rank < 21),
            aes(x = appeared,
                y = title,
                size = number_of_users/10,
                label = line_comment_token,
                colour = open_closed),
            hjust = 0.5,
            family = "Consolas",
            fontface = "bold") +
  geom_text(data = filter(select_languages,
                          still_around == 0),
            aes(x = 1970,
                y = title,
                label = paste(line_comment_token, title),
                colour = open_closed),
            hjust = 1,
            family = "Consolas",
            size = 2,
            alpha = 0.6) +
  geom_text(data = top_n(select_languages,
                         n = 20,
                         wt = number_of_users),
            aes(x = 2023, y = title,
                label = paste0(title, " ", line_comment_token, " #", rank, ifelse(rank == 1, 
                                                                                  paste0(" | ", open_closed),
                                                                                  "")),
                colour = open_closed),
            family = "Consolas",
            hjust = 0,
            size = 2) +
  scale_alpha(range = c(0.3, 0.8)) +
  #  scale_y_reverse() + doesn't work on non numeric data
  scale_y_discrete(limits = rev, expand = c(0.05, 0.05)) +
  scale_size(range = c(1, 10)) +
  scale_x_continuous(limits = c(1964, 2033),
                     breaks = c(1980, 2000, 2020)) +
  scale_colour_manual(values = c("unspecified" = "#b2aba5", 
                                 "closed source" = "#dd7a57", 
                                 "open source" = "#92ac8f")) +
  labs(title = "#NoComment",
       caption = "\n \n____________________\n 
Source: https://pldb.com 
Top 150 languages (n users) with comment token 
 
Data visualisation: @cararthompson
#TidyTuesday | Pinch of salt required
 
www.cararthompson.com\n____________________") +
  theme_minimal() +
  theme(text = element_text(family = "Consolas", colour = "#27211c"),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "#959e8d", fill = "#3d4037", size = 2),
        plot.background = element_rect(fill = "#d7c4a6", colour = "#e9e6df", size = 6),
        panel.grid = element_line(colour = alpha("#9ca393", 0.1)),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.caption = element_text(hjust = 0.5, colour = "#27211c",
                                    size = 8),
        plot.title = element_text(family = "Gugi",
                                  colour = "#6d706b",
                                  hjust = 1,
                                  margin = margin(0, 0, 0.5, 0, "cm")))


# Exports for Making of
ggsave(filename = file.path(here::here("making-of/temp", 
                                       paste0("202203_coding-", 
                                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))), 
       dpi = 400, width = 9, height = 11)

# Export final plot
ggsave(filename = here::here("plots", "202303_coding.png"), 
       dpi = 400, width = 9, height = 11)
