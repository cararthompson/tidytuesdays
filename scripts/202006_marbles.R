## Marble races
## Load packages ####
library(tidyverse)
library(gganimate)
library(extrafont)

## Read in the data ####
marbles <- read.csv("../data/202006_marbles.csv",
                    header = T,
                    stringsAsFactors = F) %>%
  mutate(Date = lubridate::dmy(Date),
         Race = fct_reorder(as.factor(Race), Date)) %>%
  group_by(Race) %>%
  mutate(Rank = rank(Time..s., ties.method = "average")) %>%
  ungroup() %>%
  group_by(Marble.Name) %>%
  mutate(Cumul.Rank = cumsum(Rank - 15), # Reversing for visualisation, leaving 1 for slowest marble
    Reg.Rank = cumsum(Rank - 8),
    Mean.Reg.Rank = mean(Reg.Rank)) %>%
  ungroup() 

marblesInOrder <- marbles %>% 
  mutate(Marble.Name = fct_reorder(as.factor(Marble.Name), Mean.Reg.Rank))

## Plot it ####
### Set colours per marble ####
marbleColours <- rainbow(length(unique(marblesInOrder$Marble.Name))) 
names(marbleColours) <- levels(as.factor(marblesInOrder$Marble.Name))

### Create custom theme emphasising marble of interest ####
theme_marbletastic <- function() {
  theme_dark() %+replace%
    theme(plot.background = element_rect(fill = "black"),
          panel.grid = element_line(color = "grey10"),
          panel.background = element_rect(fill = "black", colour = NA),
          legend.background = element_rect(fill = "black"),
          plot.title = element_text(hjust = 0, size = 16),
          text = element_text(colour = marbleColours[["Prim"]],
                              family = "Impact"), 
          axis.text.x = element_blank(),
          axis.text.y = element_text(color = marbleColours[["Prim"]]),
          axis.ticks = element_blank(),
          legend.justification= "left",
          legend.key = element_rect(fill = "black"),
          plot.caption = element_text(size = 8)) 
}

### MetaRace Plot ####
metaRace <- ggplot(marbles) +
  geom_hline(yintercept = min(marbles$Cumul.Rank), 
             colour = "white", linetype = "dashed", size = 1.5) + 
  geom_point(aes(x = Marble.Name, y =  Cumul.Rank, fill = Marble.Name),
             colour = "grey15", shape = 21, alpha = 0.9, size = 3, stroke = 1) +
  geom_segment(aes(x=Marble.Name, xend = Marble.Name, 
                   y = max(Cumul.Rank), yend = Cumul.Rank, 
                   colour = Marble.Name), alpha = 0.3, size = 2) +
  scale_y_reverse() +
  coord_flip() +
  scale_fill_manual(values = marbleColours,
                    guide = F) +
  scale_colour_manual(values = marbleColours,
                      guide = F) +
  labs(title = "Prim, The Marvellous Marbula One Marble", 
       subtitle = "Winner of the meta race, look at him go!\n",
       x = "", 
       y = "Losing < --- Cumulative Finishing Place ---> Winning!\n\n", 
       caption =  "#TidyTuesdays | @cararthompson") +
  theme_marbletastic() +
  transition_states(Race, transition_length = 2, 
                    state_length = 4, wrap = F) +
  shadow_mark(alpha = 0.3) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save(metaRace, filename = "../plots/202006_marbles1.gif",
          width = 700, height = 500, end_pause = 8)

### MeanReg Plot ####
meanReg <- ggplot(marblesInOrder) +
  geom_hline(yintercept = mean(marbles$Reg.Rank), colour = marbleColours[["Prim"]], 
             linetype = "dashed", size = 1) + 
  geom_point(aes(x = Marble.Name, y = Mean.Reg.Rank, colour = Marble.Name), 
             colour = "grey30", shape = 124, alpha = 0.2, size = 4) +
  geom_point(aes(x = Marble.Name, y = Reg.Rank, fill = Marble.Name),
             shape = 21, alpha = 0.7, size = 4, stroke = 1) +
  geom_segment(aes(x=Marble.Name, xend = Marble.Name, 
                   y = Mean.Reg.Rank, yend = Reg.Rank, 
                   colour = Marble.Name), alpha = 0.4, size = 2) +
  scale_y_reverse() +
  coord_flip() +
  scale_fill_manual(values = marbleColours,
                    guide = F) +
  scale_colour_manual(values = marbleColours,
                      guide = F) +
  labs(title = "Prim, The Marvellous Marbula One Marble", 
       subtitle = "Always towards the front of the pack, he evades the regression to the mean!\n", 
       x = "",
       y = "Losing < --- Cumulative Place Relative To The Middle Of The Pack ---> Winning!\n\n", 
       caption =  "#TidyTuesdays | @cararthompson") +
  theme_marbletastic() +
  transition_states(Race, transition_length = 2, 
                    state_length = 4, wrap = F) +
  shadow_mark(alpha = 0.4) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

anim_save(meanReg, filename = "../plots/202006_marbles2.gif",
          width = 700, height = 500, end_pause = 8) 