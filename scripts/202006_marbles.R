## Marble races
## Load packages ####
library(tidyverse)
library(emojifont)

# names(marbles)

## Read in the data ####
marbles <- read.csv("../data/202006_marbles.csv",
                    header = T,
                    stringsAsFactors = F) 

hosts <- unique(subset(marbles, marbles$Host. == "Yes")$Marble.Name)

marbles <- marbles %>%
  mutate(Hosted = as.factor(ifelse(Marble.Name %in% hosts, 
                                   "Has hosted", 
                                   "Has never hosted"))) %>%
  
  group_by(Race) %>%
  mutate(Rank = rank(Time..s., ties.method = "average")) %>%
  mutate(Alpha.Name = rank(Marble.Name)) %>%
  ungroup() %>%
  group_by(Marble.Name) %>%
  mutate(Mean.Rank = mean(Rank), na.rm = T)

# Plot it
ggplot(subset(marbles, Marble.Name %in% hosts)) +
  # plotting line first, so dots are seen on top of it
  geom_hline(yintercept = 1.1, colour = "white", linetype = "dashed", size = 2) + 
  geom_jitter(aes(x = Marble.Name, y = Rank, fill = Marble.Name, colour = Host.),
              shape = 21, width = 0, height = 0.3, alpha = 0.5, size = 3, stroke = 1.5) +
  scale_y_reverse() +

  ylab("Finishing Place") +
  xlab("Marble Name") +
  coord_flip() +
  scale_fill_manual(values = rainbow(length(hosts)),
                    guide = F) +
  scale_colour_manual(labels = c("", "Host"), 
                      values = c("black", "white"))+
  labs(title = "How to psych up your marbles\n", 
       x = "Marble Name", y = "Finishing Place", color = "") +
  theme_dark() +
  ggtitle(label = "How to psych your marbles", 
          subtitle = "Don't host") +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white"),
      #  legend.title = element_text(size = 0),
        legend.text = element_text(color = "grey30"),
        axis.title  = element_text(color = "grey30"),
    #    legend.position = "top", 
        legend.justification= "left",
        legend.key = element_rect(fill = "black"),
        plot.caption = element_text(color = "white", size = 10)) 
  
  
ggplot(marbles) +
  geom_smooth(aes(x = Alpha.Name, y = Rank, colour = Hosted)) 


theme(plot.background = element_rect(fill = "black"),
#      panel.grid.major = element_line(color ="black"),
      panel.background = element_rect(fill = "black"),
      legend.background = element_rect(fill = "black"),
      legend.title = element_text(color = "white", size = 14),
      legend.text = element_text(color = "white", size = 14),
 #     legend.position = "top", 
      legend.justification= "left",
      legend.key = element_rect(fill = "black"),
      plot.caption = element_text(color = "white", size = 10)) 