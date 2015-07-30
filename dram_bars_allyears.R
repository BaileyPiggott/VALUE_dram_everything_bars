# drama bar graph of all VALUE criteria, with dodged bars for each year

# load libraries
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)


# SET UP DATA FRAME -------------------------------------------------------

dram = read.csv("VALUE_dram_means.csv")
colnames(dram)[2] <- "year"
dram$year <- as.factor(as.character(dram$year))# convert year column to factor
criteria <- c("Explanation\nof Issues",  "Influence of\nContext and\nAssumptions", "Student's\nPosition", "Conclusions", "Context and\nPurpose for\nWriting", "Content\nDevelopment", "Genre and\nDisciplinary\nConventions",  "Syntax and\nMechanics")

define_colours <- data.frame(c('a','b','c','a','b','c', 'a','b','c','a','b','c','d','e','f','d','e','f','d','e','f','d','e','f')) #a = lightgreen, b = green, c= darkgreen, d = lightblue, e=blue, f=darkblue
colnames(define_colours) <- 'define_colour'

dram <- bind_cols(dram, define_colours)

# CREATE PLOT -------------------------------------------------------------

ggplot(
  data = dram, 
  aes(x = X, y = Mean, fill = define_colour)
  )+
  geom_bar(stat = "identity",position = "dodge", width = 0.5) + 
  coord_cartesian(ylim = c(0, 4)) +
  scale_x_discrete(labels = criteria) +
  theme(
    axis.line = element_line("grey"), 
    panel.grid.major.y = element_line("grey"),
    panel.grid.major.x = element_blank(), # remove vertical white lines
    panel.background = element_rect("white"),
    plot.title = element_text(size = 15),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
    ) +
  labs(title = "Summary of VALUE Learning Outcomes", 
       x = "Learning Outcome", 
       y = "Average Rubric Level"
       ) +
  scale_fill_manual(
    values = c('#99e6a2', '#33cc44', '#1f7a29', '#99c4f6', '#3388ee', '#1f528f')
    )
  
  
  
  