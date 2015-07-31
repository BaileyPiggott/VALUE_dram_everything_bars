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

dram$define_colour <- factor(dram$define_colour, levels = c("a", "d", "b","e","c","f"))

# LEGEND LABELS -----------------------------------------------------------

a_1 <- "First Year"
a_2 <- paste0("n = ", dram[1, "N"])

b_1 <- "Second Year"
b_2 <- paste0("n = ", dram[2, "N"])

c_1 <- "Fourth Year"
c_2 <- paste0("n = ", dram[3, "N"])


legend_labels <- c(a_1, a_2, b_1, b_2, c_1, c_2)

# CREATE PLOT -------------------------------------------------------------
dodge <- position_dodge(width=0.5) # because bars and error bars will be different widths

ggplot(
  data = dram, 
  aes(x = X, y = Mean, fill = define_colour)
  )+
  geom_bar(stat = "identity",position = dodge, width = 0.5) + 
  geom_errorbar(
    aes(ymax = Mean +Std..Error, ymin = Mean - Std..Error),
    position = dodge,
    width = 0.25
    ) + 
  coord_cartesian(ylim = c(0, 4)) +
  scale_x_discrete(labels = criteria) +
  theme(
    axis.line = element_line("grey"), 
    panel.grid.major.y = element_line("grey"),
    panel.grid.major.x = element_blank(), # remove vertical white lines
    panel.background = element_rect("white"),
    plot.title = element_text(size = 15),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12), #size of x axis labels
    legend.position = c(0.75,0.88) # position legend on graph in top right corner
    ) +
  labs(title = "Summary of VALUE Learning Outcomes", 
       x = "Learning Outcome", 
       y = "Average Rubric Level"
       ) +
  scale_fill_manual(
    values = c('#99e6a2', '#99c4f6', '#33cc44', '#3388ee', '#1f7a29' ,'#1f528f'),
    name = "Legend",
    labels = legend_labels
    ) +
  annotate( # add labels for CLA mastery levels
    "text", 
    fontface = "bold", 
    size = 5,
    x = c(2.75, 6.75), 
    y = 2.75, 
    label = c( "Critical Thinking", "Written Communication"), 
    colour = c("#33CC44", "#3388EE")
    ) +
  guides(fill=guide_legend(ncol=3))# multiple columns in legend

  
  
  
  