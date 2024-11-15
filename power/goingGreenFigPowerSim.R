### Title: CRESP Going green to be seen recplication Figure 1
### Author: Michal Folwarczny

# clean workspace
rm(list = ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages
library(tidyverse)
library(viridis)
library(colorspace)
library(gghalves)
library(ggthemes)
library(skimr)
library(Superpower)

# load data
df <- read.csv('goingGreenReplicationData.txt')

# create green preference index
df$greenScore <- rowSums(apply(df[, 9:11], 2, str_count, 'Yes'))

### create Figure 1
g1 <- ggplot(df, aes(x = condition, y = greenScore))+
  scale_color_viridis(discrete = T,
                      option = 'viridis',
                      begin = .4,
                      end = .8)+
  scale_fill_viridis(discrete = T,
                     option = 'viridis',
                     begin = .4,
                     end = .8)+
  geom_point(
  aes(color = condition),
  position = position_jitter(width = .1),
  size = .55,
  alpha = .4)+
  stat_summary(fun = mean,
               geom = "point", 
               shape = 21, 
               size = 6,
               col = "black",
               fill = "yellow")+
  geom_half_boxplot(aes(fill = condition),
                    alpha = .4,
                    side = 'r',
                    outlier.shape = NA,
                    center = T,
                    position = position_nudge(x = .15),
                    errorbar.draw = F,
                    width = .2)+
  geom_half_violin(aes(fill = condition),
                   alpha = .4,
                   side = 'r',
                   bw = .3,
                   position = position_nudge(x = .3))+
  geom_rangeframe()+
  theme_tufte()+
  scale_x_discrete(labels = c('CONTROL STORY', 'NO STORY', 'STATUS STORY'))+
  xlab("CONDITION")+
  ylab("GREEN SCORE")+
  theme(
    aspect.ratio = .75,
    legend.position = 'none',
    text = element_text(size = 24, family = "sans"),
    axis.title = element_text(size = 20, face = 'bold')
  )+
  coord_flip()
g1

# Figure note:
# The rain cloud diagram shows the distribution of responses under different conditions. Yellow dots represent mean values surrounded by individual data points with jitter. Vertical lines in the centers of the boxplots indicate the medians. Shaded areas around them correspond to interquartile ranges, and whiskers encompass 1.5 times the interquartile ranges. The shaded areas above the boxplots indicate the density of the data.


ggsave(g1, filename = 'fig1.png', units = 'cm',
       height = 20, width = 28, dpi = 300)


### power analysis with the Superpower package

# merge the two control conditions for power analysis
df$condition[df$condition == 'No story'] <- 'Control'
df$condition[df$condition == 'Control story'] <- 'Control'

# the total number of respondents categorized as either liberal or conservative
table(df$LiborCon=='Liberal' | df$LiborCon=='Conservative')

# cell sizes
df %>%
  select(LiborCon, condition) %>%
  filter(LiborCon=='Liberal' | LiborCon=='Conservative') %>% 
  table()

# sd
round(sd(df$greenScore[df$LiborCon=='Liberal' | df$LiborCon=='Conservative']), 2)

# means and SDs for each condition and politial orientation
round(mean(df$greenScore[df$condition=='Control' & df$LiborCon=='Liberal']), 2)
round(mean(df$greenScore[df$condition=='Control' & df$LiborCon=='Conservative']), 2)
round(mean(df$greenScore[df$condition=='Status' & df$LiborCon=='Liberal']), 2)
round(mean(df$greenScore[df$condition=='Status' & df$LiborCon=='Conservative']), 2)

# power analysis with Superpower
design  <-  ANOVA_design(
  design = "2b*2b",
  n = round(739/4, 0), # our cell size assuming we had EQUAL cell sizes - a very non-conservative approach (because we had between 81 and 322 participants per cell with the total sample of 739)
  mu = c(1.90, 1.38, 1.95, 1.15), # means for each cell (condition x political orientation)
  sd = 1.02) 

ANOVA_exact(design, alpha_level = 0.05)

# plot power simulation
plot_power(design,
           alpha_level = 0.05,
           desired_power = 80,
           min_n = 100,
           max_n = 500)$plot_ANOVA

# 418 participants per cell needed for .80 power to detect this interaction, assuming conventional alpha levels of .05