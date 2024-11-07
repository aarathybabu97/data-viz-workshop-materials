###############################################################
# Project: Data visualization
# Purpose: Plot self-assessed health status by states and territories
# Inputs: exercises/3-clean-data/vis1.Rda
# Outputs:
# Author: Aarathy Babu
###############################################################

##################
# Load libraries #
##################

## Load libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)


#########################
# Load the cleaned data #
#########################

# Load cleaned data
load('exercises/3-clean-data/vis1.Rda')

df1Clean

###############################
# Preparation before plotting #
###############################

ggplot(
  data=df1Clean,
  aes(x = percent, y = state, fill=status)) +
  geom_col()

# Create variable indicating % very good or excellent health
df1Plot <- df1Clean |>
  group_by(state) |>
  slice(4:5) |>
  summarise(totalHealthy = sum(percent)) |>
  left_join(df1Clean, by = 'state')

#####################
# Plotting the data #
#####################
library(forcats)

ggplot(
  data=df1Plot,
  aes(x = percent,
      y = fct_reorder(state, totalHealthy),
      fill=status)) +
  geom_col()

#reverse the pct

ggplot(
  data=df1Plot,
  aes(x = percent,
      y = fct_reorder(state, totalHealthy),
      fill=status)) +
  geom_col(position = position_stack(reverse = TRUE))


#display palette

brewer.pal(name='RdYlGn', n=5)
display.brewer.pal(name='RdYlGn', n=5)

# Custom bar colours
barCols <- RColorBrewer::brewer.pal(5, 'RdYlGn')

# Customise colours
ggplot(
  data=df1Plot,
  aes(x = percent,
      y = fct_reorder(state, totalHealthy),
      fill=status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = NULL,
                    values = barCols)+
  #get rid of axis
  scale_y_discrete(NULL) +
  scale_x_continuous(NULL)+

  labs(
    title = 'Victorians most likely to rate their health as Excellent or Very Good',
    subtitle = 'Self-assessed health status by states and territories',
    caption = 'Source: National Health Survey, 2022')+
  theme(
    plot.title.position = 'plot',
    panel.background = element_blank(),

    axis.ticks.y = element_blank()
  )

# White spacing between colours

# Move this to the top!
library(colorspace)

# Custom colors for bar labels
label_text_colors <- c('transparent',
                       darken(barCols[2], 0.4),
                       darken(barCols[3], 0.4),
                       darken(barCols[4], 0.4),
                       lighten(barCols[5], 0.6))


ggplot(
  data=df1Plot,
  aes(x = percent,
      y = fct_reorder(state, totalHealthy),
      fill=status,
      color = status,
      label = paste0(format(percent, nsmall=1), '%'))) +
  geom_col(position = position_stack(reverse = TRUE),
           color = "white") +
  geom_text(position = position_stack(reverse = TRUE),
            size = 8/.pt,
            hjust = 1.1,
            show.legend = FALSE
           ) +
  scale_fill_manual(name = NULL,
                    values = barCols) +
  scale_y_discrete(NULL) +
  scale_x_continuous(NULL) +
  scale_colour_manual(name = NULL, values = label_text_colors) +
  labs(
    title = 'Victorians most likely to rate their health as <span style="color: #1A9641;">**Excellent**</span> or <span style="color: #A6D96A;">**Very Good**</span>',
    subtitle = 'Self-assessed health status by states and territories',
    caption = 'Source: National Health Survey, 2022') +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(size=14, vjust=2),
    plot.subtitle = element_text(size=10, color = 'grey30', face = 'bold'),
    plot.caption = element_text(size=8, color = 'grey60'),
    panel.background = element_blank(),
    axis.ticks.y = element_blank()
  )

#adjust colors
library(colorspace)

lighten("red", 0.2)

# Data frame for legend
legendDf <- data.frame(
  y = rep(9.5, 5),
  x = 10 + 0:4 * 20,
  label = factor(healthStatus, levels = healthStatus)
)

label_text_colors2 <- c(lighten(barCols[1], 0.6),
                        darken(barCols[2], 0.4),
                        darken(barCols[3], 0.4),
                        darken(barCols[4], 0.4),
                        lighten(barCols[5], 0.6))

# Library to allow second colour mapping
library(ggnewscale)
ggplot() +
  geom_col(
    data = df1Plot,
    aes(x = percent,
        y = fct_reorder(state, totalHealthy),
        fill = status,
        color = status),
    position = position_stack(reverse = TRUE),
    color = "white") +
  geom_text(
    data = df1Plot,
    aes(x = percent,
        y = fct_reorder(state, totalHealthy),
        color = status,
        label = paste0(format(percent, nsmall=1), '%')),
    position = position_stack(reverse = TRUE),
    size = 8/.pt,
    hjust = 1.1,
    show.legend = FALSE) +
  scale_fill_manual(name = NULL, values = barCols) +
  scale_colour_manual(name = NULL, values = label_text_colors) +
  scale_y_discrete(NULL) +
  scale_x_continuous(NULL) +
  geom_tile( # The legend box
    data = legendDf,
    aes(x = x, y = y, fill = label),
    color = 'white', height = 0.65) +
  new_scale_colour() +
  geom_text( # The legend box
    data = legendDf,size = 10/.pt,
    aes(x = x, y = y, label = label, color = label)) +
  labs(
    title = 'Victorians most likely to rate their health as <span style="color: #1A9641;">**Excellent**</span> or <span style="color: #A6D96A;">**Very Good**</span>',
    subtitle = 'Self-assessed health status by states and territories',
    caption = 'Source: National Health Survey, 2022') +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(size=14, vjust=2),
    plot.subtitle = element_text(size=10, color = 'grey30', face = 'bold'),
    plot.caption = element_text(size=8, color = 'grey60'),
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'none'
  )

###################
# Save the output #
###################
