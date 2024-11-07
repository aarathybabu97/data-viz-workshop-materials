###############################################################
# Project:  National Health Survey 2022 Analysis
# Purpose:  Plot chronic conditions over time
# Inputs:   solutions/3-clean-data/vis2.Rda
# Outputs:  solutions/4-outputs/yyyy-mm-dd-chronic-conditions-2001-2022.png
# Author:   Mark Hanly
###############################################################

###############
# Preparation #
###############

# Load libraries
library(dplyr)
library(ggplot2)
library(ggtext)
library(colorspace)

# Load cleaned data
load('solutions/3-clean-data/vis2.Rda')

# Update data for highlighted bars
df2Plot <- df2Clean |>
  mutate(
    highlight = case_when(
      condition=="Mental health" ~ 'a',
      condition=="Allergies" ~ 'b',
      .default = 'c'),
    condition = case_when(
      condition=="Allergies" ~ "Allergies<br>**2022:** 23.8%",
      condition=="Mental health" ~ "Mental health<br>**2022:** 26.2%",
      .default = condition),
    percent2 = case_when(
      highlight=='a' ~ percent + 1.5,
      highlight=='b' ~ percent - 1.5,
      highlight=='c' ~ percent)
  )

# Define colours
highlightPalette = c('#BC2C1A', '#377771', 'grey60')



#####################
# Plotting the data #
#####################

ggplot(
  data = df2Plot,
  aes(
    y=percent,
    x=date,
    group=condition,
    color=highlight,
    fill=highlight,
    linewidth=highlight)) +
  geom_line() + # Line
  geom_point(shape=21) + # Circles
  geom_richtext( # Label lines (geom_richtext allows for formatting)
    data = df2Plot |>
      group_by(condition) |>
      slice_tail(n=1) |> # Label last point only
      filter(! condition %in% c('Cancer', 'Osteoporosis', 'Heart, stroke & vascular')),
    aes(
      x=as.Date('2023-01-01'),
      y = percent2, # Nudge one label up and one label down
      label = condition,
      hjust=0),
      fill='transparent', label.colour = 'transparent') +
  scale_y_continuous(NULL,
                     limits = c(0,30),
                     breaks = c(0, 10, 20, 30),
                     labels = c('0', '10', '20', '30%')) +
  scale_colour_manual(values = highlightPalette) +
  scale_fill_manual(values = lighten(highlightPalette, 0.5)) +
  scale_linewidth_manual(values = c(1.2, 1.2, 0.5)) +
  scale_x_date(NULL,
               limits = c(as.Date('2000-01-01'), as.Date('2023-01-01')),
               date_labels = '%Y',
               breaks = as.Date(c('2000-01-01',
                                  '2005-01-01',
                                  '2010-01-01',
                                  '2015-01-01',
                                  '2020-01-01',
                                  '2023-01-01'))) +
  coord_cartesian(clip = 'off') + # this allows the line labels to expand outside the plot area
  labs(
    title = "<span style='color:#BC2C1A;'>**Mental health**</span> and
             <span style='color:#377771;'>**Allergies**</span> continue to increase",
    subtitle = "Proportion of population with a chronic condition by selected conditions, 2001-2022",
    caption = 'Source: National Health Survey, 2022'
  ) +
  theme(
    legend.position = 'none',
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t=10, b=10, r=80, l=10),
    plot.title = element_markdown(size=12),
    plot.subtitle = element_text(size=10, color = 'grey30', face = 'bold', vjust=2),
    plot.caption = element_text(size=8, color = 'grey60'),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size=8, color = 'grey30'),
    axis.text.y = element_text(hjust=0),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey90')
  )

# Save the plot as .png
ggsave(filename = paste0('solutions/4-outputs/', Sys.Date(),"-chronic-conditions-2001-2022.png"),
       width = 6,
       height = 4,
       units = 'in')

# ### JAGO ### #

df2Plot <- df2Clean |>
  mutate(
    highlight = case_when(
      condition=="Mental health" ~ 'a',
      condition=="Allergies" ~ 'b',
      .default = 'c'),
    label2 = ifelse(
      highlight=="c", condition,
      paste0(condition, " (", percent, "%)")
    ))
df2Plot|>
  ggplot(aes(
    y = percent,
    x = date,
    group = condition,
    color = highlight,
    fill = highlight,
    linewidth = highlight))+
  geom_line()+
  geom_point(shape=21)+
  geom_richtext( data = df2Plot |>
                   group_by(condition) |>
                   slice_tail(n=1) |> # Label last point only
                   filter(! condition %in% c('Cancer', 'Osteoporosis', 'Heart, stroke & vascular')),
                 aes(
                   x=as.Date('2023-01-01'),
                   y = percent, # Nudge one label up and one label down
                   label = label2,
                   size=2,
                   hjust=0),
                 fill='transparent', label.colour = 'transparent')+
  scale_color_manual(values = highlightPalette)+
  # Use the lighten() function
  scale_fill_manual(values = lighten(highlightPalette, 0.5))+
  theme(legend.position = "none",plot.margin = margin(t=10, b=10, r=80, l=10),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = 'grey90'),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),

        plot.title = element_markdown(size=12),
        plot.subtitle = element_text(size=10, color = 'grey30', face = 'bold', vjust=2),
        plot.caption = element_text(size=8, color = 'grey60')

        )+
  scale_linewidth_manual(values=c(1.5, 1.5, 0.5))+
  scale_x_date(NULL,
               limits = c(as.Date('2000-01-01'), as.Date('2023-01-01')),
               date_labels = '%Y',
               breaks = as.Date(c('2000-01-01',
                                  '2005-01-01',
                                  '2010-01-01',
                                  '2015-01-01',
                                  '2020-01-01',
                                  '2023-01-01')))+
  scale_y_continuous(name = NULL,
                     limits = c(0, 30),
                     labels = c('0', '10', '20', '30%'))+
  labs(
    title = "<span style='color:#BC2C1A;'>**Mental health**</span> and
             <span style='color:#377771;'>**Allergies**</span> continue to increase",
    subtitle = "Proportion of population with a chronic condition by selected conditions, 2001-2022",
    caption = 'Source: National Health Survey, 2022'
  )+
  coord_cartesian(clip = 'off')



