
# load libraries
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(stringr)

# read in data
data <- read.xlsx("FINAL Sleep Content Coding (1).xlsx", sheet = 3)


# select colors to determine presence vs absence in heat map
palette <- c("Absent"  = "#f1fcff",
             "Generally Featured" = "#a2e9ff",
             "Specifically Featured" = "#53d2fc")

### DF WITH BROAD MEASURES
# transform data to long form and and columns for scale, symptom, and presence of symptom
long_data <- data %>% 
  pivot_longer(
    cols = -c(Symptom.Broad.Category, Symptom),
    names_to = "Scale",
    values_to = "Present"
  ) %>% 
  mutate(
    Scale   = factor(Scale, levels = c("SSR", "SDSC", "CSHQ", "CRSP", "PSQ", "PSQI", "CBCL", "BASC-3-PR", "PGBI", "CASI")),
    Symptom = factor(Symptom.Broad.Category, levels = rev(unique(Symptom.Broad.Category))),
    Present = factor(Present, levels = c(0,1,2), labels = c("Absent","Generally Featured", "Specifically Featured"))
  ) %>% na.omit(Scale)

# create an integer place holder for the number of scales
nx <- length(unique(long_data$Scale))

# set up data frame to feed to ggplot
symptom_ids <- data.frame(
  Symptom = levels(long_data$Symptom),
  x       = nx + 1,                              
  label   = paste0("s", rev(seq_along(levels(long_data$Symptom))))
)

# adding breaks for cluster bars  
breaks_after <- c(6, 14, 23, 32, 37)

# create heat map
heat_map <- ggplot(long_data,
                   aes(x = Scale, y = Symptom, fill = Present)) +
  geom_tile(color = "black", linewidth = 0.15, width = 1, height = 1) +
  scale_fill_manual(name = NULL, values = pallete) +
  labs(title = "Symptom Coverage Across All Scales",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x     = element_text(size = 10, angle = 70, hjust = 1),
    axis.text.y     = element_text(size = 8),
    panel.grid      = element_blank(),     # keep background clean
    legend.position = "right",
    plot.title      = element_text(hjust = 0.5)
  )+ 
  geom_hline(
    yintercept = breaks_after + 0.5, 
    colour      = "grey50",
    size        = 0.8
  )
  #geom_text(
    # data = symptom_ids, inherit.aes = FALSE,
    # aes(x = x, y = Symptom, label = label),
    # hjust = 0, size = 3.2
  #) +
  #coord_cartesian(clip = "off")

# show heat map
heat_map

ggsave("edited heat map.png", )

### DF WITH SPECIFIC MEASURES
# transform data to long form and and columns for scale, symptom, and presence of symptom
long_data2 <- data %>% 
  pivot_longer(
    cols = -c(Symptom.Broad.Category, Symptom, Symptom.Cluster),
    names_to = "Scale",
    values_to = "Present"
  ) %>% 
  mutate(
    Scale   = factor(Scale, levels = c("SSR", "SDSC", "CSHQ", "CRSP", "PSQ", "PSQI")),
    Symptom = factor(Symptom.Broad.Category, levels = rev(unique(Symptom.Broad.Category))),
    Present = factor(Present, levels = c(0,1,2), labels = c("Absent","Generally Featured", "Specifically Featured"))
  ) %>% na.omit(Scale)


# create an integer place holder for the number of scales
nx2 <- length(unique(long_data2$Scale))

# set up data frame to feed to ggplot
symptom_ids2 <- data.frame(
  Symptom = levels(long_data2$Symptom),
  x       = nx2 + 1,                              
  label   = paste0("s", rev(seq_along(levels(long_data2$Symptom))))
)

# create heat map
heat_map2 <- ggplot(long_data2,
                   aes(x = Scale, y = Symptom, fill = Present)) +
  geom_tile(color = "black", linewidth = 0.15) +
  scale_fill_manual(name = NULL, values = pallete) +
  labs(title = "Symptom Coverage Across All Scales",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y     = element_text(size = 6),
    panel.grid      = element_blank(),     # keep background clean
    legend.position = "right",
    plot.title      = element_text(hjust = 0.5)
  ) +
  geom_hline(
    yintercept = breaks_after + 0.5, 
    colour      = "grey50",
    size        = 0.8
  )+
  geom_tile(colour = "black", linewidth = 0.15)
  #+ geom_text(
   # data = symptom_ids2, inherit.aes = FALSE,
   # aes(x = x, y = Symptom, label = label),
   # hjust = 0, size = 3.2
  #) +
  #coord_cartesian(clip = "off")

# show heat map
heat_map2
