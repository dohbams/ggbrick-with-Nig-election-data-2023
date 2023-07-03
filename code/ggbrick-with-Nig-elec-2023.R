# Load required libraries
library(janitor)  # For cleaning column names
library(tidyverse)  # For data manipulation and visualization
library(extrafont)  # For using custom fonts in plots
library(ggsci)  # For using scientific color palettes
library(Cairo)  # For high-quality image rendering
library(ggbrick)  # For creating brick plots

# Read the data from 'elec.csv' file
df <- read.csv('elec.csv', header = TRUE)

# Clean the column names
df <- clean_names(df)

# Reshape the data using pivot_longer
df2 <- df %>% pivot_longer(cols = 2:5, names_to = 'party', values_to = 'votes')

# Convert 'party' column values to uppercase
df2$party <- toupper(df2$party)

# Mutate a new column 'party_cols' using case_when
df2 <- df2 %>% mutate(party_cols = case_when(party == 'PDP' ~ 'red',
                                             party == 'APC' ~ 'green',
                                             party == 'LP' ~ 'cyan',
                                             party == 'NNPP' ~ 'violet'))

# Convert 'geopolitical_zone' column to character
df2$geopolitical_zone <- as.character(df2$geopolitical_zone)

# Create the base plot
g <-  ggplot() +
  geom_brick(df2, mapping = aes(x = reorder(party, votes), y = votes, fill = geopolitical_zone)) +
  coord_flip() +
  scale_fill_npg(alpha = 0.55, name = 'Geopolitical Zone',
                 guide = guide_legend(title.hjust = 0.5, title.position = 'top', nrow = 1)) +
  theme_classic() +
  labs(title = 'HOW DID NIGERIANS VOTE AMONG THE TOP 4 POLITICAL PARTIES?
       
       ', 
       caption = '@DOh_Bams | R {tidyverse, ggbrick} | Data: www.vanguardngr.com',
       x = 'Party
       
       ', y = "
       
       Votes") +
  theme(plot.caption = element_text(size = 10, hjust = 0.5, vjust = -15),
        plot.title = element_text(size = 28, face = 'bold', family = "Tw Cen MT Condensed Extra Bold", hjust = 0.5),
        text = element_text(family = "Tw Cen MT"),
        legend.position = "top",
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 18),
        axis.text = element_text(family = "Tw Cen MT", size = 14),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(2,4,2,2,'cm'),
        plot.background = element_rect(fill = '#fffffa'),
        panel.background = element_rect(fill = '#fffffa'),
        legend.background = element_rect(fill = '#fffffa'))


# Save the plot as PNG using Cairo
ggsave('vote.png', g, type = 'cairo', width = 15, height = 9, dpi = 1200)
