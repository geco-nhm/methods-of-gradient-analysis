# Import libraries
library(vegan)
library(ggplot2)
library(dplyr)
library(readxl)

# Automatic import
setwd("C:/Users/"[insert the path to your working directory here]) #Set working directory
gnmdstot <- read_xls("P6_OppkuvenGNMDS.xls", 
                     sheet = "BASIC") %>% 
  as.data.frame() # Importing spreadsheet BASIC from data file P6OppkuvenGNMDS.xls
sp.change <- read_xls("P6_OppkuvenGNMDS.xls", 
                      sheet = "Species change") %>% 
  as.data.frame() # Importing species data

# Or, via import from Clipboard
gnmdstot <- read.table("clipboard", header = TRUE)
sp.change <- read.table("clipboard", header = TRUE)

# Attach names to variables
attach(gnmdstot)
names(gnmdstot)

attach(sp.change)
names(sp.change)

year <- as.factor(Year)
year
stand <- as.factor(Stand)
stand

# Create column for plot ID
gnmdstot$plot <- rep(x = 1:100, times = 2)

# Convert the variable "Year" to a factor variable
gnmdstot$Year <- factor(gnmdstot$Year)

# Pair observations from the two years
gnmds_pairs <- gnmdstot %>%
  group_by(plot) %>%  # Group by plot to pair points
  arrange(Year) %>%  # Ensure year 1 comes first
  summarise(x1 = first(GNMDS1), y1 = first(GNMDS2), x2 = last(GNMDS1), y2 = last(GNMDS2))  # Coordinates

# Create a baseline plot for GNMDS
baseline_plot <- ggplot() + 
  coord_fixed(ratio = 1) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  
# Plot GNMDS (with plot ID labels)
baseline_plot + 
  geom_text(data = gnmdstot, 
            aes(x = GNMDS1, y = GNMDS2, 
                label = plot, color = Year), 
            size = 3)

# Plot GNMDS (with points and connective lines)
baseline_plot + 
  geom_point(data = gnmdstot, 
             aes(x = GNMDS1, y = GNMDS2, 
                 color = Year, size = Year)) +
  geom_segment(data = gnmds_pairs, 
               aes(x = x1, y = y1, 
                   xend = x2, yend = y2),
               arrow = arrow(length = unit(0.2, "cm"), 
                             type = "closed"),
               color = "black")

# Plot GNMDS (with text and connective lines)
baseline_plot + 
  geom_text(data = gnmdstot, 
            aes(x = GNMDS1, y = GNMDS2, 
                label = plot, color = Year), 
            size = 3) +
  geom_segment(data = gnmds_pairs, 
               aes(x = x1, y = y1, 
                   xend = x2, yend = y2),
               arrow = arrow(length = unit(0.2, "cm"), 
                             type = "closed"),
               color = "black")

# GNMDS scores (sites) from 97 and 05
GNMDS1.97 <- GNMDS1[1:100]
GNMDS2.97 <- GNMDS2[1:100]  
GNMDS1.05 <- GNMDS1[101:200]
GNMDS2.05 <- GNMDS2[101:200]

# How to find mean displacement for some of the axes/stands
mean(GNMDS1.97)
mean(GNMDS1.05)
mean(GNMDS1.97[1:25])
mean(GNMDS1.05[1:25])
# ...and so on

tot97 <- GNMDS2.97[1:100]
mean(tot97)
tot05 <- GNMDS2.05[1:100]
mean(tot05)
a97 <- GNMDS2.97[1:25]
mean(a97)
a05 <- GNMDS2.05[1:25]
mean(a05)
# ...and so on

# Explanations for x1, x2 etc. in this table (A-D are the different stands):
		
# 1997					
# 		Tot	A	B	C	D	
#               Pink    Red     Green	Blue	Turquoise
# GNMDS1	x1	0.7925	0.6611	0.6729	0.9188	0.9171	
# GNMDS2	y1	1.0920	1.2488	0.9987	1.1038	0.9889	

# 2005					
# GNMDS1	x2	0.7750	0.6421	0.7156	0.8514	0.8911	
# GNMDS2	y2	1.0975	1.2086	1.0049	1.1100	1.0353	

# Create data frame for displaying mean displacement from 1997 to 2005
displacement_data <- data.frame(
  # Mean displacement from 1997 to 2005:
  x1 = c(0.7925, 0.6611, 0.6729, 0.9188, 0.9171),
  x2 = c(0.7750, 0.6421, 0.7156, 0.8514, 0.8911),
  y1 = c(1.0920, 1.2488, 0.9987, 1.1038, 0.9889),
  y2 = c(1.0975, 1.2086, 1.0049, 1.1100, 1.0353),
  stand = factor(x = c("Tot", "A", "B", "C", "D"), levels = c("Tot", "A", "B", "C", "D")))

# Set colors
stand_colors <-
  c("Tot" = "blue",
    "A" = "red",
    "B" = "green",
    "C" = "purple",
    "D" = "orange")

# Plot showing mean displacement (try to change arrow parameters: length, angle, 
# "code, linetype and linewidth, you may even try to change the length of the
# x and y axes by xlim and ylim)
ggplot() +
  geom_point(data = displacement_data, 
             aes(x = x1, y = y1), 
             color = "black", size = 3) +
  geom_point(data = displacement_data, 
             aes(x = x2, y = y2), 
             color = "black", size = 3) +
  geom_segment(data = displacement_data, 
               aes(x = x1, y = y1, 
                   xend = x2, yend = y2, 
                   color = stand),
               arrow = arrow(length = unit(0.1, "inches"), 
                             type = "closed"), linewidth = 1) +
  geom_hline(yintercept = 1.0920, color = "black") +
  geom_vline(xintercept = 0.7925, color = "black") +
  labs(x = "GNMDS1", y = "GNMDS2") +
  scale_color_manual(name = "Stand", values = stand_colors, 
                     labels = c("Tot", "A", "B", "C", "D")) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# PCA on change in species abundance from 1997 to 2005
pca.sp <- rda(sp.change, scale = FALSE)
summary(pca.sp, scaling = 1)
pca.sp

# Create baseline plot:
pca_plot <- ggplot() + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  coord_fixed(ratio = 1) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# PCA ordination of species change matrix, site score vectors 
# Choose scaling = 1 for optimising fit between distances between plots in diagram and floristic dissimilarity
pca_sites <- scores(pca.sp, display = "sites", scaling = 1) %>% 
  as.data.frame()

# Allocating all sites names to "labels":
pca_sites$labels <- rownames(sp.change)
pca_sites$labels

# Plot site scores:
pca_plot + 
  geom_text(data = pca_sites, 
            aes(x = PC1, y = PC2, 
                label = labels), 
            size = 3)

# PCA ordination of species change matrix, species score vectors 
# Choose scaling = 2
pca_species <- scores(pca.sp, display = "species", scaling = 2) %>% 
  as.data.frame()

# Allocating all species names to "labels":
pca_species$labels <- names(sp.change)
pca_species$labels

# Plot species scores:
pca_plot + 
  geom_text(data = pca_species, 
            aes(x = PC1, y = PC2, 
                label = labels), 
            size = 3)

#### The absolute abundance change ####
sort(apply(sp.change, 2, function(x) sum(abs(x))))

# Exercise:
# Perform correlation tests of PCA-axes from species change against environmental 
# variables. Load the environmental variables and find 
# which of the variables are correlated with the PCA-axes?

