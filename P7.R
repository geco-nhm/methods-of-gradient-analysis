# Practical 7: Semivariograms

# Import libraries
library(geoR)
library(sp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

# Check and change working directory if necessary
getwd()
setwd("C:/Users/Tinkerbell/methods-of-gradient-analysis") 

# Importing data from BSKGEO08.xlsx (entire spreadsheet) as geoR object
MR.gd <-
  read.geodata(
    "clipboard",
    header = TRUE,
    coords.col = 1:2,
    data.col = 3:75)

# Alternatively, import data via csv:
d <- read.xlsx("P7_BSKGEO08.xlsx")
write.table(d, "d.csv")
MR.gd <-
  read.geodata(
    "d.csv",
    header = TRUE,
    coords.col = 1:2,
    data.col = 3:75)
  #' NB! Excel on Norwegian machines may use ; instead of , 
  #' and decimal comma instead of period. In that case, add these arguments:
  #' sep = ";", dec = ","

# Attach vaariables to names
attach(MR.gd)
names(MR.gd)

# Calculate semivariogram for a selection of MR variables --------------------
#' Inter-plot distances range from 2 m (minimum value) to 
#' slightly above 2000 m. We divide the range of distances into equal
#'  intervals on a logarithmic scale (4, 8, ... 2048 m).
#'  Thus, 2-4 m is distance class 1, 4-8 m is distance class 2 etc.

# the variog command needs break points between lag classes (bins)
breaks <- c(4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048)
vario.MR.gd <- variog(MR.gd, breaks = breaks, max.dist = 2048)

# The variogram object is a list of many elements
vario.MR.gd$u # Gives the midpoint of each bin
vario.MR.gd$v  # Gives semivariances for each variable in each bin
vario.MR.gd$n # The number of observation pairs in each bin
vario.MR.gd$uvec # Bin mid-points 
vario.MR.gd$var.mark # Sample variance for each variable

#' We calculate an envelope around the semivariances with the 
#' command variog.mc.env, and find max and min values 
#' (lower and upper confidence limits) based upon s (here 999) 
#' permutations. The variogram for variable 19 
#' (Distance to the WaterTable):
sel_var <- "WatTab50"
variable_index <- which(names(MR.gd)$data == sel_var)

vario.19WatTab50.env <-
  variog.mc.env(
    MR.gd,
    coords = MR.gd$coords,
    data = MR.gd$data[, variable_index],
    vario.MR.gd,
    nsim = 999
  )

vario.19WatTab50.env$u
vario.19WatTab50.env$v.lower
vario.19WatTab50.env$v.upper

# Note that the envelope command does not provide standardised envelope values
# We standardise the variogram by division with var.mark throughout

# Compute standardized semivariance and standardized confidence limits 
standardized_semivariance <- 
  vario.MR.gd$v[, variable_index] / vario.MR.gd$var.mark[variable_index]

upper_limit <- 
  vario.19WatTab50.env$v.upper / vario.MR.gd$var.mark[variable_index]

lower_limit <- 
  vario.19WatTab50.env$v.lower / vario.MR.gd$var.mark[variable_index]

# Create data frame with plot information
variogram_data <- data.frame(
  standardized_semivariance,
  upper_limit,
  lower_limit,
  breaks
)

# Create baseline plot
baseline_plot <- ggplot() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Plot semivariance
baseline_plot + 
  geom_line(data = variogram_data, 
            aes(y = standardized_semivariance, x = breaks)) +
  geom_point(data = variogram_data, 
             aes(y = standardized_semivariance, x = breaks), size = 2) +
  geom_line(data = variogram_data, 
            aes(y = upper_limit, x = breaks), 
            linetype = "dotted", color = "red") +
  geom_line(data = variogram_data, 
            aes(y = lower_limit, x = breaks), 
            linetype = "dotted", color = "red") +
  labs(x = "Distance", 
       y = paste0("Standardised semivariance (", sel_var, ", upper limit)"))


# This semivariogram has an inappropriate scaling of the x axis
vario.MR.gd$u

# We therefore convert the scale of lag distances to a 2-log scale
distlog2 <-
  c(1,
    log2(6),
    log2(12),
    log2(24),
    log2(48),
    log2(96),
    log2(192),
    log2(384),
    log2(768),
    log2(1536))
distlog2

# Create column for the transformed scale
variogram_data$distlog2 <- distlog2

# Creating a new plot
baseline_plot + 
  geom_line(data = variogram_data, 
            aes(y = standardized_semivariance, x = distlog2)) +
  geom_point(data = variogram_data, 
             aes(y = standardized_semivariance, x = distlog2), size = 2) +
  geom_line(data = variogram_data, 
            aes(y = upper_limit, x = distlog2), 
            linetype = "dotted", color = "red") +
  geom_line(data = variogram_data, 
            aes(y = lower_limit, x = distlog2), 
            linetype = "dotted", color = "red") +
  # approximate limit between within and between swamp forest scales:
  geom_vline(xintercept = 6, linetype = "dotted") + 
  # line showing semivariance = sample variance:
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(x = "Distance (log2 scale)", 
       y = paste0("Standardised semivariance (", sel_var, ")"))

# We repeat the process for a new variable, HumusN
sel_var <- "HumusN"
variable_index <- 
  which(names(MR.gd)$data == sel_var)

standardized_semivariance <- 
  vario.MR.gd$v[, variable_index] / vario.MR.gd$var.mark[variable_index]
upper_limit <- 
  vario.19WatTab50.env$v.upper / vario.MR.gd$var.mark[variable_index]
lower_limit <- 
  vario.19WatTab50.env$v.lower / vario.MR.gd$var.mark[variable_index]

# Create data frame with plot information
variogram_data <- data.frame(
  standardized_semivariance,
  upper_limit,
  lower_limit,
  distlog2)

# Creating a new plot
baseline_plot + 
  geom_line(data = variogram_data, 
            aes(y = standardized_semivariance, x = distlog2)) +
  geom_point(data = variogram_data, 
             aes(y = standardized_semivariance, x = distlog2), size = 2) +
  geom_line(data = variogram_data, 
            aes(y = upper_limit, x = distlog2), 
            linetype = "dotted", color = "red") +
  geom_line(data = variogram_data, 
            aes(y = lower_limit, x = distlog2), 
            linetype = "dotted", color = "red") +
  # approximate limit between within and between swamp forest scales:
  geom_vline(xintercept = 6, linetype = "dotted") + 
  # line showing semivariance = sample variance:
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(x = "Distance (log2 scale)", 
       y = paste0("Standardised semivariance (", sel_var, ")"))

# Optional: plot several variables at the same time --------------------------
# Inspect variables 
variable_names <- names(MR.gd)$data
variable_names

# Select multiple variables to plot
selected_variables <- c("WatTab50", "HumusN")  

# Prepare data for multiple variables
variogram_data_list <- 
  lapply(selected_variables, function(sel_var) {
  variable_index <- which(names(MR.gd)$data == sel_var)
  data.frame(
    standardized_semivariance = vario.MR.gd$v[, variable_index] / vario.MR.gd$var.mark[variable_index],
    upper_limit = vario.19WatTab50.env$v.upper / vario.MR.gd$var.mark[variable_index],
    lower_limit = vario.19WatTab50.env$v.lower / vario.MR.gd$var.mark[variable_index],
    distlog2 = distlog2,
    variable = sel_var
  )
})

# Combine all variogram data
variogram_data <- bind_rows(variogram_data_list)

# Create the faceted plot
baseline_plot +
  geom_line(data = variogram_data, 
            aes(y = standardized_semivariance, x = distlog2)) +
  geom_point(data = variogram_data,
             aes(y = standardized_semivariance, x = distlog2),
             size = 2) +
  geom_line(data = variogram_data,
            aes(y = upper_limit, x = distlog2),
            linetype = "dotted", color = "red") +
  geom_line(data = variogram_data,
            aes(y = lower_limit, x = distlog2),
            linetype = "dotted",
            color = "red") +
  geom_vline(xintercept = 6, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  facet_wrap( ~ variable, scales = "free_y") +
  labs(x = "Distance (log2 scale)", 
       y = paste0("Standardised semivariance"))

