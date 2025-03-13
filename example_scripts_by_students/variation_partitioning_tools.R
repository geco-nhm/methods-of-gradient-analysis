# Additional tools for variation partitioning

# added by Eva Lieungh

# Variable selection with Variable Inflation Factor ---------------------
#' If you have a lot of predictor variables, it might be a good idea to exclude some before doing forward selection of variables. Vifcor will correlate variables and exclude highly correlated variables.
library(usdm)
environment_vifs <- vifcor(
  ENV, # set of predictor variables
  th = 0.8, # threshold of correlation
  keep = NULL, # if wanted, list variables to keep no matter what 
  size = 5000, # subset size in case of big data (default 5000)
  method = "pearson" # 'pearson','kendall','spearman'
)

## define a "not in" function
`%nin%` <- Negate(`%in%`) 

## save the variables kept after running vifcor
variables_to_keep <-
  names(ENV)[names(ENV) %nin% environment_vifs@excluded]
message("variables kept after excluding the most correlated ones:")
print(variables_to_keep)
