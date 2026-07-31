######################
#Packages to install
#####################
options(scipen=999)

if(!require(pacman)) install.packages("pacman")
p_load("tidyverse","glue","marginaleffects","gtsummary",
       "fixest","texreg",
       "corrplot","sf",
       "gstat", #variogram
       "scales", #rescale in covariate adjustments
       "tune", #ggplot scale axis
       #"maps", #get admin zones
       #"choroplethrAdmin1", #alternative try to get admin zones
       "rnaturalearth", #for maps
       "ggpubr", #for ggarrange
       "DescTools", #Winsorize function,
       "tictoc", #time measure
       "viridis", #colors for ggplot
       "gtools", #quantcut
       "stars", #spatial files
       "grid", #textgrob in annotate for ggarrange
       "units" , #drop_units in st_area
       #"raster" #for reading .tif and other spatial operations
       "dplyr",
       "arrow",    # parquet read/write for memory-efficient chunked data
       "future",   # parallel backend (multisession workers)
       "furrr",    # parallel map over chunk files
       "data.table", # fast in-place mutation in simulate_10_dt inner loop
       "dtplyr"     # data.table backend for dplyr syntax (used in Section 9 opp cost calc)
       )
       
       
       
       
      # "jtools","texreg","parallel","arrow","sjmisc",
      # "fixest","gstat","marginaleffects","sp","spacetime","xts","ggpubr","sf",
      # "MuMIn","sjPlot","effectsize","modelsummary","lobstr","scales",
      # "gtools","rlist","gtsummary","lfe")


######################
# Shared figure palette
######################
# Single source of truth for the paper's figure accent colors, so all figures share one
# consistent theme. We keep the original bright opt-out gold and non-additional teal, with
# a complementary purple for leakage.
pal_optout  <- "gold"     # opt-out emissions                 (bright gold, #FFD700)
pal_nonadd  <- "cyan4"    # non-additional emissions          (bright teal, #008B8B)
pal_leakage <- "#7570b3"  # leakage / price-induced emissions (purple, complements gold + teal)

