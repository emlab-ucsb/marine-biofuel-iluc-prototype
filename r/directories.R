######################
#directory paths
#####################

data_directory_base <-  ifelse(Sys.info()["nodename"] == "quebracho" | Sys.info()["nodename"] == "sequoia" | Sys.info()["nodename"] == "hpc-02"| Sys.info()["nodename"] == "hpc-01"| Sys.info()["nodename"] == "hpc-03"| Sys.info()["nodename"] == "hpc-05"| Sys.info()["nodename"] == "hpc-06"| Sys.info()["nodename"] == "hpc-16",
                               "/home/emlab",
                               # Otherwise, set the directory for local machines based on the OS
                               # If using Mac OS, the directory will be automatically set as follows
                               ifelse(Sys.info()["sysname"]=="Darwin",
                                      "/Users/Shared/nextcloud/emLab",
                                      # If using Windows, the directory will be automatically set as follows
                                      ifelse(Sys.info()["sysname"]=="Windows",
                                             "G:/Shared\ drives/nextcloud/emLab",
                                             # If using Linux, will need to manually modify the following directory path based on their user name
                                             # Replace your_username with your local machine user name
                                             #"/home/your_username/Nextcloud"
                                             "/Users/clatka/github/lbcs")))
#data_directory_base<-"/Users/clatka/github/"

data_directory <- glue::glue("{data_directory_base}/projects/current-projects/land-based-solutions/data")

nn_directory<- glue::glue("{data_directory_base}/projects/current-projects/land-based-solutions/neural-network-model/")
#nn_directory<- glue::glue("{data_directory_base}/data/nn/")
data_heterogeneity<- glue::glue("{data_directory_base}/projects/current-projects/land-based-solutions/heterogeneity-analysis/data")

paper_figures_directory<- glue::glue("{data_directory_base}/projects/current-projects/land-based-solutions/data/results/figures")
#data_directory <- ifelse(Sys.info()["sysname"]=="Windows","G:/Shared\ drives/emLab/Projects/current-projects/land-based-solutions/data",
#                         "/Users/clatka/Library/CloudStorage/GoogleDrive-clatka@ucsb.edu/Shared drives/emlab/projects/current-projects/land-based-solutions/data")


#figure_directory <- ifelse(Sys.info()["sysname"]=="Windows","G:/Shared\ drives/emLab/Projects/current-projects/land-based-solutions/results/figures",
#                           "/Users/clatka/Library/CloudStorage/GoogleDrive-clatka@ucsb.edu/Shared drives/emlab/projects/current-projects/land-based-solutions/data/results/figures")



#data_heterogeneity <- ifelse(Sys.info()["sysname"]=="Windows","G:/Shared\ drives/emLab/Projects/current-projects/land-based-solutions//heterogeneity-analysis/data",
#                          "/Users/clatka/Library/CloudStorage/GoogleDrive-clatka@ucsb.edu/Shared drives/emlab/projects/current-projects/land-based-solutions/heterogeneity-analysis/data")


#data_directory <- "/home/clatka/lbcs/data"
#data_heterogeneity <- "/home/clatka/lbcs/heterogeneity"


# data_directory <- "H:/Shared\ drives/emLab/Projects/current-projects/land-based-solutions/data"
# data_heterogeneity <- "H:/Shared\ drives/emLab/Projects/current-projects/land-based-solutions/heterogeneity-analysis/data"
# figure_direcotry <- "H:/Shared\ drives/emLab/Projects/current-projects/land-based-solutions/results/figures"