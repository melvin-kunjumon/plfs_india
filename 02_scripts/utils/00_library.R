
# library ----

# required packages as strings

libs <- c("dplyr", "tidyr", "forcats", "ggplot2", "haven", "purrr", "stringr", "rlang", "labelled", 
          "data.table", "janitor", "readr", "openxlsx", "arrow",  "survey", "srvyr", "progress")

# checking if the required packages are already installed

installed_libs <- libs %in% rownames(installed.packages())

# installing the required packages that are not present

if(any(installed_libs == F)){
  
  install.packages(libs[!installed_libs])
  
}

# loading the all required packages

invisible(lapply(libs, library, character.only = T))

# removing package strings

rm(installed_libs, libs)
