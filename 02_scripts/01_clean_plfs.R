
# cleaning environment ----

rm(list = ls())
gc()

# sourcing library ----

source("02_scripts/utils/00_library.R")
source("02_scripts/utils/01_read_plfs.R")
source("02_scripts/utils/02_standardise_plfs.R")
source("02_scripts/utils/03_recode_plfs.R")

# reading files ----

plfs_paths <- list(
  
  # calendar year file paths
  
  "2025" = "01_data/01_data_input/01_raw_data/plfs2025",
  "2024" = "01_data/01_data_input/01_raw_data/plfs2024",
  "2023" = "01_data/01_data_input/01_raw_data/plfs2023",
  "2022" = "01_data/01_data_input/01_raw_data/plfs2022",
  
  # agricultural year file paths
  
  "2023_24" = "01_data/01_data_input/01_raw_data/plfs2023_24",
  "2022_23" = "01_data/01_data_input/01_raw_data/plfs2022_23",
  "2021_22" = "01_data/01_data_input/01_raw_data/plfs2021_22",
  "2020_21" = "01_data/01_data_input/01_raw_data/plfs2020_21",
  "2019_20" = "01_data/01_data_input/01_raw_data/plfs2019_20",
  "2018_19" = "01_data/01_data_input/01_raw_data/plfs2018_19",
  "2017_18" = "01_data/01_data_input/01_raw_data/plfs2017_18"
  
) 

plfs_info <- list(
  
  # calendar year file info
  
  "2025" = data.frame(level = c("hhv1", "perv1"),
                      sheet = c(2, 3),
                      start_row = c(1, 1),
                      end_row = c(NA, NA),
                      format = c("fwf", "fwf")),
  
  "2024" = data.frame(level = c("hhv1", "perv1"),
                      sheet = c(3, 4),
                      start_row = c(2, 2),
                      end_row = c(NA, NA),
                      format = c("fwf", "fwf")),
  
  "2023" = data.frame(level = c("hhv1", "perv1"), # there are no txt files for 2023
                      sheet = c(1, 1),
                      start_row = c(3, 47),
                      end_row = c(41, 187),
                      format = c("dta", "dta")),
  
  "2022" = data.frame(level = c("hhv1", "perv1"), # there are no txt files for 2022
                      sheet = c(1, 1),
                      start_row = c(3, 47),
                      end_row = c(41, 187),
                      format = c("dta", "dta")),
  
  # agricultural year file info
  
  "2023_24" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(3, 4),
                         start_row = c(1, 1),
                         end_row = c(NA, NA),
                         format = c("tsv", "tsv")),
  
  "2022_23" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(1, 1),
                         start_row = c(3, 82),
                         end_row = c(40, 221),
                         format = c("tsv", "tsv")),
  
  "2021_22" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(1, 1),
                         start_row = c(3, 82),
                         end_row = c(40, 225),
                         format = c("tsv", "tsv")),
  
  "2020_21" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(1, 1),
                         start_row = c(3, 82),
                         end_row = c(40, 241),
                         format = c("tsv", "tsv")),
  
  "2019_20" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(1, 1),
                         start_row = c(3, 39),
                         end_row = c(35, 168),
                         format = c("tsv", "tsv")),
  
  "2018_19" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(1, 1),
                         start_row = c(3, 39),
                         end_row = c(35, 168),
                         format = c("tsv", "tsv")),
  
  "2017_18" = data.frame(level = c("hhv1", "perv1"),
                         sheet = c(1, 1),
                         start_row = c(3, 39),
                         end_row = c(35, 168),
                         format = c("tsv", "tsv"))
  
)

plfs_info <- bind_rows(plfs_info, .id = "year")

## reading layout ----

layout_list <- read_layout(plfs_info = plfs_info, plfs_paths = plfs_paths)
names(layout_list) <- paste0("plfs", plfs_info$year, "_", plfs_info$level)

## reading files ----

plfs_list <- read_plfs(plfs_info = plfs_info, plfs_paths = plfs_paths, layout_list = layout_list)
names(plfs_list) <- paste0("plfs", plfs_info$year, "_", plfs_info$level)

rm(layout_list, plfs_info, plfs_paths, read_layout, read_plfs)

# standardising columns ----

plfs_list <- imap(plfs_list, standardise_plfs)
plfs_list <- split(plfs_list, sub("_(hhv1|perv1)$", "", names(plfs_list)))

gc()

plfs_list <- map(plfs_list, function(df) {
  
  df <- reduce(rev(df), left_join, by = "chk_id")
  
  df <- df %>% 
    relocate(c(wave, chk_id, cpk_id), .before = 1)
  
})

# recoding the variables ----

plfs_list <- map(plfs_list, recode_plfs)

# saving files ----

iwalk(plfs_list, ~ {
  
  write_parquet(.x, file.path("01_data/01_data_input/02_clean_data", paste0(.y, ".parquet")))
  
})

# cleaning environment ----

rm(list = ls())
gc()
