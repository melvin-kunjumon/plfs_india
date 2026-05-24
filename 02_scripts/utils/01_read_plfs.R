
read_layout <- function(plfs_info, plfs_paths) {
  
  layout_list <- pmap(plfs_info, function(year, level, sheet, start_row, end_row, format) {
    
    file_path <- file.path(plfs_paths[[year]], paste0("plfs", year, "_datalayout.xlsx"))
    
    df <- read.xlsx(file_path, sheet = sheet, startRow = start_row) %>% clean_names()
    
    if (!is.na(end_row)) {
      
      df <- df[1:(end_row - start_row), ]
      
    }
    
    df$field_length <- as.numeric(df$field_length)
    
    return(df)
    
  })
  
}

read_plfs <- function(plfs_info, plfs_paths, layout_list) {
  
  plfs_list <- pmap(plfs_info, function(year, level, sheet, start_row, end_row, format) {
    
    file_name <- paste0("plfs", year, "_", level)
    
    if (format != "dta") {
      
      file_path <- file.path(plfs_paths[[year]], paste0(file_name, ".txt"))
      
      if (format == "fwf") {
        
        layout <- layout_list[[file_name]]
        
        position <- fwf_widths(layout$field_length)
        
        col_names <- layout$field_name
        
        df <- read_fwf(file_path, col_positions = position, show_col_types = F)
        
        names(df) <- col_names
        
      } else {
        
        df <- read.delim(file_path, header = F, sep = "\t")
        
      }
      
    } else {
      
      file_path <- file.path(plfs_paths[[year]], paste0(file_name, ".dta"))
      
      df <- read_dta(file_path)
      
    }
    
    df <- df %>% clean_names()
    
    return(df)
    
  })
  
}
