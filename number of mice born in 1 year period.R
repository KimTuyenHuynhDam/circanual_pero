library(openxlsx)
library(readxl)
library(tidyverse)
library(broom)
library(lubridate)
library(writexl)

# Function to replace spaces with underscores in column names
clean_column_names <- function(df) {
  names(df) <- gsub(" ", "", names(df))
  return(df)
}

pero = read_excel("Peromyscus.xlsx") %>% select(1:5)  %>%
  clean_column_names %>% filter(!is.na(Birthday)) 
matingcage  = read_excel("Mating Records.xlsx")  %>% select(1:5) %>%
  clean_column_names


all_stock= c("BW", "LL", "PO", "IS", "EP", "SM2")

#all_stock = c("EP")
#species = c("EP")





for ( species in all_stock) {
  
  
  ########################################################
  
  IND = pero %>% filter(STOCK == species)
  
  
  DAMSIRE = matingcage %>% filter(STOCK == species)
  
  
  IND2 = IND %>%
    mutate(MatingNumber = str_remove(MatingNumber, species)) %>%
    mutate(ID = str_remove(ID, species)) %>% 
    
    mutate(MatingNumber = str_replace_all(MatingNumber, "[^[:alnum:]]", "")) %>%
    
    mutate(ID = str_replace_all(ID, "[^[:alnum:]]", "")) 
  
  
  
  DAMSIRE2 = DAMSIRE %>%
    
    mutate(Dam = str_remove(Dam, species)) %>%
    mutate(Sire = str_remove(Sire, species)) %>%
    mutate(MatingNumber = str_remove(MatingNumber, species)) %>%
    mutate(Dam = str_replace_all(Dam, "[^[:alnum:]]", "")) %>%
    mutate(Sire = str_replace_all(Sire, "[^[:alnum:]]", "")) %>%
    mutate(MatingNumber = str_replace_all(MatingNumber, "[^[:alnum:]]", "")) 
  
  # Identify common columns (excluding 'MatingNumber')
  common_cols <- intersect(names(DAMSIRE2), names(IND2))
  common_cols <- setdiff(common_cols, "MatingNumber")
  
  # Remove duplicate columns from the second data frame (assuming df_pero)
  DAMSIRE2 <- DAMSIRE2 %>% select(-all_of(common_cols))
  
  
  merged_df = merge(DAMSIRE2, IND2, by = 'MatingNumber')  %>%
    mutate(Birthday = as.Date(Birthday, format = "%m/%d/%Y"), # Adjust format as needed
           BirthMonth = month(Birthday),
           BirthYear = year(Birthday)) 
  
  dam_info <- merged_df %>%
    select(ID, Birthday, BirthMonth, BirthYear) %>%
    rename(Birthday_Dam = Birthday, BirthMonth_Dam = BirthMonth, BirthYear_Dam = BirthYear)
  
  
  sire_info <- merged_df %>%
    select(ID, Birthday, BirthMonth, BirthYear) %>%
    rename(Birthday_Sire = Birthday, BirthMonth_Sire = BirthMonth, BirthYear_Sire = BirthYear)
  
  
  merged_df2 <- merged_df %>%  
    left_join(dam_info, by = c("Dam" = "ID")) %>%
    left_join(sire_info, by = c("Sire" = "ID")) %>%
    filter(!is.na(Birthday), !is.na(Birthday_Sire), !is.na(Birthday_Dam))
  
  #############
  # Function to process each sire or dam
  process_parent <- function(data, parent) {
    data %>% 
      group_by(.data[[parent]]) %>% # Correctly refer to dynamic column names
      nest() %>% 
      mutate(data = map(data, ~ .x %>%
                          mutate(one_year_later = min(Birthday) + years(1)) %>%
                          filter(Birthday < one_year_later))) %>%
      unnest(cols = c(data)) %>%
      select(-one_year_later)
  }
  
 
  ##########################
  # Function to summarize data by birth month over 5-year intervals and write to Excel
  
  summarize_by_birthmonth_5_year_interval_to_excel <- function(data, parent_type) {
    birth_year_col <- paste0("BirthYear_", parent_type)
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
    # Prepare an empty list to store results
    results <- list()
    
    # Calculate intervals based on the range of years in the dataset, divided into 5-year segments
    seq_interval_start <- seq(min(data[[birth_year_col]]), max(data[[birth_year_col]]), by = 5)
    
    # Iterate over each interval
    for (interval_start in seq_interval_start) {
      interval_end <- interval_start + 4  # Define the end of the interval
      
      # Filter and summarize data within the current interval
      summarized <- data %>%
        filter(between(.data[[birth_year_col]], interval_start, interval_end)) %>%
        group_by(.data[[birth_month_col]], BirthMonth) %>%
        summarise(total_count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE) %>%
        mutate(Year_Period = paste(interval_start, interval_end, sep = "-"))
      
      # Append summarized data to the results list
      results[[paste(interval_start, interval_end, sep = "-")]] <- summarized
    }
    
    # Combine all summarized data frames into one
    final_result <- bind_rows(results)
    
    # Write to an Excel file
    #write_xlsx(final_result, output_file)
  }
  
 
  
   ensure_continuous_birthmonth <- function(data, parent_type, output_file_path) {
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
    all_months <- tibble(BirthMonth = 1:12)
    
    completed_data <- lapply(unique(data$Year_Period), function(year_period) {
      period_data <- filter(data, Year_Period == year_period)
      
      # Use direct column name for renaming if necessary
      period_data <- period_data %>%
        rename(BirthMonth = !!sym(birth_month_col))
      
      # Ensure all months are present
      period_data_full <- full_join(all_months, period_data, by = "BirthMonth") %>%
        replace_na(list(total_count = 0)) %>%
        mutate(Year_Period = year_period)
      
      # Optionally, rename the BirthMonth column back to its original dynamic name
      period_data_full <- period_data_full %>%
        rename(!!sym(birth_month_col) := BirthMonth)
      
      period_data_full
    })
    
    completed_data <- bind_rows(completed_data)
    
    write_xlsx(completed_data, output_file_path)
  }
  
  parents = c('Dam', 'Sire')
  for (parent in parents) {
    full_1_year <- process_parent(merged_df2, parent)
    
    write.xlsx(full_1_year, paste0(species,'-',parent,'- all mice born for 1 year from 1st delivery.xlsx'))
    
    summarize_by_birthmonth = summarize_by_birthmonth_5_year_interval_to_excel(full_1_year, parent)
    
    output_file_path = paste0(species,"-",parent,"_by_birthmonth_5_year_interval.xlsx")
    
    ensure_continuous_birthmonth(summarize_by_birthmonth, parent, output_file_path)
  }

  
 
  
}
