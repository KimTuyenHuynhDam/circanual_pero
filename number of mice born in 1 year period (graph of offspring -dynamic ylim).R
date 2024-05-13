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
  
  process_parent <- function(data, parent) {
    data %>% 
      group_by(.data[[parent]]) %>% 
      nest() %>% 
      mutate(data = map(data, ~ .x %>%
                          mutate(one_year_later = min(Birthday) + years(1)) %>%
                          filter(Birthday < one_year_later))) %>%
      unnest(cols = c(data)) %>%
      select(-one_year_later)
  }
  
  
  ##########################
  # Function to summarize data by birth month over 5-year intervals
  
  summarize_by_birthmonth_5_year_interval_to_excel <- function(data, parent_type) {
    birth_year_col <- paste0("BirthYear_", parent_type)
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
   
    results <- list()
    
    # Calculate intervals based on the range of years in the dataset, divided into 5-year segments
    seq_interval_start <- seq(min(data[[birth_year_col]]), max(data[[birth_year_col]]), by = 5)
    
    
    max_year <- max(data[[birth_year_col]])
    

    for (interval_start in seq_interval_start) {
      interval_end <- interval_start + 4  
      
      # Filter and summarize data within the current interval
      summarized <- data %>%
        filter(between(.data[[birth_year_col]], interval_start, interval_end)) %>%
        group_by(.data[[birth_month_col]], BirthMonth) %>%
        summarise(total_count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE) %>%
        
        mutate(Year_Period = if_else(interval_end > max_year, "Out_of_Range", paste(interval_start, interval_end, sep = "-")))
      
      results[[paste(interval_start, interval_end, sep = "-")]] <- summarized
    }
    
    
    final_result <- bind_rows(results)
    
   
    final_result <- filter(final_result, Year_Period != "Out_of_Range")

  }
  
  
  
  
  
  ensure_continuous_birthmonth <- function(data, parent_type) {
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
    all_months <- tibble(BirthMonth = 1:12)
    
    completed_data <- lapply(unique(data$Year_Period), function(year_period) {
      period_data <- filter(data, Year_Period == year_period)
      
    
      period_data <- period_data %>%
        rename(BirthMonth = !!sym(birth_month_col))
      
    
      period_data_full <- full_join(all_months, period_data, by = "BirthMonth") %>%
        replace_na(list(total_count = 0)) %>%
        mutate(Year_Period = year_period)
      
     
      period_data_full <- period_data_full %>%
        rename(!!sym(birth_month_col) := BirthMonth)
      
      period_data_full
    })
    
    completed_data <- bind_rows(completed_data)
    
   
  }
  parent = 'Dam'
  
  parents = c('Sire','Dam')
  
  for (parent in parents) {
    full_1_year <- process_parent(merged_df2, parent)
    
    #write.xlsx(full_1_year, paste0(species,'-',parent,'- all mice born for 1 year from 1st delivery.xlsx'))
    
    summarize_by_birthmonth = summarize_by_birthmonth_5_year_interval_to_excel(full_1_year, parent)
    
    #output_file_path = paste0(species,"-",parent,"_by_birthmonth_5_year_interval.xlsx")
    
    data = ensure_continuous_birthmonth(summarize_by_birthmonth, parent) 
    data2 = data %>%  select(-ncol(.)) 
    
    # Transform the data to ratios and multiply by 100, and add the Month column
    data_transformed <- data2 %>%
      mutate(RowSum = rowSums(select(., -1), na.rm = TRUE)) %>%
      rowwise() %>%
      mutate(across(-c(1, RowSum), ~ .x / RowSum * 100)) %>%
      ungroup() %>%
      select(-RowSum)
    
    birth_month_col <- paste0("BirthMonth_", parent)
    
    ################
    
    data_matrix <- as.matrix(data_transformed %>% select(-all_of(birth_month_col)))
    
   
    for (i in 1:nrow(data_matrix)) {
      if(sum(is.na(data_matrix[i, ])) > 9) {
        data_matrix[i, ] <- NA
      }
    }
    

    data_adjusted <- as.data.frame(data_matrix)
  
    data_adjusted <- bind_cols(data_transformed %>% select(all_of(birth_month_col)), data_adjusted)  %>%
      mutate(across(everything(), ~replace(., is.nan(.), NA)))
    
   
    colnames(data_adjusted)[-1] <- colnames(data_transformed)[-which(names(data_transformed) == birth_month_col)]
    
    data_adjusted2 <- data_adjusted %>% cbind(data %>%  select(ncol(.)) )
    
    write_xlsx(data_adjusted2, paste0('./percentage offspring/',species, '-',parent,'-percentage of offspring by parent birthmonth.xlsx'))

    
    # Step 2 and Step 3: Combine Rows Based on Month and Calculate Averages and Standard Deviations
   
    data_grouped <- data_adjusted %>%
      group_by(!!sym(birth_month_col)) %>%
      summarise(
        across(
          everything(),
          list(
            mean = ~mean(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE),
            n = ~sum(!is.na(.x))  
          )
        ),
        .groups = 'drop'
      ) %>%
      ungroup() %>%
    
      # Calculate SEM for each group based on SD and N
      mutate(across(contains("_sd"), 
                    ~.x / sqrt(get(str_replace(cur_column(), "_sd", "_n"))), 
                    .names = "{.col}_sem"))%>%
      select(-contains("_n"))
    
    
   
    first_col_name <- names(data_grouped)[1]
    
    data_grouped <- data_grouped %>%
      rename_with(~str_replace_all(.x, "_sd_sem$", "_sem")) %>%
      
      rename_with(
        ~case_when(
          .x == first_col_name ~ .x,  
          str_starts(.x, "X") ~ .x,  
          TRUE ~ paste0("X", .x)  
        ),
        .cols = -all_of(first_col_name)  
      ) 
 
    
    
    
    
    
    data_mean = data_grouped %>% select(contains('mean')) %>% t() %>%
      as.data.frame() 
      
    
    data_sem = data_grouped %>% select(contains('sem')) %>% t() %>%
      as.data.frame() 

  
   for (i in 1:12) {

      full_month_name <- month.name[i]
      mean = data_mean[i] 
      colnames(mean) = 'mean'
        
      sem = data_sem[i] 
      colnames(sem) = 'sem'
      
      data = mean %>% cbind(sem) %>% cbind('BirthMonth'= 1:12)
 
      max_y_with_error <- max(data$mean + data$sem, na.rm = TRUE)
      
 
      upper_ylim <- ceiling(max(max_y_with_error, 20) / 5) * 5
      
     
      p <- ggplot(data, aes(x = BirthMonth, y = mean)) + 
        geom_point(color = "blue", size = 4) +
        geom_errorbar(color = "blue",
                      aes(
                        ymin = mean - sem, 
                        ymax =mean + sem
                      ),
                      width = 0.4
        ) +
        theme_minimal() +
        labs(
          title = full_month_name,
          x = "Litter in",
          y = "% offspring in month"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18), 
          axis.title = element_text(size = 14),  
          axis.line = element_line(linewidth = 1, color = "black"),  
          axis.ticks = element_line(color = "black", size = 1),
          axis.text.x = element_text(margin = margin(t = 5)),  
          axis.text.y = element_text(margin = margin(r = 5)),  
          axis.ticks.length = unit(0.3, "cm"), 
          axis.text = element_text(size = 14), 
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = NA)  
        ) +
        scale_x_discrete(limits = month.abb) + s
        ylim(0, upper_ylim)
   
      print(p)
      
    
      ggsave(
        paste0('./graph/SEM/(SEM) ',species, ' - ', parent, ' born in ', i ,' (', full_month_name, ").png"), 
        plot = p, 
        width = 8, 
        height = 6
      )
    }
  }
}
