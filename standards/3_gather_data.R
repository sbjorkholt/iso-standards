
#### STEP 3 ####

# List up files from folders

dates_table_list <- list.files("../../data/dates_table/", pattern = "dates_table.*rds", full.names = TRUE) 

life_cycle_list <- list.files("../../data/life_cycle/", pattern = "life_cycle.*csv", full.names = TRUE)

# Read the files into R

dates_table_data <- list()

for (i in 1:length(dates_table_list)) {
  
  # Make a progress bar on the reading of the csv-files
  it <- 100 * i / length(unique(dates_table_list))
  cat(paste0(sprintf("Progress: %.2f%%         ", it), "\r"))
  
  data <- read_rds(dates_table_list[[i]])
  
  dates_table_data[[i]] <- data %>%
    mutate(stdno = str_extract(dates_table_list[[i]], "[0-9]+")) 
  
}

dates_table_data <- discard(dates_table_data, function(z) nrow(z) == 0) # Some of the tables are empty -- discard them

# Same procedure for the life cycle tables
life_cycle_data <- list()

for (i in 1:length(life_cycle_list)) {
  
  it <- 100 * i / length(unique(life_cycle_list))
  cat(paste0(sprintf("Progress: %.2f%%         ", it), "\r"))
  
  data <- vroom(life_cycle_list[[i]], 
                col_select = c(2, 3, 4, 5), 
                col_types = c(.default = "c"),
                .name_repair = "minimal")
  
  life_cycle_data[[i]] <- data %>%
    mutate(stdno = str_extract(life_cycle_list[[i]], "[0-9]+")) 
  
}

life_cycle_data <- discard(life_cycle_data, function(z) nrow(z) == 0)

# Removing standards that are in one dataset but not the other

unnested_dates_table <- tibble(dates_table = dates_table_data) %>% 
  unnest(dates_table)

unnested_life_cycle <- tibble(life_cycle = life_cycle_data) %>% 
  unnest(life_cycle) 

lost_stdno <- vecsets::vsetdiff(unnested_dates_table %>% pull(stdno) %>% unique(), 
                                unnested_life_cycle %>% pull(stdno) %>% unique())

dates_table_data2 <- tibble(dates_table = dates_table_data) %>% 
  rowid_to_column() %>% 
  unnest(cols = c(dates_table)) %>%
  unnest(cols = c(data)) %>%
  mutate(variable = ifelse(variable == "abstract", "Abstract",
                           variable)) %>%
  filter(!stdno %in% lost_stdno) 

# Put files together and make two variables for the committee name, the committee title, the ICS name and the ICS id
committee <- dates_table_data2 %>%
  filter(variable == "Technical Committee") %>%
  mutate(value = str_squish(str_extract(value, "ISO/(TMBG)?(IEC JTC 1)?(TC [0-9]+)?( [0-9]+)?(\\s+)?(\\/SC [0-9]+)?( [0-9]+)?"))) %>%
  mutate(value = str_replace_all(value, "(?<=\\d)\\s(?=\\d)", "")) %>%
  pull(value) 

title <- dates_table_data2 %>%
  filter(variable == "Technical Committee") %>%
  mutate(value = str_squish(str_remove_all(value, "ISO/(TMBG)?(IEC JTC 1)?(TC [0-9]+)?( [0-9]+)? (\\/SC [0-9]+)?( [0-9]+)?"))) %>%
  mutate(value = ifelse(value == "ISO/TMBG", "Technical Management Board Group", value)) %>%
  pull(value) 

stdnos <- dates_table_data2 %>% pull(stdno) %>% unique()

status_df <- dates_table_data2 %>% select(rowid, stdno, variable, value) %>% spread(variable, value)

## For alternative ICS:
# https://github.com/metanorma/iso-ics-codes.git

# Put all information together in one dataframe
standards_df <- tibble(stdno = status_df$stdno,
                       rowid = status_df$rowid,
                       title = title,
                       committee = committee,
                       status = status_df$Status,
                       publication_date = status_df$`Publication date`,
                       edition = status_df$Edition,
                       pages = status_df$`Number of pages`,
                       abstract = status_df$Abstract,
                       ics = dates_table_data2 %>% select(ICS, rowid, stdno) %>% group_by(rowid, stdno) %>% nest() %>% ungroup() %>% select(-c(rowid, stdno)),
                       sdgs = dates_table_data2 %>% select(SDGs, rowid, stdno) %>% group_by(rowid, stdno) %>% nest() %>% ungroup() %>% select(-c(rowid, stdno))) %>%
  unnest() %>%
  rename(ics = data, 
         sdgs = data1) %>%
  mutate(life_cycle = life_cycle_data,
         link = paste0("https://www.iso.org/standard/", stdnos, ".html?browse=tc"))

## Life cycle

standards_df <- standards_df %>%
  mutate(life_cycle = map(life_cycle, 
                          ~ drop_na(., value)),
         life_cycle = map(life_cycle,
                          ~ filter(., !value %in% c("10", "20", "30", "40", "50", "60", "90", "95"))),
         life_cycle = map(life_cycle,
                          ~ mutate(., value = str_remove(value, "[0-9]{2} "))))

standards_df <- standards_df %>%
  unnest(ics) %>%
  unique() %>%
  unnest(sdgs) %>%
  unique()

## Save the dataset to folder
saveRDS(standards_df, file = "../../data/final_data/standards_df.rds")
