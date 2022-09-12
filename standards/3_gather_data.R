
#### STEP 3 ####

# List up files from folders

dates_table_list <- list.files("../data/dates_table/", pattern = "dates_table.*csv", full.names = TRUE) 

life_cycle_list <- list.files("../data/life_cycle/", pattern = "life_cycle.*csv", full.names = TRUE)

# Read the files into R

dates_table_data <- list()

for (i in 1:length(dates_table_list)) {
  
  # Make a progress bar on the reading of the csv-files
  it <- 100 * i / length(unique(dates_table_list))
  cat(paste0(sprintf("Progress: %.2f%%         ", it), "\r"))
  
  data <- vroom(dates_table_list[[i]], 
                col_select = c(2, 3), # Choose the second and third column to avoid the `...1` variable
                col_types = c(.default = "c"), # Set all columns to character
                .name_repair = "minimal") # Do not do any name repairs
  
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

# Some poorly written code in order to take out the standard that is present in the life_cycle list but not the dates_table list

unnested_dates_table <- tibble(dates_table = dates_table_data) %>% 
  unnest(dates_table) 

unnested_life_cycle <- tibble(life_cycle = life_cycle_data) %>% 
  unnest(life_cycle) 

lost_stdno <- vecsets::vsetdiff(unnested_dates_table %>% pull(stdno) %>% unique(), 
                                unnested_life_cycle %>% pull(stdno) %>% unique())

dates_table_data <- dates_table_data %>%
  tibble() %>% rowid_to_column() %>% 
  unnest() %>% 
  filter(!stdno %in% lost_stdno) %>% 
  group_by(rowid) %>%
  nest() %>% 
  ungroup() %>%
  select(-rowid)

# Put files together and make two variables for the committee name, the committee title, the ICS name and the ICS id
committee <- dates_table_data %>%
  unnest() %>%
  filter(variable == "Technical Committee") %>%
  pull(value)

title <- dates_table_data %>%
  unnest() %>%
  .[which(.$variable == "Technical Committee") + c(1),] %>%
  pull(variable)

ics_id <- dates_table_data %>%
  unnest() %>%
  filter(variable == "ICS") %>%
  pull(value)

ics_name <- dates_table_data %>%
  unnest() %>%
  .[which(.$variable == "ICS") + c(1),] %>%
  mutate(variable = ifelse(variable == "Status", NA, variable)) %>%
  pull(variable)

# Put all information together in one dataframe
standards_df <- tibble(title = title,
                       committee = committee,
                       ics_name = ics_name,
                       ics_id = ics_id,
                       dates_table = dates_table_data$data,
                       life_cycle = life_cycle_data,
                       link = paste0("https://www.iso.org/standard/", unnest(dates_table_data) %>% pull(stdno) %>% unique(), ".html?browse=tc")) 


standards_df <- readRDS("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/data/standards_df_09_09_2022.rds")

#### FINAL CLEANING ####

## Life cycle

standards_df <- standards_df %>%
  mutate(life_cycle = map(life_cycle, 
                          ~ drop_na(., value)),
         life_cycle = map(life_cycle,
                          ~ filter(., !value %in% c("10", "20", "30", "40", "50", "60", "90", "95"))),
         life_cycle = map(life_cycle,
                          ~ mutate(., value = str_remove(value, "[0-9]{2} "))))

## Status

status <- standards_df %>%
  mutate(dates_table = map(dates_table, 
                           ~ filter(., variable %in% c("Status", "Number of pages", "Edition", "Publication date"))),
         dates_table = map(dates_table,
                           ~ pivot_wider(., names_from = variable, values_from = value))) %>%
  unnest(dates_table) %>%
  rename(status = Status,
         pages = `Number of pages`,
         edition = Edition,
         publication_date = `Publication date`) %>%
  select(stdno, title, committee, ics_name, ics_id, status, publication_date, edition, pages, life_cycle, link)

## Sustainability goals

goals <- standards_df %>%
  rowid_to_column() %>%
  select(rowid, committee, title, dates_table) %>%
  unnest(dates_table) %>%
  filter(str_detect(variable, "^[0-9]{1,2} ") | str_detect(variable, "Status")) %>% #".{60,}")) %>%
  mutate(variable = str_squish(variable)) %>%
  mutate(value = str_squish(value)) %>%
  group_by(rowid) %>%
  filter(!str_detect(variable, "[0-9] This")) %>%
  rename(sustainability = variable) %>%
  select(-value) %>%
  mutate(sustainability = str_split(sustainability,"\\s+([0-9]{1,2})")) %>%
  ungroup() %>%
  unnest(sustainability) %>%
  mutate(sustainability = str_remove_all(sustainability, "[0-9]{1,2}"),
         sustainability = str_squish(sustainability)) %>%
  mutate(sustainability = ifelse(sustainability == "Status", 0, sustainability)) %>%
  group_by(rowid) %>%
  nest(sustainability = c(sustainability)) %>%
  mutate(goals_dich = map_dbl(sustainability, nrow),
         goals_dich = ifelse(goals_dich == 1, 0, 1)) %>%
  ungroup() %>%
  unnest() %>%
  mutate(sustainability = ifelse(sustainability == 0 & goals_dich == 1, NA,
                                 ifelse(sustainability == 0, "",
                                        sustainability))) %>%
  filter(sustainability %in% c("Industry, Innovation and Infrastructure", "Good Health and Well-being", "Responsible Consumption and Production",
                               "Decent Work and Economic Growth", "Sustainable Cities and Communities", "Life on Land",
                               "Climate Action", "Affordable and Clean Energy", "Zero Hunger", "Reduced Inequalities",
                               "Clean Water and Sanitation", "Quality Education", "No Poverty", "Life Below Water",
                               "Gender Equality", "Peace, Justice and Strong Institutions")) %>%
  nest(goals_cat = c(sustainability)) %>%
  ungroup() %>%
  select(-rowid)


standards_df2 <- left_join(status, goals, 
                          by = c("stdno", "title", "committee"))

## Abstracts

abstracts <- standards_df %>%
  select(title, committee, dates_table) %>%
  unnest() %>%
  mutate(nvariable = nchar(stringr::str_conv(variable, "UTF-8")),
         nvalue = nchar(str_conv(value, "UTF-8"))) %>%
  filter(nvariable >= 100 | nvalue >= 100) %>%
  mutate(sustainability = str_detect(variable, "^([0-9]{1,2})")) %>%
  mutate(variable = replace_na(variable, ""),
         value = replace_na(value, ""),
         value = str_c(" ", value)) %>%
  mutate(abstract1 = str_c(variable, value)) %>%
  filter(sustainability != TRUE) %>%
  group_by(stdno) %>%
  mutate(abstract = paste0(abstract1, collapse = " ")) %>%
  select(title, committee, stdno, abstract) %>%
  unique() %>% 
  ungroup()

standards_df <- left_join(standards_df2, abstracts, 
                          by = c("stdno", "title", "committee"))

standards_df <- standards_df %>% 
  select(stdno, title, committee, ics_name, ics_id, status, publication_date, edition, pages, abstract, goals_dich, goals_cat, life_cycle, link)


## Save the dataset to folder
saveRDS(standards_df, file = "../../data/final_data/standards_df.rds")
