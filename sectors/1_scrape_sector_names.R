
################################################################################################################################
#################################                            SECTORS                              ##############################
################################################################################################################################

library(tidyverse)
library(rvest)

sectorlist <- read_html("https://www.iso.org/technical-committees.html") %>% # All technical committees in ISO
  # Fetching the node containing information on sectors
  html_nodes("#content > section:nth-child(3) > div > div > div > nav > div.navbar-collapse.nav-2levels > ul > li.dropdown.active > ul > li > a") %>%
  html_attr("href") %>% str_c("https://www.iso.org/technical-committees.html", .) %>% # Grabbing the link and adding pre-link info
  tibble() %>% # Make into a tibble to...
  filter(str_detect(., "[A-Z]+")) %>% # ... easily use str_detect to remove the links that are not about sectors (all sector-links have capital letters)
  pull(.) # And make into a character vector again

sectortable <- list() 
sectorname <- list()

for (i in 1:length(sectorlist)) { # Loop over each link
  
  sectortable[[i]] <- read_html(sectorlist[i]) %>%
    html_node("#datatable-committees") %>% # Fetch the table showing technical committees belonging to that sector
    html_table() # Parse to table
  
  sectorname[[i]] <- read_html(sectorlist[i]) %>%
    # Also fetch the name of the sector, as they have changed names on the webpage but not in the html-code
    html_node("#content > section:nth-child(3) > div > div > div > nav > div.navbar-collapse.nav-2levels > ul > li.dropdown.active > a") %>% 
    html_text() %>% # Parse to text
    str_remove("\\([0-9]+\\)") %>% # Remove numbers showing how many technical committees are in the sector
    str_squish() # Remove whitespace
  
}

sectors <- tibble(name = unlist(sectorname),
                  table = sectortable) %>% # Put the two vectors into a tibble
  unnest() %>% # Unnest the table column
  select(name, Reference, Title) %>% # Pick the relevant variables
  rename(sector = name, # Rename these variables so that they match with the rest of the data
         committee = Reference,
         title = Title)

saveRDS(sectors, file = "../../data/final_data/sectors.rds")

                       