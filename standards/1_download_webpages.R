
#### STEP 1 ####

exists <- list.files("../data/webpages/") %>%
  str_replace_all(., "_", "") %>%
  str_c("https://www.iso.org/standard/", ., "?browse=tc")

html_test <- list() # Make a vector with new links, with standard numbers from 1 to 150000

for (i in 1:150000) { # Paste into the vector the link structure
  html_test[i] <- paste0("https://www.iso.org/standard/", i, ".html?browse=tc")
}

html_test <- unlist(html_test) 
html_test <- vecsets::vsetdiff(html_test, exists) # Extract links that have standards which are not already downloaded

html_test_tibble <- tibble(html_test) %>%
  rowid_to_column() # Make this vector into a tibble with row numbers

## When new standards are developed, I still do not know whether they could take a lower number than the highest number that exists currently.
## To be sure, I loop through all unused numbers when attempting to download webpages, to see if they have gotten content.

for (i in 1:nrow(html_test_tibble)) { # Loop through and download all the webpages that have operative links
  
  tryCatch({
    
    standard_no <- str_extract(html_test_tibble$html_test[i], "[0-9]+")
    
    suppressWarnings(download.file(html_test[i], # Download the html-file into a folder
                                   destfile = paste0("../data/test/", "_", standard_no, "_", ".html"), quiet=TRUE))
    
    message(paste("Downloading webpage no.", i, ", standard no.", standard_no)) # If the link has content, throw message
    
  }, 
  
  error = function(cond){
    
    message(paste("No webpage for webpage no.", i)) # If there is no standard at the link, throw message
  
  })
  
  Sys.sleep(2) 
  
  print(paste("Done with no.", i))
  
}
