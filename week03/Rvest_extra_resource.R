# General-purpose data wrangling
library(tidyverse)

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

# identifier
url <-'http://www.trustpilot.com/review/www.amazon.com'



# function that figures out last page for you
get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.pagination-page') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}



first_page <- read_html(url)
(latest_page_number <- get_last_page(first_page))


# generate a list of all relevant URLS
list_of_pages <- str_c(url, '?page=', 1:latest_page_number)



# get reviews function
get_reviews <- function(html){
  html %>% 
    
    # The relevant tag
    html_nodes('.review-body') %>%      
    html_text() %>% 
    
    # Trim additional white space
    str_trim() %>%                       
    
    # Convert the list into a vector
    unlist()                             
}


# find reviewers name
get_reviewer_names <- function(html){
  html %>% 
    
    # relevant tag
    html_nodes('.user-review-name-link') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}


# look for the most broad description
# and then try to cut out all redundant info
get_review_dates <- function(html){
  
  status <- html %>% 
    html_nodes('time') %>% 
    # The status information is this time a tag attribute
    html_attrs() %>%             
    # Extract the second element
    map(2) %>%                    
    unlist() 
  
  dates <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>% 
    map(1) %>% 
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%                 
    unlist()
  
  # Combine the status and the date information to filter one via the other
  return_dates <- tibble(status = status, dates = dates) %>%   
    # Only these are actual reviews
    filter(status == 'ndate') %>%              
    # Select and convert to vector
    pull(dates) %>%                            
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00') 
  
  # The lengths still occasionally do not lign up. You then arbitrarily crop the dates to fit
  # This can cause data imperfections, however reviews on one page are generally close in time)
  
  length_reviews <- length(get_reviews(html))
  
  return_reviews <- if (length(return_dates)> length_reviews){
    return_dates[1:length_reviews]
  } else{
    return_dates
  }
  return_reviews
}








