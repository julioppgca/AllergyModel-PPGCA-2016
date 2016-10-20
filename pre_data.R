libs <- c('dplyr', 'tidyr', 'rvest')
sapply(libs, require, character.only = TRUE)


# Get ATLANTA ALLERGY POLLEN COUNT ----------------------------------------

pllc_web <- read_html("http://www.atlantaallergy.com/pollen_counts/index/2015/04/01")

# calendar-day-inner > day-num | count > a
# 

pg <- html_nodes(pllc_web, "div.calendar-body")

pg %>% html_nodes("div.calendar-day-inner") 

vals=pg %>% html_nodes("div.calendar-day-inner")  %>% html_nodes("span.day-num") %>% html_text() 

pg %>% html_nodes("div.calendar-day-inner")  %>% html_nodes("a") %>% html_text() %>% as.numeric()

pg %>% html_nodes("div.calendar-day-inner")  %>% html_nodes("a") %>% html_attr("href") 
