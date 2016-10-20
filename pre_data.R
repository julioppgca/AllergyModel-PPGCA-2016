libs <- c('dplyr', 'tidyr', 'rvest')
sapply(libs, require, character.only = TRUE)


# Get ATLANTA ALLERGY POLLEN COUNT ----------------------------------------

pllc_web <- read_html("http://www.atlantaallergy.com/pollen_counts/index/2015/04/01")

# We have interest in these contents of HTML page
# inside div.calendar-body
# calendar-day-inner > day-num | count > a


pg <- html_nodes(pllc_web, "div.calendar-body")

pg %>% html_nodes("div.calendar-day-inner") 

days=pg %>% html_nodes("div.calendar-day-inner") %>% 
  html_nodes("span.day-num") %>% 
  html_text() %>%
  gsub(pattern = "\\s+(\\d+)\\s+", replacement = "\\1") %>%
  as.numeric()

counts=pg %>% html_nodes("div.calendar-day-inner")  %>% html_nodes("a") %>% html_text() %>% as.numeric()

idx = pg %>% html_nodes("div.calendar-day-inner") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  grep(pattern = "2015/04")


dt = data.frame(days=days[idx],pollen=counts[idx])

