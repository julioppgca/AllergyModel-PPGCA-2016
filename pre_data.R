libs <- c('dplyr', 'tidyr', 'rvest')
sapply(libs, require, character.only = TRUE)

# Get ATLANTA ALLERGY POLLEN COUNT ----------------------------------------


gatherPollen = function(yr, mth) {
  
  web_addr = sprintf("http://www.atlantaallergy.com/pollen_counts/index/%4d/%02d/01",
                     yr, mth)
  
  pllc_web <- read_html(web_addr)
  
  # We have interest in these contents of HTML page
  # inside div.calendar-body
  # calendar-day-inner > day-num | count > a
  
  pg <- html_nodes(pllc_web, "div.calendar-body")
  
  # pg %>% html_nodes("div.calendar-day-inner") 
  
  days = pg %>% html_nodes("div.calendar-day-inner") %>% 
    html_nodes("span.day-num") %>% 
    html_text() %>%
    gsub(pattern = "\\s+(\\d+)\\s+", replacement = "\\1") %>%
    as.numeric()
  
  counts = pg %>% html_nodes("div.calendar-day-inner") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    as.numeric()
  
  idx = pg %>% html_nodes("div.calendar-day-inner") %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    grep(pattern = sprintf("%d/%02d", yr, mth))
  
  data.frame(days = days[idx],
             pollen = counts[idx], 
             fact = sprintf("%d/%02d", yr, mth))
}


dt=do.call(rbind,mapply(gatherPollen, yr = rep(2015:2016, each=4), mth=4:7, SIMPLIFY = FALSE))
