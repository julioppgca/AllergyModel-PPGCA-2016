libs <- c('dplyr', 'tidyr', 'rvest')
sapply(libs, require, character.only = TRUE)

# Get ATLANTA ALLERGY POLLEN COUNT ----------------------------------------


gatherPollen = function(yr, mth) {
  
  web_addr = sprintf("http://www.atlantaallergy.com/pollen_counts/index/%4d/%02d/01",
                     yr, mth)
  
  # We have interest in these contents of HTML page
  # inside div.calendar-body
  # calendar-day-inner > day-num | count > a
  
  pg = read_html(web_addr) %>% html_nodes("div.calendar-body")
  
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
  
  tmpD = as.Date(sprintf("%d-%02d-%02d", yr, mth, days[idx]))
  fPollen = counts[idx]
  doy = as.numeric(strftime(tmpD, format = "%j"))
  
  data.frame(yr = yr,
             doy = doy,
             pollen = fPollen)
}


dt=do.call(rbind,mapply(gatherPollen, yr = rep(2010:2016, each=4), mth=4:7, SIMPLIFY = FALSE))

wthDaily = read.csv('Data/GA_aemn_daily_375.csv')
wthHourly = read.csv('Data/GA_aemn_daily_375.csv')
