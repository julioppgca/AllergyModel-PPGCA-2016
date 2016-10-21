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


if(!file.exists('Data/Pollen.csv')) {
  dt=do.call(rbind,mapply(gatherPollen, 
                          yr = rep(2007:2016, each=5), 
                          mth=3:7, 
                          SIMPLIFY = FALSE))
  write.csv(dt, file = 'Data/Pollen.csv', row.names = FALSE)
} 


wthDaily = read.csv('Data/GA_aemn_daily_375.csv')
wthHourly = read.csv('Data/GA_aemn_daily_375.csv')
dt = read.csv('Data/Pollen.csv')
