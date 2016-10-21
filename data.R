source('pre_data.R', local = TRUE)
libs <- c(libs, "ggplot2")
sapply(libs, require, character.only = TRUE)


dtPW = inner_join(dt %>% mutate(id=paste(doy,yr,sep="-")), 
                 wthDaily %>% mutate(id=paste(doy, yyyy, sep="-")) %>%
                   select(id,Tavg:WSavg), 
                 by = "id") %>% select(-id)

tdtPW = dtPW %>% gather(key="variable", value = "value", pollen:WSavg)

# Ploting gathered Pollen Data --------------------------------------------

# All period from April 1st to July 31
ggplot(dt, aes(x = doy, y = pollen)) + 
  facet_wrap(~yr) + geom_line()


# Show only 40 days from April 1st
ggplot(dt %>% filter(between(doy, 90, 130)), aes(x = doy, y = pollen)) + 
  facet_wrap(~yr) + geom_line()

ggplot(wthDaily %>% filter(between(doy, 90,130)), aes(x = doy, y = RainTot)) + 
  facet_wrap(~yyyy) + geom_line()


# Plot tidy Dataset 2014-2016
ggplot(tdtPW %>% filter(between(doy, 90, 130)), aes(x=doy, y=value)) + 
  facet_grid(variable~yr, scales = "free_y") + 
  geom_line()
