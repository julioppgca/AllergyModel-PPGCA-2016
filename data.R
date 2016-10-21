source('pre_data.R', local = TRUE)
libs <- c(libs, "ggplot2")
sapply(libs, require, character.only = TRUE)


dtPW = inner_join(dt %>% mutate(id=paste(doy,yr,sep="-")), 
                 wthDaily %>% mutate(id=paste(doy, yyyy, sep="-")) %>%
                   select(id,Tavg:WSavg), 
                 by = "id") %>% select(-id) %>%
  mutate(RTemp = Tmax - Tmin,
         RH90 = ifelse(RHavg >= 90, 1, 0))

tdtPW = dtPW %>% gather(key="variable", value = "value", pollen:WSavg)

tdtPW2 = dtPW %>% select(yr:pollen, RainTot, SRtot, WSavg, RTemp, RH90) %>%
  gather(key="variable", value = "value", pollen:RH90)

# Ploting gathered Pollen Data --------------------------------------------

# All period from March 1st to July 31
ggplot(dt, aes(x = doy, y = pollen)) + 
  facet_wrap(~yr) + geom_line()

# Show only 55 days from March 10 - May 04
ggplot(dt %>% filter(between(doy, 70, 125)), aes(x = doy, y = pollen)) + 
  facet_wrap(~yr) + geom_line()

ggplot(wthDaily %>% filter(between(doy, 70,125)), aes(x = doy, y = RainTot)) + 
  facet_wrap(~yyyy) + geom_line()


# Plot tidy Dataset 2014-2016
ggplot(tdtPW %>% filter(between(doy, 70, 125)), aes(x=doy, y=value)) + 
  facet_grid(variable~yr, scales = "free_y") + 
  geom_line()

# Plot tidy Dataset 2014-2016 pollen:RH90
ggplot(tdtPW2 %>% filter(between(doy, 70, 125)), aes(x=doy, y=value)) + 
  facet_grid(variable~yr, scales = "free_y") + 
  geom_line()
