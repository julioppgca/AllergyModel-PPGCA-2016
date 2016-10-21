libs <- c(libs, "lme4")
sapply(libs, require, character.only = TRUE)


summary(dtPW)

m0 <- glm(pollen~RainTot+SRtot+RHavg+RTemp+as.factor(RH90), data=dtPW,
          family = poisson, subset = yr == 2016)
summary(m0)
par(mfrow=c(2,2))
plot(m0)

dtPW$fyr = factor(dtPW$yr)
dtPW$fRH90 = factor(dtPW$RH90)
dtPW$fdoy = factor(dtPW$doy)

m1 <- glmer(pollen~RainTot+SRtot+RHavg+RTemp+fRH90 + (1|fyr) + (1|fdoy),
            data=dtPW %>% filter(between(doy, 70, 125)), 
            family = "poisson")

m2 <- glmer(pollen~RainTot+SRtot+RHavg+RTemp+fRH90 + (1|fyr) ,
            data=dtPW %>% filter(between(doy, 70, 125)), 
            family = "poisson")

m3 <- glmer(pollen~RainTot+SRtot+RHavg+RTemp+fRH90 + (1 + doy| fyr) ,
            data=dtPW %>% filter(between(doy, 70, 125)), 
            family = "poisson")
BIC(m1,m2,m3)

summary(m1)
vcov(m1)
ranef(m1)
profile(m1)
