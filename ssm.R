source('pre_data.R', local = TRUE)
libs <- c(libs, "KFAS")
sapply(libs, require, character.only = TRUE)

dtPW = inner_join(dt %>% mutate(id=paste(doy,yr,sep="-")), 
                  wthDaily %>% mutate(id=paste(doy, yyyy, sep="-")) %>%
                    select(id,Tavg:WSavg), 
                  by = "id") %>% select(-id) %>%
  mutate(RTemp = Tmax - Tmin,
         RH90 = ifelse(RHavg >= 90, 1, 0)) %>%
  select(yr:pollen,RHavg:SRtot,RTemp:RH90) %>%
  filter(between(doy, 70, 125))

# SSM - Pollen -----------------------------------------------------------

y_pollen_split = split(dtPW["pollen"], dtPW["yr"])
ppollen <- length(y_pollen_split)

y_pollen = matrix(unlist(y_pollen_split), ncol = ppollen,
                  dimnames = list(NULL, paste("Year", names(y_pollen_split))))


dataf_pollen <- split(dtPW, dtPW["yr"])

## Block-Diag 5 params + intercept
P1pollen <- as.matrix(.bdiag(replicate(ppollen, matrix(NA, 6, 6), simplify = FALSE)))

model_lmm_pollen <- SSModel(y_pollen ~ -1 +
                              SSMregression(rep(list(~ RHavg+RainTot+SRtot+RTemp+RH90), ppollen), 
                                            type = "common", data = dataf_pollen,
                                            remove.intercept = FALSE) +
                              SSMregression(rep(list(~ RHavg+RainTot+SRtot+RTemp+RH90), ppollen),
                                            data = dataf_pollen, remove.intercept = FALSE,
                                            P1 = P1pollen),
                            H = diag(NA, ppollen))

update_lmm_pollen <- function(pars, model) {
  P1 <- diag(exp(pars[1:5]))
  P1[1, 2:5] <- pars[6:9]
  P1[2, 3:5] <- pars[10:12]
  P1[3, 4:5] <- pars[13:14]
  P1[4, 5]   <- pars[15]
  P1 <- crossprod(P1)
  # p * 5 params each + 5 common = 38
  model["P1", states = 6:20] <- as.matrix(.bdiag(replicate(ppollen, P1, simplify = FALSE)))
  model["H"] <- diag(exp(pars[16:20]), ppollen)
  model
}

fit_lmm_pollen <- fitSSM(model_lmm_pollen, 
                         c(rep(2, 15), rep(10,5)), 
                           update_lmm_pollen, 
                           method = "BFGS")
