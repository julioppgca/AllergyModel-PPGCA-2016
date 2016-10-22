
# SSM - Example -----------------------------------------------------------

library("lme4", quietly = TRUE)
summary(sleepstudy)
nlevels(sleepstudy$Subject)
y_split <- split(sleepstudy["Reaction"], sleepstudy["Subject"])
p <- length(y_split)

y <- matrix(unlist(y_split), ncol = p,
            dimnames = list(NULL, paste("Subject", names(y_split))))

dataf <- split(sleepstudy, sleepstudy["Subject"])

P1 <- as.matrix(.bdiag(replicate(p, matrix(NA, 2, 2), simplify = FALSE)))

model_lmm <- SSModel(y ~ -1 +
                       SSMregression(rep(list(~ Days), p), type = "common", 
                                     data = dataf, remove.intercept = FALSE) +
                       SSMregression(rep(list(~ Days), p), data = dataf,
                                     remove.intercept = FALSE, P1 = P1),
                     H = diag(NA, p))

update_lmm <- function(pars, model) {
  P1 <- diag(exp(pars[1:2]))
  P1[1, 2] <- pars[3]
  P1 <- crossprod(P1)
  # p * 2 params each + 2 common = 38
  model["P1", states = 3:38] <- as.matrix(.bdiag(replicate(p, P1, simplify = FALSE)))
  model["H"] <- diag(exp(pars[4]), p)
  model
}

fit_lmm <- fitSSM(model_lmm, c(1, 1, 1, 5), update_lmm, method = "BFGS")


# Example Seatbelts ------------------------------------------------------------

## Not run: 
# Seatbelts data
# See Durbin and Koopman (2012)

model_drivers <- SSModel(log(drivers) ~ SSMtrend(1, Q = list(NA))+
                           SSMseasonal(period = 12, sea.type = "trigonometric", Q = NA) +
                           log(PetrolPrice) + law, data = Seatbelts, H = NA)

# As trigonometric seasonal contains several disturbances which are all
# identically distributed, default behaviour of fitSSM is not enough,
# as we have constrained Q. We can either provide our own
# model updating function with fitSSM, or just use optim directly:

# option 1:
ownupdatefn <- function(pars, model){
  model$H[] <- exp(pars[1])
  diag(model$Q[, , 1]) <- exp(c(pars[2], rep(pars[3], 11)))
  model #for optim, replace this with -logLik(model) and call optim directly
}

fit_drivers <- fitSSM(model_drivers,
                      log(c(var(log(Seatbelts[, "drivers"])), 0.001, 0.0001)),
                      ownupdatefn, method = "BFGS")

out_drivers <- KFS(fit_drivers$model, smoothing = c("state", "mean"))
out_drivers
ts.plot(out_drivers$model$y, fitted(out_drivers), lty = 1:2, col = 1:2,
        main = "Observations and smoothed signal with and without seasonal component")
lines(signal(out_drivers, states = c("regression", "trend"))$signal,
      col = 4, lty = 1)
legend("bottomleft", col = c(1, 2, 4), lty = c(1, 2, 1),
       legend = c("Observations", "Smoothed signal", "Smoothed level"))

# Multivariate model with constant seasonal pattern,
# using the the seat belt law dummy only for the front seat passangers,
# and restricting the rank of the level component by using custom component

