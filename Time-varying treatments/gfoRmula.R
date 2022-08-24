install.packages("gfoRmula")
library(gfoRmula)
library(Hmisc)

# please refer to: https://www.rdocumentation.org/packages/gfoRmula/versions/0.3.1/topics/continuous_eofdata
data(continuous_eofdata)
str(continuous_eofdata)


id <- 'id'
time_name <- 't0'
covnames <- c('L1', 'L2', 'A')
outcome_name <- 'Y'
outcome_type <- 'continuous_eof'
covtypes <- c('categorical', 'normal', 'binary')
histories <- c(lagged)
histvars <- list(c('A', 'L1', 'L2'))
covparams <- list(covmodels = c(L1 ~ lag1_A + lag1_L1 + L3 + t0 +
                                  rcspline.eval(lag1_L2, knots = c(-1, 0, 1)),
                                L2 ~ lag1_A + L1 + lag1_L1 + lag1_L2 + L3 + t0,
                                A ~ lag1_A + L1 + L2 + lag1_L1 + lag1_L2 + L3 + t0))
ymodel <- Y ~ A + L1 + L2 + lag1_A + lag1_L1 + lag1_L2 + L3
intvars <- list('A', 'A')
interventions <- list(list(c(static, rep(0, 7))),
                      list(c(static, rep(1, 7))))
int_descript <- c('Never treat', 'Always treat')
nsimul <- 10000

gform_cont_eof <- gformula(obs_data = continuous_eofdata,
                           id = id, time_name = time_name,
                           covnames = covnames, outcome_name = outcome_name,
                           outcome_type = outcome_type, covtypes = covtypes,
                           covparams = covparams, ymodel = ymodel,
                           intvars = intvars, interventions = interventions,
                           int_descript = int_descript,
                           histories = histories, histvars = histvars,
                           basecovs = c("L3"), nsimul = nsimul, seed = 1234)
gform_cont_eof
