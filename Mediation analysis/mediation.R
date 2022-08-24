install.packages("mediation")
library(mediation)

# load the data 
data(framing) 
# treat: treatment
# emo: mediator
# cong_mesg: outcome 
# age, educ, gender, income: pre-treatment covariates

# fit a linear model for the mediator
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)

# fit a probit model for the outcome
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))

# use the `mediate` function to estimate 
# (1) the average causal mediation effects (AMCE): natural indirect effects
# (2) the average direct effects (ADE): natural direct effects
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                    robustSE= TRUE, sims = 100)
summary(med.out)
# total effect = ACMD (control) + ADE (treated)
# you can add an interaction term between the treatment and mediator to the outcome model

# sensitivity analysis
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)
plot(sens.out, sens.par = "rho")
