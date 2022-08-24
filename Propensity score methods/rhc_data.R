rhc = read.csv("Propensity score methods/rhc.csv", header = TRUE, sep = ",")
head(rhc)

## a list of covariates we will use 
xvars <- c("ARF", "CHF", "Cirr", "colcan", "Coma", "lungcan",
           "MOSF", "sepsis", "age", "female")

############ problem set #############
## Fisher's exact test 
rhc[rhc$treatment==1,]
n11<-sum(rhc$treatment==1 & rhc$died==1)
n10<-sum(rhc$treatment==1 & rhc$died==0)
n01<-sum(rhc$treatment==0 & rhc$died==1)
n00<-sum(rhc$treatment==0 & rhc$died==0)

odds_table<-matrix(c(n11, n01, n10,n00), 2,2, dimnames=list(c("treated", "control"),c("Y=1", "Y=0")))

fisher.test(odds_table , alternative = "two.sided", conf.int=TRUE, conf.level=0.95)


## regression-adjusted estimate
glm.fit<-glm(died ~ treatment + ARF + CHF + Cirr + colcan + Coma + lungcan +
          MOSF + sepsis + age + female, 
            data=rhc, family=binomial(link = "logit"))


glm.fit2<-glm(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
               MOSF + sepsis + age + female, 
             data=rhc, family=binomial(link = "logit"))
## odd-ratio
exp(summary(glm.fit)$coefficients[2,1])

## confidence interval
exp(summary(glm.fit)$coefficients[2,1] - 1.96*summary(glm.fit)$coefficients[2,2])
exp(summary(glm.fit)$coefficients[2,1] + 1.96*summary(glm.fit)$coefficients[2,2])


## ATE
w.out1 <- weightit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                    MOSF + sepsis + age + female, data = rhc, estimand = "ATE")
summary(w.out1)
design.w <- svydesign(~1, weights = w.out1$weights, data = rhc)
fit.w <- svyglm(died ~ treatment +ARF + CHF + Cirr + colcan + Coma + lungcan +
                  MOSF + sepsis + age + female,
                design = design.w, family = quasibinomial())
## the target estimand w.out was ATE
exp(summary(fit.w)$coefficients[2,1])
exp(summary(fit.w)$coefficients[2,1] - 1.96*summary(fit.w)$coefficients[2,2])
exp(summary(fit.w)$coefficients[2,1] + 1.96*summary(fit.w)$coefficients[2,2])
exp(confint(fit.w)[2,])



## ATT
w.out2 <- weightit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                     MOSF + sepsis + age + female, data = rhc, estimand = "ATT")
summary(w.out2)
design.w <- svydesign(~1, weights = w.out2$weights, data = rhc)
fit.w <- svyglm(died ~ treatment +ARF + CHF + Cirr + colcan + Coma + lungcan +
                  MOSF + sepsis + age + female,
                design = design.w, family = quasibinomial())
## the target estimand w.out was ATE
exp(summary(fit.w)$coefficients[2,1])
exp(summary(fit.w)$coefficients[2,1] - 1.96*summary(fit.w)$coefficients[2,2])
exp(summary(fit.w)$coefficients[2,1] + 1.96*summary(fit.w)$coefficients[2,2])
exp(confint(fit.w)[2,])

library(MatchIt)
m.out <- matchit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                   MOSF + sepsis + age + female, data = rhc,
                 method = "nearest", ratio = 1)

fit.m <- glm(died ~ treatment + ARF + CHF + Cirr + colcan + Coma + lungcan +
               MOSF + sepsis + age + female,
             data = match.data(m.out), family = quasibinomial())
summary(fit.m)
exp(summary(fit.m)$coefficients[2,1])
exp(summary(fit.m)$coefficients[2,1] - 1.96*summary(fit.m)$coefficients[2,2])
exp(summary(fit.m)$coefficients[2,1] + 1.96*summary(fit.m)$coefficients[2,2])
exp(confint(fit.m)[2,])


######## step 1 ###########
library(tableone)
table1 <- CreateTableOne(vars = xvars, strata = "treatment", data = rhc)
## include standardized mean difference (SMD)
print(table1, smd = TRUE)

library(cobalt)
bal.tab(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
        MOSF + sepsis + age + female, data = rhc, estimand = "ATE")


#bal.tab(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
#          MOSF + sepsis + age + female, data = rhc, estimand = "ATE", 
#        m.threshold = 0.05)


######## steps 2-3 ###########
## full matching
library(MatchIt)

m.out <- matchit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                   MOSF + sepsis + age + female, data = rhc,
                 method = "full", estimand = "ATE") # it may take time..
print(summary(m.out, standardize = TRUE))

## weighting
library(WeightIt)
w.out <- weightit(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan +
                    MOSF + sepsis + age + female, data = rhc, estimand = "ATE")
summary(w.out)
  

######## step 4 ###########
bal.tab(m.out, stats = "m", thresholds = c(m = 0.1))
love.plot(m.out, binary = "std", thresholds = c(m = 0.1))

bal.tab(w.out, stats = "m", thresholds = c(m = 0.1))
love.plot(w.out, binary = "std", thresholds = c(m = 0.1))

######## step 5 ###########
fit.m <- glm(died ~ treatment +ARF + CHF + Cirr + colcan + Coma + lungcan +
               MOSF + sepsis + age + female,
            data = match.data(m.out), family = binomial())
summary(fit.m)
confint(fit.m)[2,]

library(survey)
design.w <- svydesign(~1, weights = w.out$weights, data = rhc)
fit.w <- svyglm(died ~ treatment +ARF + CHF + Cirr + colcan + Coma + lungcan +
                  MOSF + sepsis + age + female,
                design = design.w, family = binomial())
## the target estimand w.out was ATE
confint(fit.w)[2,]

