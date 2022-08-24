install.packages("ivreg")
library(ivreg)

# Data from Data from the U.S. National Longitudinal Survey of Young Men (NLSYM) in 1976 but using some variables dating back to earlier years.
# Please see the details here: https://www.rdocumentation.org/packages/ivreg/versions/0.6-1/topics/SchoolingReturns
data("SchoolingReturns")
head(SchoolingReturns)
# outcome: log(wage)
# treatment: education
# iv: nearcollege


# Table 5.1 in Verbeek (2004) / Table 2(1) in Card (1995)
# Add a quadratic terms in labor market 'experience'
# Returns to education: 7.4%
m_ols <- lm(log(wage) ~ education + poly(experience, 2, raw = TRUE) + ethnicity + smsa + south,
            data = SchoolingReturns)
summary(m_ols)

## Table 5.2 in Verbeek (2004) / similar to Table 3(1) in Card (1995)
m_red <- lm(education ~ nearcollege + poly(age, 2, raw = TRUE) + ethnicity + smsa + south,
            data = SchoolingReturns)
summary(m_red)

## Table 5.3 in Verbeek (2004) / similar to Table 3(5) in Card (1995)
## Returns to education: 13.3%
m_iv <- ivreg(log(wage) ~ education + poly(experience, 2, raw = TRUE) + ethnicity + smsa + south |
                nearcollege + poly(age, 2, raw = TRUE) + ethnicity + smsa + south,
              data = SchoolingReturns)
summary(m_iv)
