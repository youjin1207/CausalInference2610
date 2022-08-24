install.packages("rddtools")
library(rddtools)

# https://cran.r-project.org/web/packages/rddtools/rddtools.pdf

data(house)
rdd_house <- rdd_data(x=x, y=y, data=house, cutpoint = 0)
summary(rdd_house)
plot(rdd_house)

## parametric estimation
# fit a 2nd order polynomial
reg_para <- rdd_reg_lm(rdd_object = rdd_house, order = 2)
plot(reg_para, ylim = c(0, 1.0))
summary(reg_para)

# fit a 4th order polynomial
reg_para <- rdd_reg_lm(rdd_object = rdd_house, order = 4)
plot(reg_para, ylim = c(0, 1.0))
summary(reg_para)

## nonparametri estimation
bw_ik <- rdd_bw_ik(rdd_house)
reg_nonpara <- rdd_reg_np(rdd_object = rdd_house, bw = bw_ik)
plot(reg_nonpara, ylim = c(0, 1.0))
summary(reg_nonpara)
