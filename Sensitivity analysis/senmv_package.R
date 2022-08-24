install.packages("sensitivitymv")
library(sensitivitymv)

data(erpcp) # DNA Damage Among Welders 

# Please see the details at: https://doi.org/10.1111/j.1541-0420.2006.00717.x

head(erpcp)
senmv(erpcp, gamma = 1)
senmv(erpcp, gamma = 3)
senmv(erpcp, gamma = 4)


senmv(erpcp, gamma = 2)
