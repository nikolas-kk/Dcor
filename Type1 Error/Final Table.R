library(kableExtra)
library(dplyr)
final_table <- t(as.data.frame(cbind(Beta_Res, Cauchy_Res, Gamma_Res
                                   , SkewNormal_Res, von_results)))
