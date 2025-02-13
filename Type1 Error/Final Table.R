library(kableExtra)
library(dplyr)
final_table <- t(as.data.frame(cbind(Beta_Res, Cauchy_Res, Gamma_Res
                                   , SkewNormal_Res, von_results)))
final_table
final_table_latex <- kable(final_table, format = 'latex', booktabs = TRUE,row.names = FALSE) %>%
  kable_styling(full_width = TRUE)%>%
  pack_rows('Beta - Mixture2Normal',1,2)%>%
  pack_rows('Beta - Mixture3Normal',1,2)%>%
  pack_rows('Beta - Mixture2Skew.t',1,2)%>%
  pack_rows('Cauchy - Mixture2Normal',1,2)%>%
  pack_rows('Cauchy - Mixture3Normal',1,2)%>%
  pack_rows('Cauchy - Mixture2Skew.t',1,2)%>%
  pack_rows('Gamma - Mixture2Normal',1,2)%>%
  pack_rows('Gamma - Mixture3Normal',1,2)%>%
  pack_rows('Gamma - Mixture2Skew.t',1,2)%>%
  pack_rows('Skewed Normal - Mixture2Normal',1,2)%>%
  pack_rows('Skewed Normal - Mixture3Normal',1,2)%>%
  pack_rows('Skewed Normal - Mixture2Skew.t',1,2)%>%
  pack_rows('Von Mises - Mixture2Normal',1,2)%>%
  pack_rows('Von Mises - Mixture3Normal',1,2)%>%
  pack_rows('Von Mises - Mixture2Skew.t',1,2)
final_table_latex 
