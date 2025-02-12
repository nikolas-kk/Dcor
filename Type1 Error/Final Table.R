library(kableExtra)

final_table <- as.data.frame(cbind(Beta_Res, Cauchy_Res, Gamma_Res
                                   , SkewNormal_Res, von_results))
final_table
for (i in 1:15){
  cname<-paste0("combined",i)
  final_table[[cname]]<-paste0('(',final_table[[2*i-1]],',',final_table[[2*i]],')')
}
final_table<-final_table[,c(31:45)]

kable(final_table,'latex',booktabs=TRUE)
#Το υπόλοιπο θα το φτιάξω στο Latex και θα βάλω τον κώδικα εδώ