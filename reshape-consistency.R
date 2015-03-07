#reshaping pooledsds table
library("plyr", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")

conTable1 = dcast(pooledsds4, participant + task ~ condition, value.var = "overall")
conTable2 = dcast(pooledsds4, participant + task ~ condition, value.var = "pooled_z")
conTable3 = dcast(pooledsds4, participant + task ~ condition, value.var = "pooled_X")
conTable4 = dcast(pooledsds4, participant + task ~ condition, value.var = "pooled_y")

conTable = merge(conTable1, conTable2, by=c("participant","task"))
conTable = merge(conTable, conTable3, by=c("participant","task"))
conTable = merge(conTable, conTable4, by=c("participant","task"))
colnames(conTable)=c("participant","task","overall_hd","overall_str","overall_tr","pooled_z_hd","pooled_z_str","pooled_z_tr","pooled_X_hd","pooled_X_str","pooled_X_tr","pooled_y_hd","pooled_y_str","pooled_y_tr")


# second part of doing long to wide format, in a loop this time

conTableA = dcast(conTable, participant ~ task, value.var = colnames(conTable)[3])
conTableM = conTableA["participant"]
conTableM = merge(conTableM,conTableA,by="participant")

for(i in 4:ncol(conTable)){
  conTableA = dcast(conTable, participant ~ task, value.var = colnames(conTable)[i])
  conTableM = merge(conTableM,conTableA,by="participant")
}

colnames(conTableM)=c("participant","overall_hd_cr","overall_hd_m","overall_str_cr","overall_str_m","overall_tr_cr","overall_tr_m","pooled_z_hd_cr","pooled_z_hd_m",
                      "pooled_z_str_cr","pooled_z_str_m","pooled_z_tr_cr","pooled_z_tr_m","pooled_X_hd_cr","pooled_X_hd_m","pooled_X_str_cr","pooled_X_str_m",
                      "pooled_X_tr_cr","pooled_X_tr_m","pooled_y_hd_cr","pooled_y_hd_m","pooled_y_str_cr","pooled_y_str_m","pooled_y_tr_cr","pooled_y_tr_m")

write.table(conTableM, file = "consistency-wide-format.csv", sep=";", row.names = FALSE)
