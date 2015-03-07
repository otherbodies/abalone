sds2d = aggregate(data2d[c("mouse_x","mouse_y")],data2d[c("month_no","user_id")],sd)
lengths2d = aggregate(data2d[c("mouse_x","mouse_y")],data2d[c("month_no","user_id")],length)
means_sds2d = aggregate(sds2d[c("mouse_x","mouse_y")],sds2d[c("user_id")],mean)



write.table(means_sds2d, file = "sds2d.csv", sep=";", row.names = FALSE)

library(xlsx)
write.xlsx(means_sds2d, "sds2d.xlsx")

#means_sds2d manually corrected

didall2d = subset(means_sds2d,didall=="yes")
t.test(didall2d$mouse_x ~ didall2d$type)
t.test(didall2d$mouse_y ~ didall2d$type)


write.xlsx(mainTable, "maintable.xlsx")
