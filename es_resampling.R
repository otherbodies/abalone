# bootES calculating all in this manner.. 
library("bootES", lib.loc="~/R/win-library/3.2")

data_for_es$type = as.factor(data_for_es$type)
data_for_es = data_for_es[-11,]

##

es = bootES(data = data_for_es,data.col = "suis", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "suis2", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "osiq_vis", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "osiq_spat", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "bsfq_calendar", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "bsfq_number", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "age", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

##

es = bootES(data = data_for_es,data.col = "suis", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "suis2", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "osiq_vis", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "osiq_spat", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "bsfq_calendar", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "bsfq_number", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "age", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es


##

##

es = bootES(data = data_for_es,data.col = "a", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "b", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "c", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "d", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "e", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "g", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es,data.col = "h", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

##

es = bootES(data = data_for_es,data.col = "a", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "b", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "c", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "d", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "e", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "g", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es,data.col = "h", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

##
data_for_es2$type = as.factor(data_for_es2$type)
data_for_es2$task = as.factor(data_for_es2$task)

data_for_es2$dif_trunk = data_for_es2$trunk2- data_for_es2$trunk
data_for_es2$dif_head = data_for_es2$head2- data_for_es2$head
data_for_es2$dif_room = data_for_es2$room2- data_for_es2$room
data_for_es2$dif_fit = data_for_es2$fit2- data_for_es2$fit
data_for_es2$dif_ratio = data_for_es2$ratio2- data_for_es2$ratio

data_for_es2_syn = subset(data_for_es2,type=="syn")
data_for_es2_con = subset(data_for_es2,type=="control")

es = bootES(data = data_for_es2_syn,data.col = "dif_trunk", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_trunk", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_head", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_head", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_room", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_room", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_fit", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_fit", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_ratio", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "dif_ratio", effect.type = "cohens.d")
es

#

es = bootES(data = data_for_es2_con,data.col = "dif_trunk", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "dif_trunk", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_con,data.col = "dif_head", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "dif_head", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_con,data.col = "dif_room", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "dif_room", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_con,data.col = "dif_fit", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "dif_fit", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_con,data.col = "dif_ratio", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "dif_ratio", effect.type = "cohens.d")
es


## 2d consistency
X2dconsistency$group = factor(X2dconsistency$group)

es = bootES(data = X2dconsistency,data.col = "2Dmean", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = X2dconsistency,data.col = "2Dmean", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = X2dconsistency,data.col = "2Dxaxis", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = X2dconsistency,data.col = "2Dxaxis", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = X2dconsistency,data.col = "2Dyaxis", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = X2dconsistency,data.col = "2Dyaxis", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es
