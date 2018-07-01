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
data_for_es2$diff_ang_str_hd = data_for_es2$angle_str_hd_cr- data_for_es2$angle_str_hd_mon
data_for_es2$diff_ang_str_tr = data_for_es2$angle_str_tr_cr- data_for_es2$angle_str_tr_mon

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


es = bootES(data = data_for_es2_syn,data.col = "diff_ang_str_hd", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "diff_ang_str_hd", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_syn,data.col = "diff_ang_str_hd", effect.type = "unstandardized")
es


es = bootES(data = data_for_es2_syn,data.col = "diff_ang_str_tr", effect.type = "r")
es

es = bootES(data = data_for_es2_syn,data.col = "diff_ang_str_tr", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_syn,data.col = "diff_ang_str_tr", effect.type = "unstandardized")
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


es = bootES(data = data_for_es2_con,data.col = "diff_ang_str_hd", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "diff_ang_str_hd", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_con,data.col = "diff_ang_str_hd", effect.type = "unstandardized")
es


es = bootES(data = data_for_es2_con,data.col = "diff_ang_str_tr", effect.type = "r")
es

es = bootES(data = data_for_es2_con,data.col = "diff_ang_str_tr", effect.type = "cohens.d")
es

es = bootES(data = data_for_es2_con,data.col = "diff_ang_str_tr", effect.type = "unstandardized")
es


# between subject syn vs controls crazy
es = bootES(data = data_for_es2,data.col = "trunk2", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "trunk2", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "trunk2", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "head2", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "head2", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "head2", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "room2", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "room2", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "room2", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "fit2", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "fit2", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "fit2", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "ratio2", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "ratio2", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "ratio2", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "angle_str_hd_cr", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "angle_str_hd_cr", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "angle_str_hd_cr", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "angle_str_tr_cr", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "angle_str_tr_cr", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "angle_str_tr_cr", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es



es = bootES(data = data_for_es2,data.col = "angle_str_hd_mon", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "angle_str_hd_mon", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "angle_str_hd_mon", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
es


es = bootES(data = data_for_es2,data.col = "angle_str_tr_mon", group.col = "type", contrast = c("control", "syn"), effect.type = "cohens.d")
es

es = bootES(data = data_for_es2,data.col = "angle_str_tr_mon", group.col = "type", contrast = c("control", "syn"), effect.type = "r")
es

es = bootES(data = data_for_es2,data.col = "angle_str_tr_mon", group.col = "type", contrast = c("control", "syn"), effect.type = "unstandardized")
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


# 2d consistency for columns with 2 participants data changed

es = bootES(data = X2dconsistency,data.col = "2Dmean2", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = X2dconsistency,data.col = "2Dmean2", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = X2dconsistency,data.col = "2Dxaxis2", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = X2dconsistency,data.col = "2Dxaxis2", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = X2dconsistency,data.col = "2Dyaxis2", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = X2dconsistency,data.col = "2Dyaxis2", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es


##3d consistency
consistency3d_forES$group = factor(consistency3d_forES$group)

es = bootES(data = consistency3d_forES,data.col = "Head.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "Head.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "Head.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "Head.months", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "Head.months", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "Head.months", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "Straight.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "Straight.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "Straight.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "Straight.months", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "Straight.months", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "Straight.months", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "Trunk.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "Trunk.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "Trunk.crazy", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "Trunk.months", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "Trunk.months", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "Trunk.months", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "main.mini.straight", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "main.mini.straight", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "main.mini.straight", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES,data.col = "main.mini.trunk", group.col = "group", contrast = c("c", "s"), effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES,data.col = "main.mini.trunk", group.col = "group", contrast = c("c", "s"), effect.type = "r")
es

es = bootES(data = consistency3d_forES,data.col = "main.mini.trunk", group.col = "group", contrast = c("c", "s"), effect.type = "unstandardized")
es

## 3d consistency within subject crazy vs months
consistency3d_forES$diff_head = consistency3d_forES$Head.crazy - consistency3d_forES$Head.months
consistency3d_forES$diff_trunk = consistency3d_forES$Trunk.crazy - consistency3d_forES$Trunk.months
consistency3d_forES$diff_straight = consistency3d_forES$Straight.crazy - consistency3d_forES$Straight.months

consistency3d_forES_syn = subset(consistency3d_forES,group=="s")
consistency3d_forES_con = subset(consistency3d_forES,group=="c")

# syn

es = bootES(data = consistency3d_forES_syn,data.col = "diff_head", effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES_syn,data.col = "diff_head", effect.type = "r")
es

es = bootES(data = consistency3d_forES_syn,data.col = "diff_head", effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES_syn,data.col = "diff_straight", effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES_syn,data.col = "diff_straight", effect.type = "r")
es

es = bootES(data = consistency3d_forES_syn,data.col = "diff_straight", effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES_syn,data.col = "diff_trunk", effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES_syn,data.col = "diff_trunk", effect.type = "r")
es

es = bootES(data = consistency3d_forES_syn,data.col = "diff_trunk", effect.type = "unstandardized")
es

#con

es = bootES(data = consistency3d_forES_con,data.col = "diff_head", effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES_con,data.col = "diff_head", effect.type = "r")
es

es = bootES(data = consistency3d_forES_con,data.col = "diff_head", effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES_con,data.col = "diff_straight", effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES_con,data.col = "diff_straight", effect.type = "r")
es

es = bootES(data = consistency3d_forES_con,data.col = "diff_straight", effect.type = "unstandardized")
es


es = bootES(data = consistency3d_forES_con,data.col = "diff_trunk", effect.type = "cohens.d")
es

es = bootES(data = consistency3d_forES_con,data.col = "diff_trunk", effect.type = "r")
es

es = bootES(data = consistency3d_forES_con,data.col = "diff_trunk", effect.type = "unstandardized")
es