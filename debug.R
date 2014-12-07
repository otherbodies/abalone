testForDifferences = function(dataVar,dataVar2,axis){
  assign("temp",dataVar)
  assign("temp2",dataVar2)
  
  testResults = data.frame(participant=character(24),axis=character(24),statistic=numeric(24),pvalue=numeric(24),cohenD=numeric(24),estimate=numeric(24),stringsAsFactors = F)
  
  for(i in 1:24){
    
    test = t.test(temp[,i],temp2[,i],paired = T)
    cohen = cohensD(temp[,i],temp2[,i],method="paired")
    
    testResults$participant[i]=colnames(diff_z_month_str_hd)[i]
    testResults$axis = axis
    testResults$statistic[i]=test$statistic
    testResults$pvalue[i]=format(test$p.value,scientific=FALSE)
    testResults$cohenD[i]=cohen
    testResults$estimate[i]=test$estimate
    
    return(testResults)
  }
}

tResultsMonthX = testForDifferences(diff_X_month_str_hd,diff_X_month_str_tr,"X")