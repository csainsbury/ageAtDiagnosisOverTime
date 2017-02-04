
library(data.table)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

diagnosisSet<-paste("../GlCoSy/SDsource/diagnosisDateDeathDate.txt",sep="")
diagnosisSetDF<-read.csv(diagnosisSet); diagnosisSetDT<-data.table(diagnosisSetDF)

diagnosisSetDT$birthDateUnix<-returnUnixDateTime(diagnosisSetDT$BirthDate)
diagnosisSetDT$diagnosisDateUnix<-returnUnixDateTime(diagnosisSetDT$DateOfDiagnosisDiabetes_Date)
  diagnosisSetDT$ageAtDiagnosis<-diagnosisSetDT$diagnosisDateUnix-diagnosisSetDT$birthDateUnix
  diagnosisSetDT$ageAtDiagnosisYears<-diagnosisSetDT$ageAtDiagnosis/(60*60*24*365.25)

# diabetes type of interest
diagnosisSetDT_T2<-diagnosisSetDT[DiabetesMellitusType_Mapped=="Type 2 Diabetes Mellitus"]
diagnosisSetDT_T1<-diagnosisSetDT[DiabetesMellitusType_Mapped=="Type 1 Diabetes Mellitus"]

## type 2 
# remove those with a false diagnosis date of 1-1-1900
diagnosisSetDT_T2<-diagnosisSetDT_T2[ageAtDiagnosisYears>0]
# remove the early data (lower quality)
diagnosisSetDT_T2<-diagnosisSetDT_T2[diagnosisSetDT_T2$diagnosisDateUnix>(returnUnixDateTime("2016-01-01")-(60*60*24*365.25*15))]

# diagnosisSetDT_T2<-diagnosisSetDT_T2[diagnosisSetDT_T2$diagnosisDateUnix>9.2e+08]

T2YearData<-boxplot(diagnosisSetDT_T2$ageAtDiagnosisYears ~ cut(diagnosisSetDT_T2$diagnosisDateUnix,breaks=seq(min(diagnosisSetDT_T2$diagnosisDateUnix),max(diagnosisSetDT_T2$diagnosisDateUnix),(365.25*24*60*60))),las=3,varwidth=T)

plot(diagnosisSetDT_T2$diagnosisDateUnix,diagnosisSetDT_T2$ageAtDiagnosisYears,pch=16,cex=0.3)
fit<-lm(diagnosisSetDT_T2$ageAtDiagnosisYears ~ diagnosisSetDT_T2$diagnosisDateUnix)
abline(fit,col="red")

#project 20y
plot(diagnosisSetDT_T2$diagnosisDateUnix,diagnosisSetDT_T2$ageAtDiagnosisYears,pch=16,cex=0.3,xlim=c(min(diagnosisSetDT_T2$diagnosisDateUnix),max(diagnosisSetDT_T2$diagnosisDateUnix)+(20*60*60*24*365.25)))
fit<-lm(diagnosisSetDT_T2$ageAtDiagnosisYears ~ diagnosisSetDT_T2$diagnosisDateUnix)
abline(fit,col="red")


## type 1
# remove those with a false diagnosis date of 1-1-1900
diagnosisSetDT_T1<-diagnosisSetDT_T1[ageAtDiagnosisYears>0]
# remove the early data (lower quality)
diagnosisSetDT_T1<-diagnosisSetDT_T1[diagnosisSetDT_T1$diagnosisDateUnix>(returnUnixDateTime("2016-01-01")-(60*60*24*365.25*15))]

T1YearData<-boxplot(diagnosisSetDT_T1$ageAtDiagnosisYears ~ cut(diagnosisSetDT_T1$diagnosisDateUnix,breaks=seq(min(diagnosisSetDT_T1$diagnosisDateUnix),max(diagnosisSetDT_T1$diagnosisDateUnix),(365.25*24*60*60))),las=3,varwidth=T)