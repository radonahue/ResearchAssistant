#install.packages("mi")
library(mi)
library(MASS)
library(dplyr)
library(openxlsx)
library(sqldf)
library(tidyr)
library(stringr)
library(ggplot2)
library(egg)

#workforce dataframe
unzip("Workforce.zip")
load(file="ICPSR_37941/DS0005/37941-0005-Data.rda")
work<-da37941.0005

#centers dataframe
unzip("Centers.zip")
load(file="ICPSR_37941/DS0006/37941-0006-Data.rda")
cent<-da37941.0006

#main df
l<-left_join(cent, work, by="CB9_METH_CASEID")

#user guide pg 30, deleting public school records
l<-l%>%filter(WF9_METH_PUBSCHFLAG=="(0) WF spawned from a CB provider not associated with a Public School District"|is.na(WF9_METH_PUBSCHFLAG))%>%droplevels()

#creating HS Funding Flag
l$HSFunding<-ifelse(grepl("Head Start", l$CB9_RVNU_CENTER_FUND_COMBO.x), 1, 0)


#creating scaled variables for race
l$PCTWHITEScaled<-(l$WF9_CL6A_A_PRCNT_CHCLASS_WHITE/10)
l$PCTBLACKScaled<-(l$WF9_CL6A_B_PRCNT_CHCLASS_BLACK/10)
l$PCTASIANScaled<-(l$WF9_CL6A_C_PRCNT_CHCLASS_ASIAN/10)
l$PCTMIXEDScaled<-(l$WF9_CL6A_D_PRCNT_CHCLASS_MIXED/10)
l$PCTHISPScaled<-(l$WF9_CL6_PRCNT_CHCLASS_HISP/10)

#splitting dataframe up by age groups, p is prek i is infant
p<-l%>%filter(WF9_G_ACTIVITY_PK_G!="(-2) Valid Skip")%>%droplevels()
i<-l%>%filter(WF9_G_ACTIVITY_IT_G!="(-2) Valid Skip")%>%droplevels()

#regrouping teacher's hispanic variable to be binary
levels(p$WF9_CHAR_HISP)[levels(p$WF9_CHAR_HISP)=="(-1) Don't know/Refused/No Answer"] <-"(2) Not Hispanic or Latino"
levels(i$WF9_CHAR_HISP)[levels(i$WF9_CHAR_HISP)=="(-1) Don't know/Refused/No Answer"] <-"(2) Not Hispanic or Latino"

#just selecting the columns used in the model to play with data replacement algorithm
p2<-p%>%select(WF9_G_ACTIVITY_PK_G, HSFunding,CB9_REGION,CB9_COMM_POVERTY_DENSITY,CB9_COMM_URBAN_DENSITY,WF9_CHAR_RACE,WF9_CHAR_HISP,PCTWHITEScaled,PCTBLACKScaled,PCTASIANScaled,PCTMIXEDScaled,PCTHISPScaled, WF9_CL6A_A_PRCNT_CHCLASS_WHITE, WF9_CL6A_B_PRCNT_CHCLASS_BLACK, WF9_CL6A_C_PRCNT_CHCLASS_ASIAN, WF9_CL6A_D_PRCNT_CHCLASS_MIXED, WF9_CL6_PRCNT_CHCLASS_HISP, CB9_ENRL_PRCNTCH_HISP_0TO5, CB9_ENRL_PRCNTCH_NHWHITE_0TO5, CB9_ENRL_PRCNTCH_NHBLACK_0TO5, CB9_ENRL_PRCNTCH_NHASIAN_0TO5, CB9_ENRL_PRCNTCH_NHOTHER_0TO5)

p2$WF9_G_ACTIVITY_PK_G<-factor(p2$WF9_G_ACTIVITY_PK_G, levels=c("(6) Don't Know/Refused/No Answer", "(1) No time", "(2) 30 min or less", "(3) About one hour", "(4) About two hours", "(5) Three hours or more"))


#setting scaled to missing
p2$PCTWHITEScaled<-ifelse(sign(p2$PCTWHITEScaled)==-1, "",p2$PCTWHITEScaled)
p2$PCTBLACKScaled<-ifelse(sign(p2$PCTBLACKScaled)==-1, "",p2$PCTBLACKScaled)
p2$PCTASIANScaled<-ifelse(sign(p2$PCTASIANScaled)==-1, "",p2$PCTASIANScaled)
p2$PCTMIXEDScaled<-ifelse(sign(p2$PCTMIXEDScaled)==-1, "",p2$PCTMIXEDScaled)
p2$PCTHISPScaled<-ifelse(sign(p2$PCTHISPScaled)==-1, "",p2$PCTHISPScaled)


#Infant

i2<-i%>%select(WF9_G_ACTIVITY_IT_G, HSFunding,CB9_REGION,CB9_COMM_POVERTY_DENSITY,CB9_COMM_URBAN_DENSITY,WF9_CHAR_RACE,WF9_CHAR_HISP,PCTWHITEScaled,PCTBLACKScaled,PCTASIANScaled,PCTMIXEDScaled,PCTHISPScaled, WF9_CL6A_A_PRCNT_CHCLASS_WHITE, WF9_CL6A_B_PRCNT_CHCLASS_BLACK, WF9_CL6A_C_PRCNT_CHCLASS_ASIAN, WF9_CL6A_D_PRCNT_CHCLASS_MIXED, WF9_CL6_PRCNT_CHCLASS_HISP, CB9_ENRL_PRCNTCH_HISP_0TO5, CB9_ENRL_PRCNTCH_NHWHITE_0TO5, CB9_ENRL_PRCNTCH_NHBLACK_0TO5, CB9_ENRL_PRCNTCH_NHASIAN_0TO5, CB9_ENRL_PRCNTCH_NHOTHER_0TO5)

i2$WF9_G_ACTIVITY_IT_G<-factor(i2$WF9_G_ACTIVITY_IT_G, levels=c("(6) Don't Know/Refused/No Answer", "(1) No time", "(2) 30 min or less", "(3) About one hour", "(4) About two hours", "(5) Three hours or more"))

i2$PCTWHITEScaled<-ifelse(sign(i2$PCTWHITEScaled)==-1, "",i2$PCTWHITEScaled)
i2$PCTBLACKScaled<-ifelse(sign(i2$PCTBLACKScaled)==-1, "",i2$PCTBLACKScaled)
i2$PCTASIANScaled<-ifelse(sign(i2$PCTASIANScaled)==-1, "",i2$PCTASIANScaled)
i2$PCTMIXEDScaled<-ifelse(sign(i2$PCTMIXEDScaled)==-1, "",i2$PCTMIXEDScaled)
i2$PCTHISPScaled<-ifelse(sign(i2$PCTHISPScaled)==-1, "",i2$PCTHISPScaled)

#PreK w/o Don't know
p3<-p2%>%filter(WF9_G_ACTIVITY_PK_G!="(6) Don't Know/Refused/No Answer")%>%droplevels()

#Inf w/o Don't know
i3<-i2%>%filter(WF9_G_ACTIVITY_IT_G!="(6) Don't Know/Refused/No Answer")%>%droplevels()


