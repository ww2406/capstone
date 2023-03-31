library(readxl)
library(plm)
library(tidyr)
library(dplyr)
library(broom)
library(stargazer)
library(haven)
library(knitr)
library(car)
library(foreach)
library(iterators)

#This file helps identify consistent agency participation over time, given a
#certain ending year and starting year
ucrorig<-read.csv("OneDrive/Documents/Capstone/ucr_csv.csv")
endingyear=2016
startingyear=2002

i=endingyear
while(i>(startingyear-1)){
  name=paste("y",i,sep = "")
  seq <- seq(1,nrow(ucrorig))
  seq[1:nrow(ucrorig)]<-NA
  ucrorig[name]<-seq
  ucrorig[,name][ucrorig$year==i] <- 1
  ucrorig[,name][ucrorig$year!=i] <- 0
  i <- i-1
}
j=endingyear-1
consist <- ucrorig%>%group_by(state_abb,ori9)%>%summarize(cy2002=sum(y2002),cy2003=sum(y2003),cy2004=sum(y2004),cy2005=sum(y2005),cy2006=sum(y2006),cy2007=sum(y2007),cy2008=sum(y2008),cy2009=sum(y2009),cy2010=sum(y2010),cy2011=sum(y2011),cy2012=sum(y2012),cy2013=sum(y2013),cy2014=sum(y2014),cy2015=sum(y2015),cy2016=sum(y2016))
consist<-consist[which(consist$ori9!=""),]
startconsist=paste("c",as.character(endingyear),sep="")
startconsist2=paste("cy",as.character(endingyear),sep="")
consist[startconsist]<-NA
consist[startconsist][consist[,startconsist2]==1]<-1
consist[startconsist][consist[,startconsist2]!=1]<-0
while(j>(startingyear-1)) {
  name=paste("cy",j,sep="")
  cname=paste("c",j,sep="")
  cnameprev=paste("c",j+1,sep="")
  consist[cname]<-NA
  consist[,cname][consist[,cnameprev]==1&consist[,name]==1]<-1
  consist[,cname][consist[,cnameprev]==0&consist[,name]==1]<-0
  consist[,cname][consist[,cnameprev]==1&consist[,name]==0]<-0
  consist[,cname][consist[,cnameprev]==0&consist[,name]==0]<-0
  j <- j-1
}
state.consist.ucr<-consist%>%group_by(state_abb)%>%summarize(cc2002=sum(c2002),cc2003=sum(c2003),cc2004=sum(c2004),cc2005=sum(c2005),cc2006=sum(c2006),cc2007=sum(c2007),cc2008=sum(c2008),cc2009=sum(c2009),cc2010=sum(c2010),cc2011=sum(c2011),cc2012=sum(c2012),cc2013=sum(c2013),cc2014=sum(c2014),cc2015=sum(c2015),cc2016=sum(c2016))

drugs <- read_dta("OneDrive/Documents/Capstone/Data/asr_drug_crimes_1980_2016.dta")
drugs<-zap_labels(drugs)
drugs<-zap_formats(drugs)
i=endingyear
drugs$year<-as.numeric(drugs$year)
while(i>(startingyear-1)){
  name=paste("y",i,sep = "")
  seq <- seq(1,nrow(drugs))
  seq[1:nrow(drugs)]<-NA
  drugs[name]<-seq
  drugs[drugs$year==i,name] <- 1
  drugs[drugs$year!=i,name] <- 0
  i <- i-1
}
j=endingyear-1
drugs <- drugs[which(drugs$number_of_months_reported==12),]
consist.drugs <- drugs%>%group_by(state_abb,ori9)%>%summarize(cy2002=sum(y2002),cy2003=sum(y2003),cy2004=sum(y2004),cy2005=sum(y2005),cy2006=sum(y2006),cy2007=sum(y2007),cy2008=sum(y2008),cy2009=sum(y2009),cy2010=sum(y2010),cy2011=sum(y2011),cy2012=sum(y2012),cy2013=sum(y2013),cy2014=sum(y2014),cy2015=sum(y2015),cy2016=sum(y2016))
consist.drugs<-consist.drugs[which(consist.drugs$ori9!=""),]
startconsist.drugs=paste("c",as.character(endingyear),sep="")
startconsist.drugs2=paste("cy",as.character(endingyear),sep="")
consist.drugs[startconsist.drugs]<-NA
consist.drugs[startconsist.drugs][consist.drugs[,startconsist.drugs2]==1]<-1
consist.drugs[startconsist.drugs][consist.drugs[,startconsist.drugs2]!=1]<-0
while(j>(startingyear-1)) {
  name=paste("cy",j,sep="")
  cname=paste("c",j,sep="")
  cnameprev=paste("c",j+1,sep="")
  consist.drugs[cname]<-NA
  consist.drugs[consist.drugs[,cnameprev]==1&consist.drugs[,name]==1,cname]<-1
  consist.drugs[,cname][consist.drugs[,cnameprev]==0&consist.drugs[,name]==1]<-0
  consist.drugs[,cname][consist.drugs[,cnameprev]==1&consist.drugs[,name]==0]<-0
  consist.drugs[,cname][consist.drugs[,cnameprev]==0&consist.drugs[,name]==0]<-0
  j <- j-1
}

state.consist.drugs<-consist.drugs%>%group_by(state_abb)%>%summarize(cc2002=sum(c2002),cc2003=sum(c2003),cc2004=sum(c2004),cc2005=sum(c2005),cc2006=sum(c2006),cc2007=sum(c2007),cc2008=sum(c2008),cc2009=sum(c2009),cc2010=sum(c2010),cc2011=sum(c2011),cc2012=sum(c2012),cc2013=sum(c2013),cc2014=sum(c2014),cc2015=sum(c2015),cc2016=sum(c2016))

# #pop consistency
# i=endingyear
# pop.drugs <- drugs[which(drugs$year>=startingyear&drugs$year<=endingyear),]
# pop.drugs.wide <- spread(pop.drugs,year,population)
# modifynames<-function(x){
#   result<-paste("p",x,sep="")
#   return(result)
# }
# names(pop.drugs.wide)[which(names(pop.drugs.wide)==as.character(endingyear)):which(names(pop.drugs.wide)==as.character(startingyear))] <- lapply(names(pop.drugs.wide)[which(names(pop.drugs.wide)==as.character(endingyear)):which(names(pop.drugs.wide)==as.character(startingyear))], modifynames)
# yearseq=seq(startingyear,endingyear)
# 
# yearspresent<-function(df,group_var,years,othervars){
#   #group_var<-enquos(group_var)
#   print(group_var)
#   args<-NULL
#   sumstring<-NULL
#   j<-1
#   foreach(i=first(years):last(years)) %do% {
#     print(i)
#     sumstring<-paste(sumstring,",cy",i,":=sum(y",i,")",sep="")
#     tempstr<-paste0("y",i)
#     print(sym(tempstr))
#     args[[j]]<-quo(sum(!!sym(tempstr)))
#     print(args[j])
#     names(args)[j]<-paste0("cy",i)
#     j <- j+1
#   }
#   foreach(i=iter(othervars)) %do% {
#     args[[j]]<-quo(sum(!!sym(i)))
#     names(args)[j]<-paste0("sum",i)
#     j<-j+1
#   }
#   print(args)
#   sumstring <- substring(sumstring,2)
#   print(sumstring)
#   df %>% group_by_at(.vars=group_var) %>% summarize(!!! args)
# }
# 
# lyears<-lapply(yearseq,function(x){return(paste0("p",x))})
# iyears<-lapply(lyears,function(x){return(which(names(pop.drugs.wide)==x))})
# cyears <- unlist(lyears,use.names=FALSE)
# 
# #yearsconsistent<-function(df,group_var,years)
# 
# pop.drugs.wide[cyears]<-lapply(pop.drugs.wide[cyears],function(x) replace(x, is.na(x),0))
# pres.pop.drugs.wide<-yearspresent(pop.drugs.wide,c("state_abb","ori9"),yearseq,names(pop.drugs.wide)[substring(names(pop.drugs.wide),1,1)=="p"])
# 
# pres.pop.drugs.wide <- pres.pop.drugs.wide[pres.pop.drugs.wide$ori9!="",]
# 
# consistpop<-function(df,group_var,var_stub,years){
#   args<-NULL
#   sumstring<-NULL
#   j<-1
#   foreach(i=first(years):last(years)) %do% {
#     print(i)
#     sumstring<-paste(sumstring,",cy",i,":=sum(y",i,")",sep="")
#     tempstr<-paste0(var_stub,i)
#     print(sym(tempstr))
#     args[[j]]<-quo(sum(!!sym(tempstr)))
#     print(args[j])
#     names(args)[j]<-paste0("sum",i)
#     j <- j+1
#   }
#   df %>% group_by_at(.vars=group_var) %>% summarize(!!! args)
# }
# pop.consist.drugs <- consistpop(pres.pop.drugs.wide,c("state_abb"),"sump",yearseq)
# 
# testing<-drugs%>%group_by(state_abb,year)%>%filter(number_of_months_reported==12)%>%summarize(spop=sum(population))
