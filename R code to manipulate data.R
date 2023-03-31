library(readxl)
library(plm)
library(tidyr)
library(dplyr)
library(broom)
library(stargazer)
library(haven)
library(knitr)
library(car)
data <- read_excel("OneDrive/Documents/Capstone/Data.xlsx")

endingyear=2013
startingyear=2008
yearseq<-seq(startingyear,endingyear)

ucr.base <- read_dta("Downloads/ucr_offenses_known_yearly_1960_2017_dta/ucr_offenses_known_yearly_1960_2017.dta")

#Let's subset the UCR data in R instead of Stata to get more flexibility

#Let's start with getting a listing of consistent ORIs

ucr.base<-subset(ucr.base,year>=startingyear&year<=endingyear)
ucr.base<-subset(ucr.base,last_month_reported=="december is the last month reported")
i=endingyear
yearstr<-c()
while(i>(startingyear-1)){
  name=paste("y",i,sep = "")
  seq <- seq(1,nrow(ucr.base))
  seq[1:nrow(ucr.base)]<-NA
  ucr.base[,name]<-seq
  #ucr.base[,name][ucr.base$year==i] <- 1
  ucr.base[ucr.base$year==i,name] <- 1
  ucr.base[ucr.base$year!=i,name] <- 0
  yearstr=c(yearstr,name)
  i <- i-1
}
ucr.baseconsist<-ucr.base%>%group_by(ori9,state)%>%select(ori9,state,yearstr)%>%summarize_all(sum)
ucr.baseconsist <- ucr.baseconsist[ucr.baseconsist$ori9!="",]
maxcol=length(yearseq)+2
ucr.baseconsist$numconsist<-rowSums(ucr.baseconsist[,3:maxcol])
ucr.baseconsist <- subset(ucr.baseconsist,ucr.baseconsist$numconsist==length(yearseq))
ucr.baseconsist <- ucr.baseconsist[,1]
ucr.consist <- subset(ucr.base, ucr.base$ori9 %in% ucr.baseconsist$ori9)
out <- ucr.consist
#ucr.baseconsist<-ucr.baseconsist%>%mutate(numconsist=Reduce("+",.[3:17]))

#out <- read.csv("OneDrive/Documents/Capstone/Data/ucr_revised.csv")
admits_ct <- read.csv("OneDrive/Documents/Capstone/Data/NPS_Report_22-Oct-18_07_22_37PM.csv")
privprison_ct <- read.csv("OneDrive/Documents/Capstone/Data/Total Priv Prison/Total-Table 1.csv")
#privprison_ct<-read.csv("OneDrive/Documents/Capstone/Data/In State Private Prisons.csv")
sentlength<-read_dta("OneDrive/Documents/Capstone/Data/Incarc_data.dta")
stateideol<-read_dta("OneDrive/Documents/Capstone/Data/stateideology_v2018.dta")
drugs <- read_dta("OneDrive/Documents/Capstone/Data/asr_drug_crimes_1980_2016.dta")
fec.2002<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2001-2002.csv")
fec.2004<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2003-2004.csv")
fec.2006<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2005-2006.csv")
fec.2008<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2007-2008.csv")
fec.2010<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2009-2010.csv")
fec.2012<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2011-2012.csv")
fec.2014<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2013-2014.csv")
fec.2016<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/FEC PAC 2015-2016.csv")

#This is the part that reads in Median Income Data
Median_Income_in_2017_Dollars <- read_excel("OneDrive/Documents/Capstone/Data/Median Income in 2017 Dollars.xlsx", 
                                            col_types = c("text", "text", "blank", "text", "blank", "text", 
                                                          "blank", "text", "blank", "text", "blank", 
                                                          "text", "blank", "text", "blank", 
                                                          "text", "blank", "text", "blank", 
                                                          "text", "blank", "text", "blank", 
                                                          "text", "blank", "text", "blank", 
                                                          "text", "blank", "text", "blank", 
                                                          "text", "blank"))
long_medincome <- gather(Median_Income_in_2017_Dollars,Year,MedIncome,2:17)
long_medincome <- long_medincome[-c(1:2),]
long_medincome <- long_medincome %>% rename(state.name=State,year=Year)

#This part reads in UCR data and joins with median income data and admissions
#data
###The below lines were taken care of in the revised first part above
#out$allyear[out$last_month_reported=="december is the last month reported"] <- 1
#out$allyear[out$last_month_reported!="december is the last month reported"] <- 0
#out<-subset(out,allyear==1)
###The below line is unnecessary
# test<-out%>%group_by(state_abb,year)%>%summarize(summurder=sum(act_murder),summanslaughter=sum(act_manslaughter),sumrape=sum(act_rape_total),sumrobbery=sum(act_robbery_total),sumassault=sum(act_aggravated_assault),sumburglary=sum(act_burglary_total),sumtheft=sum(act_theft_total),summv=sum(act_mtr_vhc_theft_total))
drugs<-drugs[-c(1,5:8,10:23)]
drugs <- drugs[which(drugs$number_of_months_reported==12),]
drugs<-drugs[-c(3)]
drugs <- drugs[which(!is.na(drugs$total_drug_tot_arrests)),]
out <- inner_join(out,drugs,by=c("year","ori9","state_abb"))
out<-out[which(!is.na(out$total_drug_tot_arrests)),]
##from prev place below BEGIN
outconsist <- out%>%group_by(ori9,state_abb)%>%summarize(consistyears=n())
out <- inner_join(outconsist,out,by=c("ori9","state_abb"))
out<-out[which(out$consistyears==length(yearseq)),]
out$consistyears <- NULL
##from prev place below END
ucr <- out %>% select(state_abb,year,act_aggravated_assault,act_rape_total,
                      act_robbery_total,act_manslaughter,act_murder,
                      act_burglary_total,act_theft_total,act_mtr_vhc_theft_total,
                      population_1,tot_clr_aggravated_assault,tot_clr_rape_total,
                      tot_clr_robbery_total,tot_clr_manslaughter,tot_clr_murder,
                      tot_clr_burglary_total,tot_clr_theft_total,
                      tot_clr_mtr_vhc_theft_total,total_drug_tot_arrests) %>% 
  filter(!(state_abb %in% c("CZ", "PR", "GU"))) %>% group_by(state_abb,year) %>%
  summarize(crime_viol=sum(act_manslaughter)+sum(act_murder)+
              sum(act_aggravated_assault)+sum(act_rape_total)+sum(act_robbery_total),
            crime_prop=sum(act_mtr_vhc_theft_total)+sum(act_theft_total)+
              sum(act_burglary_total),crime_drugs=sum(total_drug_tot_arrests),
            crime_index=crime_viol+crime_prop, crime=crime_viol+crime_prop+crime_drugs,
            agency_pop=sum(population_1),clr_viol=sum(tot_clr_manslaughter)+
              sum(tot_clr_murder)+sum(tot_clr_aggravated_assault)+
              sum(tot_clr_rape_total)+sum(tot_clr_robbery_total),
            clr_prop=sum(tot_clr_mtr_vhc_theft_total)+sum(tot_clr_theft_total)+
              sum(tot_clr_burglary_total),clr_total=clr_viol+clr_prop,
            clrrate_viol_new=clr_viol/crime_viol,clrrate_prop_new=clr_prop/crime_prop,
            clrrate_total_new=clr_total/crime)
states<-ucr%>%group_by(state_abb)%>%summarize(count=n())
##prev place BEGIN
#These items taken care of above now
# ucrconsist <- ucr%>%group_by(state_abb)%>%summarize(consistyears=n())
# ucr <- inner_join(ucrconsist,ucr,by=c("state_abb"))
# ucr<-ucr[which(ucr$consistyears==length(yearseq)),]
# ucr$consistyears <- NULL
##prev place END
ucr<-ucr %>% rename(state=state_abb)
combined1<-inner_join(data,ucr,by=c("year","state"))
dc<-data.frame("D.C.","DC")
names(dc)<-c("state.name","state.abb")
state_abbrev<-data.frame(state.name,state.abb)
state_abbrev<-rbind(state_abbrev,dc)
state_abbrev<-state_abbrev%>%rename(state=state.abb)
medincome.state<-inner_join(long_medincome,state_abbrev,by="state.name")
medincome.state <- transform(medincome.state, year=as.numeric(year), MedIncome=as.numeric(MedIncome))
combined2<-inner_join(combined1,medincome.state,by=c("year","state"))
names(admits_ct)[2:16]<-c(2002:2016)
admits_ct<-admits_ct%>%gather(year,admits_ct,2:16)
admits_ct<-transform(admits_ct,year=as.numeric(year))
admits_ct<-inner_join(state_abbrev,admits_ct,by="state.name")
#names(admits_ct)[1]<-"state"
combined3<-inner_join(combined2,admits_ct,by=c("year","state"))
combined3$state.name.x<-NULL
combined3$state.name.y <- NULL

#Bring in state ideology data
stateideol$state <- NULL
names(stateideol)[names(stateideol)=="statename"]="state.name"
stateideol <- inner_join(stateideol,state_abbrev,by="state.name")
stateideol <- stateideol[which(stateideol$year>=2002&stateideol$year<=2016),]
combined3a<-inner_join(stateideol,combined3,by=c("year","state"))
combined3a <- combined3a[,c(1,2,5:54,3,4)]

#This joins combined3a with data indicating whether a private prison was present
#in a state in a given year
names(privprison_ct)[2:16]<-c(2002:2016)
privprison_ct<-privprison_ct%>%gather(year,privprison_ct,2:16)
privprison_ct<-transform(privprison_ct,year=as.numeric(year))
privprison_ct<-inner_join(state_abbrev,privprison_ct,by="state.name")
privprison_ct$X<-NULL
privprison_ct$privprispresent[privprison_ct$privprison_ct>0]<-1
privprison_ct$privprispresent[privprison_ct$privprison_ct==0]<-0
privprison_ct <- subset(privprison_ct,year>=startingyear&year<=endingyear)
numprivpris<-privprison_ct%>%group_by(state)%>%summarize(totalpriv=sum(privprispresent))
privprison_ct<-inner_join(privprison_ct,numprivpris,by="state")
privprison_ct$privprisconsistent[privprison_ct$totalpriv==length(yearseq)]<-1
privprison_ct$privprisconsistent[privprison_ct$totalpriv<length(yearseq)]<-0
combined4<-inner_join(combined3a,privprison_ct,by=c("year","state"))
combined4$state.name<-NULL
combined4a<-subset(combined4,privprisconsistent!=0)

#This joins combined4 with data indicating whether sentence length data is
#available for the whole time period and subsets the output
# names(sentlength)[4]<-"state.name"
# sentlength<-transform(sentlength,year=as.numeric(year))
# sentlength<-na.omit(sentlength)
# sentlength.filled<-complete(sentlength,state,year=full_seq(year,1))
# sentlength.filled$present[!(is.na(sentlength.filled$mpsent))]<-1
# sentlength.filled$present[(is.na(sentlength.filled$mpsent))]<-0
# sentlength.consist<-sentlength.filled%>%group_by(state)%>%summarize(totalpres=sum(present))
# sentlength.consist$sentconsist[sentlength.consist$totalpres==15] <- 1
# sentlength.consist$sentconsist[sentlength.consist$totalpres<15] <- 0
# temp<-inner_join(combined4,sentlength.consist,by=c("state"))
# combined5<-inner_join(temp,sentlength,by=c("year","state"))
# combined5<-subset(combined5,sentconsist!=0)
# 
# #This subsets combined5 to exclude those states without consistent presence
# #of private prisons
# combined5<-subset(combined5,privprisconsistent!=0)
# 
# #Create some logged variables in combined5
# combined5$logjt=log(combined5$juris_total)
# combined5$logpop=log(combined5$pop)
# combined5$logspend3201=log(combined5$spending+3201)
# combined5$logcv=log(combined5$crime_viol)
# combined5$logcp=log(combined5$crime_prop)
# combined5$loglaw=log(combined5$lawenf_officers)
# combined5$logsent=log(combined5$mpsent)
# 
#Prepare combined5 for output to Stata
#***REMEMBER TO REMOVE LINE BELOW IF REINTRODUCE SENTLENGTH
combined5<-combined4a
combined5$state.name<-NULL
combined5$STATE<-NULL

#Prepare the FEC datafile
fec<-rbind(fec.2002,fec.2004,fec.2006,fec.2008,fec.2010,fec.2012,fec.2014,fec.2016)
fec.out<-subset(fec,recipient_committee_id==""&
                  disbursement_type_description!="CONTRIBUTION MADE TO NON-AFFILIATED"&
                  disbursement_description!="BANK FEES"&disbursement_description!="SERVICE FEE"&
                  disbursement_description!="FEES")
write.csv(fec.out,"OneDrive/Documents/Capstone/Data/FEC PAC/fecout.csv")
#Modify the output file to remove cancelled contributions and negative contrib
#with no corresponding positive contrib, as well as refunds
#
#Reinput
fec.in<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/fecin.csv")
#Just to make sure, and to get rid of all the fees (the above did not)
fec.in<-subset(fec.in, disbursement_type_description!="CONTRIBUTION MADE TO NON-AFFILIATED"&
                  !grepl(pattern = "[A-Z]*FEE[A-Z]*", x = disbursement_description))
fec.in <- fec.in%>%filter(!(recipient_state %in% c("CZ", "PR", "GU")))
fec.final<-fec.in%>%group_by(report_year,recipient_state)%>%summarize(totpac=sum(disbursement_amount))
names(fec.final)=c("year","state","totpac")
combined6<-left_join(combined5,fec.final,by=c("year","state"))
combined6$totpac[is.na(combined6$totpac)]<-0
combined6$spending=combined6$contributions+combined6$lobbying+combined6$totpac
combined6$logspend=log(combined6$spending+1)
combined6$state.name<-NULL

#Prepare the FEC datafile for the non-sentence consistent block
fec<-rbind(fec.2002,fec.2004,fec.2006,fec.2008,fec.2010,fec.2012,fec.2014,fec.2016)
fec.out<-subset(fec,recipient_committee_id==""&
                  disbursement_type_description!="CONTRIBUTION MADE TO NON-AFFILIATED"&
                  disbursement_description!="BANK FEES"&disbursement_description!="SERVICE FEE"&
                  disbursement_description!="FEES")
write.csv(fec.out,"OneDrive/Documents/Capstone/Data/FEC PAC/fecout.csv")
#Modify the output file to remove cancelled contributions and negative contrib
#with no corresponding positive contrib, as well as refunds
#
#Reinput
fec.in<-read.csv("OneDrive/Documents/Capstone/Data/FEC PAC/fecin.csv")
#Just to make sure, and to get rid of all the fees (the above did not)
fec.in<-subset(fec.in, disbursement_type_description!="CONTRIBUTION MADE TO NON-AFFILIATED"&
                 !grepl(pattern = "[A-Z]*FEE[A-Z]*", x = disbursement_description))
fec.in <- fec.in%>%filter(!(recipient_state %in% c("CZ", "PR", "GU")))
fec.final<-fec.in%>%group_by(report_year,recipient_state)%>%summarize(totpac=sum(disbursement_amount))
names(fec.final)=c("year","state","totpac")
combined4b<-left_join(combined4a,fec.final,by=c("year","state"))
combined4b$totpac[is.na(combined4b$totpac)]<-0
combined4b$spending=combined4b$contributions+combined4b$lobbying+combined4b$totpac
combined4b$logspend=log(combined4b$spending+1)

combined6$state.name.y<-NULL
names(combined6)[names(combined6)=="state.name.x"] <- "state_name"
write_dta(combined6,"OneDrive/Documents/Capstone/consistent.dta")
states_final<-combined6%>%group_by(state)%>%summarize(count=n())

reg<-plm(data=combined6,formula=logjt~logpop+logspend+logcv+logcp+loglaw+logsent,model="within",index=c("state","year"))
summary(reg)
