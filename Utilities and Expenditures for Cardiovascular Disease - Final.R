####################
#Install packages ##
####################

install.packages("foreign")
install.packages("survey")
install.packages("tidyr")
install.packages("dplyr")
install.packages("icd")
install.packages("packagename")
install.packages("ggplot2")
install.packages("comorbidity")
install.packages("jtools")
install.packages("weights")
install.packages("stringi")

###################
## Load packages ##
###################

library(foreign)
library(survey)
library(tidyr)
library(dplyr)
library(magrittr)
library(icd)
library(ggplot2)
library(comorbidity)
library(jtools)
library(weights)
library(readr)
library(stringi)

#############################
## Remove existing objects ##
#############################
rm(list=ls())

######################
## Set up directory ##
######################

#Step1, Please create a folder named "data" under your directory #
#Step2, under "data", please create folders named: 
  # prescribed medicine
  # dental visits
  # other medical expenses
  # er visits
  # hospital inpatient stays
  # outpatient visits
  # office based medical provider visits
  # home health
  # medical conditions
  # charlson
  # longitudinal weights
  # consolidated #
#Step3, under each folder created in Step2, please create two folders: raw and rdata #

directory<-c("~/documents/ISMMS/Research/Current Projects/ASCVD QoL & Expenditures/MEPS Files/")
setwd(directory)

########################
## set up year vector ##
########################

year<-c(seq(2012,2016))

###############################
## Read Inflation excel file ##
###############################

#Notes: Inflation files are used to inflate all expenditures to 2017 values; this is set up to inflate family (out-of-pocket) expenditures by the consumer price index (CPI) and other medical expenditures by the personal consumer expenditures (PCE) health index 
#Step1: Import inflation csv file
inflation <-read.csv(paste0(directory,"Inflation - 2017.csv"), row.names = 1)

#Step2: Create ratePCE_health variable to inflate other medical expenditures
inflation$ratePCE_health <- 1/(inflation$PCE_Health/inflation$PCE_Health[max(nrow(inflation))])

#Step3: Create rateCPI variable to inflate family (out-of-pocket) expenditures
inflation$rateCPI <- 1/(inflation$CPI/inflation$CPI[max(nrow(inflation))]) 

########################
### DATA PREPARATION ###
########################

###################################
## Prescribed medicine 2012-2016 ##
###################################

#Step1: Create directories for loading and save data
directory_PrescribedMedicine_load<-paste0(directory, "prescribed medicine/raw/")
directory_PrescribedMedicine_save<-paste0(directory, "prescribed medicine/rdata/")

#Step2: Create vectors for extracting names and assigning names
PrescribedMedicine_file_name_load_ssp=list.files(path=directory_PrescribedMedicine_load) ##file name.ssp
PrescribedMedicine_file_name_load<-stringr::str_remove(PrescribedMedicine_file_name_load_ssp,".ssp") ##file name. 

PrescribedMedicine_file_name_save<-paste0("PrescribedMedicines_",year)
PrescribedMedicine_file_name_save_grouped<-paste0("PrescribedMedicines_Grouped_", year)

#Step3: Load and save files
counter=1
for(i in PrescribedMedicine_file_name_load) {
  data1=read.xport(paste0(directory_PrescribedMedicine_load,i,".ssp"))
  assign(PrescribedMedicine_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in PrescribedMedicine_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("RXSF",year_temporary,"X"), !!paste0("RXMR",year_temporary,"X"), !!paste0("RXMD",year_temporary,"X"), !!paste0("RXPV",year_temporary,"X"), !!paste0("RXXP",year_temporary,"X"))
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_RX   = sum(get(paste0("RXSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_RX = sum(get(paste0("RXMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_RX = sum(get(paste0("RXMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_RX  = sum(get(paste0("RXPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_RX    = (sum(get(paste0("RXXP",year_temporary,"X")))- sum(get(paste0("RXSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("RXSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]), 
              n_RX = n()
              )
  assign(PrescribedMedicine_file_name_save_grouped[counter], data1)
  save(data1,file=paste0(directory_PrescribedMedicine_save,PrescribedMedicine_file_name_save_grouped[counter],".rdata"))
  counter<-counter+1}

############################
## Dental visit 2012-2016 ##
############################

#Step1: Create directories for loading and save data
directory_DentalVisits_load<-paste0(directory, "dental visits/raw/")
directory_DentalVisits_save<-paste0(directory, "dental visits/rdata/")

#Step2: Create vectors for extracting names and assigning names
DentalVisits_file_name_load_ssp<-list.files(path = directory_DentalVisits_load)
DentalVisits_file_name_load<-stringr::str_remove(DentalVisits_file_name_load_ssp,".ssp")

DentalVisits_file_name_save<-paste0("DentalVisits_",year)
DentalVisits_file_name_save_grouped<-paste0("DentalVisits_Grouped_",year)

#Step3: Load and save files
counter=1
for(i in DentalVisits_file_name_load) {
  data1=read.xport(paste0(directory_DentalVisits_load,i,".ssp"))
  assign(DentalVisits_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in DentalVisits_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("DVSF",year_temporary,"X"), !!paste0("DVMR",year_temporary,"X"), !!paste0("DVMD",year_temporary,"X"), !!paste0("DVPV",year_temporary,"X"), !!paste0("DVXP",year_temporary,"X"))
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_DV=sum(get(paste0("DVSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_DV = sum(get(paste0("DVMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_DV = sum(get(paste0("DVMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_DV = sum(get(paste0("DVPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_DV    = (sum(get(paste0("DVXP",year_temporary,"X")))- sum(get(paste0("DVSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("DVSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]), 
              n_DV = n())
  assign(DentalVisits_file_name_save_grouped[counter], data1)
  save(data1,file=paste0(directory_DentalVisits_save,DentalVisits_file_name_save_grouped[counter], ".rdata"))
  counter<-counter+1}

######################################
## Other Medical Expenses 2012-2016 ##
######################################

#Step1: Create directories for loading and save data
directory_OtherMedicalExpenses_load<-paste0(directory,"other medical expenses/raw/")
directory_OtherMedicalExpenses_save<-paste0(directory,"other medical expenses/rdata/")

#Step2: Create vectors for extracting names and assigning names
OtherMedicalExpenses_file_name_load_ssp<-list.files(path = directory_OtherMedicalExpenses_load)
OtherMedicalExpenses_file_name_load<-stringr::str_remove(OtherMedicalExpenses_file_name_load_ssp,".ssp")

OtherMedicalExpenses_file_name_save<-paste0("OtherMedicalExpenses_",year)
OtherMedicalExpenses_file_name_save_grouped<-paste0("OtherMedicalExpenses_Grouped_", year)

#Step3: Load and save files
counter=1
for(i in OtherMedicalExpenses_file_name_load) {
  data1=read.xport(paste0(directory_OtherMedicalExpenses_load,i,".ssp"))
  assign(OtherMedicalExpenses_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in OtherMedicalExpenses_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("OMSF",year_temporary,"X"), !!paste0("OMMR",year_temporary,"X"), !!paste0("OMMD",year_temporary,"X"), !!paste0("OMPV",year_temporary,"X"), !!paste0("OMXP",year_temporary,"X"))
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_OM=sum(get(paste0("OMSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_OM = sum(get(paste0("OMMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_OM = sum(get(paste0("OMMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_OM = sum(get(paste0("OMPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_OM    = (sum(get(paste0("OMXP",year_temporary,"X")))- sum(get(paste0("OMSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("OMSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]), 
              n_OM = n())
  assign(OtherMedicalExpenses_file_name_save_grouped[counter],data1)
  save(data1,file=paste0(directory_OtherMedicalExpenses_save, OtherMedicalExpenses_file_name_save_grouped[counter],".rdata"))
  counter<-counter+1}

#####################################
## Emergency Room Visits 2012-2016 ##
#####################################

#Step1: Create directories for loading and save data
directory_EmergencyRoomVisits_load<-paste0(directory,"er visits/raw/")
directory_EmergencyRoomVisits_save<-paste0(directory,"er visits/rdata/")

#Step2: Create vectors for extracting names and assigning names
EmergencyRoomVisits_file_name_load_ssp<-list.files(path = directory_EmergencyRoomVisits_load)
EmergencyRoomVisits_file_name_load<-stringr::str_remove(EmergencyRoomVisits_file_name_load_ssp,".ssp")

EmergencyRoomVisits_file_name_save<-paste0("EmergencyRoomVisits_",year)
EmergencyRoomVisits_file_name_save_grouped<-paste0("EmergencyRoomVisits_Grouped_", year)

#Step3: Load and save files
counter=1
for(i in EmergencyRoomVisits_file_name_load) {
  data1=read.xport(paste0(directory_EmergencyRoomVisits_load,i,".ssp"))
  assign(EmergencyRoomVisits_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in EmergencyRoomVisits_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("ERDSF",year_temporary,"X"), !!paste0("ERFSF",year_temporary,"X"), !!paste0("ERDMR",year_temporary,"X"), !!paste0("ERFMR",year_temporary,"X"), !!paste0("ERDMD",year_temporary,"X"),
           !!paste0("ERFMD",year_temporary,"X"), !!paste0("ERDPV",year_temporary,"X"), !!paste0("ERFPV",year_temporary,"X"), !!paste0("ERXP",year_temporary,"X"))
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_ER=sum(get(paste0("ERDSF",year_temporary,"X"))+get(paste0("ERFSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_ER = sum(get(paste0("ERDMR",year_temporary,"X"))+get(paste0("ERFMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_ER = sum(get(paste0("ERDMD",year_temporary,"X"))+get(paste0("ERFMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_ER  = sum(get(paste0("ERDPV",year_temporary,"X"))+get(paste0("ERFPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_ER    = (sum(get(paste0("ERXP",year_temporary,"X")))- sum(get(paste0("ERDSF",year_temporary,"X"))+get(paste0("ERFSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("ERDSF",year_temporary,"X"))+get(paste0("ERFSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]),
              n_ER = n())
  assign(EmergencyRoomVisits_file_name_save_grouped[counter], data1)
  save(data1, file=paste0(directory_EmergencyRoomVisits_save,EmergencyRoomVisits_file_name_save_grouped[counter],".rdata"))
  counter<-counter+1}

################################################
## Hospital Inpatient Stays - Costs 2012-2016 ##
################################################

#Step1: Create directories for loading and save data
directory_HospitalInpatientStays_load<-paste0(directory,"hospital inpatient stays/raw/")
directory_HospitalInpatientStays_save<-paste0(directory,"hospital inpatient stays/rdata/")

#Step2: Create vectors for extracting names and assigning names
HospitalInpatientStays_file_name_load_ssp<-list.files(path = directory_HospitalInpatientStays_load)
HospitalInpatientStays_file_name_load<-stringr::str_remove(HospitalInpatientStays_file_name_load_ssp,".ssp")

HospitalInpatientStays_file_name_save<-paste0("HospitalInpatientStays_",year)
HospitalInpatientStays_file_name_save_grouped<-paste0("HospitalInpatientStays_Grouped_",year)

#Step3: Load and save files
counter=1
for(i in HospitalInpatientStays_file_name_load) {
  data1=read.xport(paste0(directory_HospitalInpatientStays_load,i,".ssp"))
  assign(HospitalInpatientStays_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in HospitalInpatientStays_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("IPDSF",year_temporary,"X"), !!paste0("IPFSF",year_temporary,"X"), !!paste0("IPDMR",year_temporary,"X"), !!paste0("IPFMR",year_temporary,"X"), !!paste0("IPDMD",year_temporary,"X"),
           !!paste0("IPFMD",year_temporary,"X"), !!paste0("IPDPV",year_temporary,"X"), !!paste0("IPFPV",year_temporary,"X"), !!paste0("IPXP",year_temporary,"X"))
  
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_IP=sum(get(paste0("IPDSF",year_temporary,"X"))+get(paste0("IPFSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_IP = sum(get(paste0("IPDMR",year_temporary,"X"))+get(paste0("IPFMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_IP = sum(get(paste0("IPDMD",year_temporary,"X"))+get(paste0("IPFMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_IP  = sum(get(paste0("IPDPV",year_temporary,"X"))+get(paste0("IPFPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_IP    = (sum(get(paste0("IPXP",year_temporary,"X")))- sum(get(paste0("IPDSF",year_temporary,"X"))+get(paste0("IPFSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("IPDSF",year_temporary,"X"))+get(paste0("IPFSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]),
              n_IP = n())
  assign(HospitalInpatientStays_file_name_save_grouped[counter], data1)
  save(data1,file=paste0(directory_HospitalInpatientStays_save,HospitalInpatientStays_file_name_save_grouped[counter], ".rdata"))
  counter<-counter+1}

#########################################
## Outpatient Visits - Costs 2012-2016 ##
#########################################

#Step1: Create directories for loading and save data
directory_OutpatientVisits_load<-paste0(directory,"outpatient visits/raw/")
directory_OutpatientVisits_save<-paste0(directory,"outpatient visits/rdata/")

#Step2: Create vectors for extracting names and assigning names
OutpatientVisits_file_name_load_ssp<-list.files(path = directory_OutpatientVisits_load)
OutpatientVisits_file_name_load<-stringr::str_remove(OutpatientVisits_file_name_load_ssp,".ssp")

OutpatientVisits_file_name_save<-paste0("OutpatientVisits_",year)
OutpatientVisits_file_name_save_grouped<-paste0("OutpatientVisits_Grouped_",year)

#Step3: Load and save files
counter=1
for(i in OutpatientVisits_file_name_load) {
  data1=read.xport(paste0(directory_OutpatientVisits_load,i,".ssp"))
  assign(OutpatientVisits_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in OutpatientVisits_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("OPDSF",year_temporary,"X"), !!paste0("OPFSF",year_temporary,"X"), !!paste0("OPDMR",year_temporary,"X"), !!paste0("OPFMR",year_temporary,"X"), !!paste0("OPDMD",year_temporary,"X"),
           !!paste0("OPFMD",year_temporary,"X"), !!paste0("OPDPV",year_temporary,"X"), !!paste0("OPFPV",year_temporary,"X"), !!paste0("OPXP",year_temporary,"X"))
  data1[data1==-1] <- NA
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_OP=sum(get(paste0("OPDSF",year_temporary,"X"))+get(paste0("OPFSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_OP = sum(get(paste0("OPDMR",year_temporary,"X"))+get(paste0("OPFMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_OP = sum(get(paste0("OPDMD",year_temporary,"X"))+get(paste0("OPFMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_OP  = sum(get(paste0("OPDPV",year_temporary,"X"))+get(paste0("OPFPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_OP    = (sum(get(paste0("OPXP",year_temporary,"X")))- sum(get(paste0("OPDSF",year_temporary,"X"))+get(paste0("OPFSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("OPDSF",year_temporary,"X"))+get(paste0("OPFSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]),
              n_OP = n())
  assign(OutpatientVisits_file_name_save_grouped[counter], data1)
  save(data1,file=paste0(directory_OutpatientVisits_save,OutpatientVisits_file_name_save_grouped[counter],".rdata"))
  counter<-counter+1}

############################################################
## Office-Based Medical Provider Visits - Costs 2012-2016 ##
############################################################

#Step1: Create directories for loading and save data
directory_OfficeBasedMedicalProviderVisits_load<-paste0(directory,"office based medical provider visits/raw/")
directory_OfficeBasedMedicalProviderVisits_save<-paste0(directory,"office based medical provider visits/rdata/")

#Step2: Create vectors for extracting names and assigning names
OfficeBasedMedicalProviderVisits_file_name_load_ssp<-list.files(path = directory_OfficeBasedMedicalProviderVisits_load)
OfficeBasedMedicalProviderVisits_file_name_load<-stringr::str_remove(OfficeBasedMedicalProviderVisits_file_name_load_ssp,".ssp")

OfficeBasedMedicalProviderVisits_file_name_save<-paste0("OfficeBasedMedicalProviderVisits_",year)
OfficeBasedMedicalProviderVisits_file_name_save_grouped<-paste0("OfficeBasedMedicalProviderVisits_Grouped_",year)

#Step3: Load and save files
counter=1
for(i in OfficeBasedMedicalProviderVisits_file_name_load) {
  data1=read.xport(paste0(directory_OfficeBasedMedicalProviderVisits_load,i,".ssp"))
  assign(OfficeBasedMedicalProviderVisits_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in OfficeBasedMedicalProviderVisits_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("OBSF",year_temporary,"X"), !!paste0("OBMR",year_temporary,"X"), !!paste0("OBMD",year_temporary,"X"), !!paste0("OBPV",year_temporary,"X"), !!paste0("OBXP",year_temporary,"X"))
  data1[data1==-1] <- NA
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_OB=sum(get(paste0("OBSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_OB = sum(get(paste0("OBMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_OB = sum(get(paste0("OBMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_OB  = sum(get(paste0("OBPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_OB    = (sum(get(paste0("OBXP",year_temporary,"X")))- sum(get(paste0("OBSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("OBSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]), 
              n_OB = n())
  assign(OfficeBasedMedicalProviderVisits_file_name_save_grouped[counter], data1)
  save(data1,file=paste0(directory_OfficeBasedMedicalProviderVisits_save,OfficeBasedMedicalProviderVisits_file_name_save_grouped[counter],".rdata"))
  counter<-counter+1}

###################################
## Home Health - costs 2012-2016 ##
###################################

#Step1: Create directories for loading and save data
directory_HomeHealth_load<-paste0(directory,"home health/raw/")
directory_HomeHealth_save<-paste0(directory,"home health/rdata/")

#Step2: Create vectors for extracting names and assigning names
HomeHealth_file_name_load_ssp<-list.files(path = directory_HomeHealth_load)
HomeHealth_file_name_load<-stringr::str_remove(HomeHealth_file_name_load_ssp,".ssp")

HomeHealth_file_name_save<-paste0("HomeHealth_",year)
HomeHealth_file_name_save_grouped<-paste0("HomeHealth_Grouped_",year)

#Step3: Load and save files
counter=1
for(i in HomeHealth_file_name_load) {
  data1=read.xport(paste0(directory_HomeHealth_load,i,".ssp"))
  assign(HomeHealth_file_name_save[counter], data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in HomeHealth_file_name_save) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    select(DUPERSID, !!paste0("HHSF",year_temporary,"X"), !!paste0("HHMR",year_temporary,"X"), !!paste0("HHMD",year_temporary,"X"), !!paste0("HHPV",year_temporary,"X"), !!paste0("HHXP",year_temporary,"X"))
  data1[data1==-1] <- NA
  data1<-data1 %>% 
    group_by(DUPERSID) %>% 
    summarise(Family_HH=sum(get(paste0("HHSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"], 
              Medicare_HH = sum(get(paste0("HHMR",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Medicaid_HH = sum(get(paste0("HHMD",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Private_HH  = sum(get(paste0("HHPV",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"], 
              Total_HH    = (sum(get(paste0("HHXP",year_temporary,"X")))- sum(get(paste0("HHSF",year_temporary,"X")))) * inflation[match(year[counter],inflation$Year),"ratePCE_health"] + (sum(get(paste0("HHSF",year_temporary,"X"))) * inflation[match(year[counter],inflation$Year),"rateCPI"]), 
              n_HH = n())
  assign(HomeHealth_file_name_save_grouped[counter], data1)
  save(data1,file=paste0(directory_HomeHealth_save,HomeHealth_file_name_save_grouped[counter],".rdata"))
  counter<-counter+1}

##################################
## Medical Conditions 2011-2016 ## 
##################################

#Notes: ICD 9 codes are applicable to 2011-2015 files, ICD 10 codes are applicable starting from 2016.#

#Step0: Create year vectors based on ICD codes
year_ICD_10<-c(2016)
year_ICD_09<-c(seq(2011, 2015))
year_MedicalCondition<-seq(2011, 2016)

#Step1: Create directories for loading and save data
directory_MedicalCondition_load<-paste0(directory,"medical conditions/raw/")
directory_MedicalCondition_save<-paste0(directory,"medical conditions/rdata/")

#Step2: Create vectors for extracting names and assigning names
MedicalCondition_file_name_load_ssp<-list.files(path = directory_MedicalCondition_load)
MedicalCondition_file_name_load<-stringr::str_remove(MedicalCondition_file_name_load_ssp,".ssp")
MedicalCondition_file_name_save_all<-paste0("MedicalConditions_", year_MedicalCondition)

MedicalCondition_file_name_save_ICD10<-paste0("MedicalConditions_",year_ICD_10)
MedicalCondition_file_name_save_ICD10_grouped<-paste0("MedicalConditions_Grouped_",year_ICD_10)

MedicalCondition_file_name_save_ICD9<-paste0("MedicalConditions_",year_ICD_09)
MedicalCondition_file_name_save_ICD9_grouped<-paste0("MedicalConditions_Grouped_",year_ICD_09)

#Step3: Load and save files
#For ICD 10 and ICD 9
counter=1
for(i in MedicalCondition_file_name_load) {
  data1=read.xport(paste0(directory_MedicalCondition_load,i,".ssp"))
  assign(MedicalCondition_file_name_save_all[counter], data1)
  counter<-counter+1}

#Step4: Data management
#For ICD 10
counter=1
for (i in MedicalCondition_file_name_save_ICD10){
  data1 = get(i) %>% 
    select(DUPERSID, ICD10CDX) %>% 
    mutate(MedicalCondition_1 = ifelse(ICD10CDX == "I21" | ICD10CDX == "I25", 1, 0), #Myocardial Infarction
           MedicalCondition_2 = ifelse(ICD10CDX == "I63", 1, 0), #Ischemic Stroke
           MedicalCondition_3 = ifelse(ICD10CDX == "I50", 1, 0), #Heart Failure
           MedicalCondition_4 = ifelse(ICD10CDX == "I47" | ICD10CDX == "I48" | ICD10CDX == "I49", 1, 0), #Cardiac Dysrhythmias
           MedicalCondition_5 = ifelse(ICD10CDX == "I20", 1, 0), #Angina Pectoris
           MedicalCondition_6 = ifelse(ICD10CDX == "I70" | ICD10CDX == "I71" | ICD10CDX == "I72" | ICD10CDX == "I73" | ICD10CDX == "I74" | ICD10CDX == "I77", 1, 0), #Peripheral Vascular Disease
           MedicalCondition_7 = ifelse(ICD10CDX == "E11", 1, 0)) #Type II Diabetes Mellitus
  assign(MedicalCondition_file_name_save_ICD10[counter], data1)
  counter<-counter+1}

counter=1
for (i in MedicalCondition_file_name_save_ICD10) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    group_by(DUPERSID) %>% 
    summarise(MedicalCondition_1_Grouped = max(MedicalCondition_1), #Medical conditions with the same DUPERSID are summed, values > 0 represent that the medical condition is present
              MedicalCondition_2_Grouped = max(MedicalCondition_2),
              MedicalCondition_3_Grouped = max(MedicalCondition_3),
              MedicalCondition_4_Grouped = max(MedicalCondition_4),
              MedicalCondition_5_Grouped = max(MedicalCondition_5),
              MedicalCondition_6_Grouped = max(MedicalCondition_6),
              MedicalCondition_7_Grouped = max(MedicalCondition_7))
  assign(MedicalCondition_file_name_save_ICD10_grouped[counter], data1)
  save(data1,file=paste0(directory_MedicalCondition_save,MedicalCondition_file_name_save_ICD10_grouped[counter],".rdata"))
  counter<-counter+1}

#For ICD 9
counter=1
for (i in MedicalCondition_file_name_save_ICD9){
  data1 = get(i) %>% 
    select(DUPERSID, ICD9CODX) %>% 
    mutate(MedicalCondition_1 = ifelse(ICD9CODX == "410" | ICD9CODX == "414", 1, 0), #Myocardial Infarction
           MedicalCondition_2 = ifelse(ICD9CODX == "433" | ICD9CODX == "434" | ICD9CODX == "436", 1, 0), #Ischemic Stroke
           MedicalCondition_3 = ifelse(ICD9CODX == "428", 1, 0), #Heart Failure
           MedicalCondition_4 = ifelse(ICD9CODX == "427", 1, 0), #Cardiac Dysrhythmias
           MedicalCondition_5 = ifelse(ICD9CODX == "413", 1, 0), #Angina Pectoris
           MedicalCondition_6 = ifelse(ICD9CODX == "440" | ICD9CODX == "441" | ICD9CODX == "442" | ICD9CODX == "443" | ICD9CODX == "444" | ICD9CODX == "447", 1, 0), #Peripheral Vascular Disease
           MedicalCondition_7 = ifelse(ICD9CODX == "250", 1, 0)) #Type II Diabetes Mellitus
    assign(MedicalCondition_file_name_save_ICD9[counter], data1)
  counter<-counter+1}

counter=1
for (i in MedicalCondition_file_name_save_ICD9) {
  year_temporary=stri_sub(i,-2,-1)
  data1 = get(i) %>% 
    group_by(DUPERSID) %>% 
    summarise(MedicalCondition_1_Grouped = max(MedicalCondition_1), #Medical conditions with the same DUPERSID are summed, values > 0 represent that the medical condition is present
              MedicalCondition_2_Grouped = max(MedicalCondition_2),
              MedicalCondition_3_Grouped = max(MedicalCondition_3),
              MedicalCondition_4_Grouped = max(MedicalCondition_4),
              MedicalCondition_5_Grouped = max(MedicalCondition_5),
              MedicalCondition_6_Grouped = max(MedicalCondition_6),
              MedicalCondition_7_Grouped = max(MedicalCondition_7))
  assign(MedicalCondition_file_name_save_ICD9_grouped[counter], data1)
  save(data1,file=paste0(directory_MedicalCondition_save,MedicalCondition_file_name_save_ICD9_grouped[counter],".rdata"))
  counter<-counter+1}

################################
## Charlson Comorbidity Index ##
################################

#Note: CCIs are derived from MedicalConditions files.

#Step0: Create year vectors based on ICD codes
year_ICD_10<-c(2016)
year_ICD_09<-c(seq(2011, 2015))

#Step1: Create directories for loading and save data
directory_Charlson_save<-paste0(directory,"charlson/rdata/")

#Step2: Create vectors for extracting names and assigning names
Charlson_file_name_save_ICD10<-paste0("Charlson_",year_ICD_10)
Charlson_file_name_save_ICD9<-paste0("Charlson_",year_ICD_09)

#Step3: Load and save files
#For ICD 10
counter=1
for (i in MedicalCondition_file_name_save_ICD10){
  data1 = get(i) %>% 
    select(DUPERSID, ICD10CDX) %>% 
    charlson(visit_name = "DUPERSID", return_df = T)
  assign(Charlson_file_name_save_ICD10[counter],data1)
  save(data1,file=paste0(directory_Charlson_save,MedicalCondition_file_name_save_ICD10[counter],".rdata"))
  counter<-counter+1}

#For ICD 9
counter=1
for (i in MedicalCondition_file_name_save_ICD9){
  data1 = get(i) %>% 
    select(DUPERSID, ICD9CODX) %>% 
    charlson(visit_name = "DUPERSID", return_df = T)
  assign(Charlson_file_name_save_ICD9[counter],data1)
  save(data1,file=paste0(directory_Charlson_save, MedicalCondition_file_name_save_ICD9[counter],".rdata"))
  counter<-counter+1}

##########################
## Longitudinal Weights ##
##########################

#Step0: Create year vector
year_longitudinal_panel<-c(seq(16,20))

#Step1: Create directories for loading and save data
directory_Longitudinal_load<-paste0(directory,"longitudinal weights/raw/")
directory_Longitudinal_save<-paste0(directory, "longitudinal weights/rdata/")

#Step2: Create vectors for extracting names and assigning names
Longitudinal_file_name_load_ssp<-list.files(path=directory_Longitudinal_load)
Longitudinal_file_name_load<-stringr::str_remove(Longitudinal_file_name_load_ssp,".ssp")
Longitudinal_file_name_save<-paste0("LONGWT_PANEL_",year_longitudinal_panel)

#Step3: Load and save files
counter=1
for (i in Longitudinal_file_name_load){
  data1=read.xport(paste0(directory_Longitudinal_load, i, ".ssp"))
  assign(Longitudinal_file_name_save[counter],data1)
  counter<-counter+1}

#Step4: Data management
counter=1
for (i in Longitudinal_file_name_save){
  data1=get(i) %>% 
    select(DUPERSID, LONGWT)
  assign(Longitudinal_file_name_save[counter],data1)
  save(data1,file=paste0(directory_Longitudinal_save, Longitudinal_file_name_save[counter], ".rdata"))
  counter<-counter+1}

########################
## Consolidated Files ##
########################

#Notes: Variables regarding race and education are coded differently in some years. 
#Thus, we manually manage these variables without using loops.

#Step0: Create year vector
year_consolidated<-c(seq(2011,2016))

#Step1: Create directories for loading and save data
directory_Consolidated_load<-paste0(directory, "consolidated/raw/")
directory_Consolidated_save<-paste0(directory, "consolidated/rdata/")

#Step2: Create vectors for extracting names and assigning names
Consolidated_file_name_load_ssp<-list.files(path=directory_Consolidated_load)
Consolidated_file_name_load<-stringr::str_remove(Consolidated_file_name_load_ssp,".ssp")

Consolidated_file_name_save_original<-paste0("Consolidated_","Original_", year_consolidated) ##original dataset
Consolidated_file_name_save_incomplete<-paste0("Consolidated_","incomplete_",year_consolidated) ##incomplete: withouth EDUCATION and RACE
Consolidated_file_name_save<-paste0("Consolidated_", year_consolidated) ##final consolidated file

#Step3: Load and save files
counter=1
for (i in Consolidated_file_name_load){
  data1=read.xport(paste0(directory_Consolidated_load, i, ".ssp"))
  assign(Consolidated_file_name_save_original[counter], data1)
  counter<-counter+1}

#Step4: Data management except for variables of race and edcuation
counter=1
for (i in Consolidated_file_name_save_original){
  year_temporary=stri_sub(i, -2, -1)
  data1=get(i) %>% 
    rename(
      INSURC=!!(paste0("INSURC",year_temporary)),
      POVCAT=!!(paste0("POVCAT",year_temporary)),
      PERWT =!!(paste0("PERWT" ,year_temporary, "F"))) %>% 
    select(DUPERSID, PANEL, AGE31X, SEX, MARRY31X, INSURC, POVCAT, BMINDX53, PERWT, VARSTR, VARPSU, ADDAYA42, ADPWLM42, ADMALS42, ADPAIN42, ADNRGY42, ADDOWN42, ADSOCA42, MIAGED, CHDAGED, ANGIAGED, STRKAGED, DIABAGED)
  assign(Consolidated_file_name_save_incomplete[counter],data1)
  counter<-counter+1}

#Step5: Manually code race and education
Consolidated_2011<-Consolidated_incomplete_2011 %>% 
  mutate(EDUCATION=case_when(
    Consolidated_Original_2011$HIDEG==1 | Consolidated_Original_2011$HIDEG==8 ~ 1,
    Consolidated_Original_2011$HIDEG==2 | Consolidated_Original_2011$HIDEG==3 ~ 2,
    Consolidated_Original_2011$HIDEG==4 | Consolidated_Original_2011$HIDEG==7 ~ 3,
    Consolidated_Original_2011$HIDEG==5 | Consolidated_Original_2011$HIDEG==6 ~ 4,
    TRUE~ 0)) %>% 
  mutate(RACETHX=case_when(
    Consolidated_Original_2011$RACETHNX==1 ~ 1,
    Consolidated_Original_2011$RACEWX==1 ~ 2,
    Consolidated_Original_2011$RACETHNX==2 ~ 3,
    Consolidated_Original_2011$RACETHNX==3 ~ 4,
    TRUE~5))

Consolidated_2012<-Consolidated_incomplete_2012 %>% 
  mutate(EDUCATION=case_when(
    Consolidated_Original_2012$HIDEG==1 | Consolidated_Original_2012$HIDEG==8 ~ 1,
    Consolidated_Original_2012$HIDEG==2 | Consolidated_Original_2012$HIDEG==3 ~ 2,
    Consolidated_Original_2012$HIDEG==4 | Consolidated_Original_2012$HIDEG==7 ~ 3,
    Consolidated_Original_2012$HIDEG==5 | Consolidated_Original_2012$HIDEG==6 ~ 4,
    TRUE~ 0)) %>% 
  mutate(RACETHX=Consolidated_Original_2012$RACETHX)

Consolidated_2013<-Consolidated_incomplete_2013 %>% 
  mutate(EDUCATION=case_when(
    Consolidated_Original_2013$EDUYRDG==1 | Consolidated_Original_2013$EDUYRDG==2 |Consolidated_Original_2013$EDUYRDG==10 ~ 1,
    Consolidated_Original_2013$EDUYRDG>=3 & Consolidated_Original_2013$EDUYRDG<=5 ~ 2,
    Consolidated_Original_2013$EDUYRDG>=6 & Consolidated_Original_2013$EDUYRDG<=8 ~ 3,
    Consolidated_Original_2013$EDUYRDG==9 ~ 4,
    TRUE ~ 0)) %>% 
  mutate(RACETHX=Consolidated_Original_2013$RACETHX)

Consolidated_2014<-Consolidated_incomplete_2014 %>% 
  mutate(EDUCATION=case_when(
    Consolidated_Original_2014$EDUYRDG==1 | Consolidated_Original_2014$EDUYRDG==2 |Consolidated_Original_2014$EDUYRDG==10 ~ 1,
    Consolidated_Original_2014$EDUYRDG>=3 & Consolidated_Original_2014$EDUYRDG<=5 ~ 2,
    Consolidated_Original_2014$EDUYRDG>=6 & Consolidated_Original_2014$EDUYRDG<=8 ~ 3,
    Consolidated_Original_2014$EDUYRDG==9 ~ 4,
    TRUE ~ 0)) %>% 
  mutate(RACETHX=Consolidated_Original_2014$RACETHX)

Consolidated_2015<-Consolidated_incomplete_2015 %>% 
  mutate(EDUCATION=case_when(
    Consolidated_Original_2015$HIDEG==1 | Consolidated_Original_2015$HIDEG==8 ~ 1,
    Consolidated_Original_2015$HIDEG==2 | Consolidated_Original_2015$HIDEG==3 ~ 2,
    Consolidated_Original_2015$HIDEG==4 | Consolidated_Original_2015$HIDEG==7 ~ 3,
    Consolidated_Original_2015$HIDEG==5 | Consolidated_Original_2015$HIDEG==6 ~ 4,
    TRUE~ 0)) %>% 
  mutate(RACETHX=Consolidated_Original_2015$RACETHX)

Consolidated_2016<-Consolidated_incomplete_2016 %>% 
  mutate(EDUCATION=case_when(
    Consolidated_Original_2016$HIDEG==1 | Consolidated_Original_2016$HIDEG==8 ~ 1,
    Consolidated_Original_2016$HIDEG==2 | Consolidated_Original_2016$HIDEG==3 ~ 2,
    Consolidated_Original_2016$HIDEG==4 | Consolidated_Original_2016$HIDEG==7 ~ 3,
    Consolidated_Original_2016$HIDEG==5 | Consolidated_Original_2016$HIDEG==6 ~ 4,
    TRUE~ 0)) %>% 
  mutate(RACETHX=Consolidated_Original_2016$RACETHX)

#Step6: Save the final consolidated files
counter=1
for (i in Consolidated_file_name_save) {
  data1 = get(i) %>% 
  save(data1,file=paste0(directory_Consolidated_save,Consolidated_file_name_save[counter],".rdata"))
  counter<-counter+1}

#################
## Merge Files ##
#################

#Master File <- Consolidated file 2015 merged with all 2016 event files
Master_2016 = Consolidated_2015 %>%  #Year prior to QoL & Expenditure Estimates
  merge(LONGWT_PANEL_20, all.x = T) %>%
  merge(PrescribedMedicines_Grouped_2016, all.x = T) %>%
  merge(DentalVisits_Grouped_2016, all.x = T) %>%
  merge(OtherMedicalExpenses_Grouped_2016, all.x = T) %>%
  merge(HospitalInpatientStays_Grouped_2016, all.x = T) %>%
  merge(EmergencyRoomVisits_Grouped_2016,  all.x = T) %>%
  merge(OutpatientVisits_Grouped_2016, all.x = T) %>%
  merge(OfficeBasedMedicalProviderVisits_Grouped_2016, all.x = T) %>%
  merge(HomeHealth_Grouped_2016, all.x = T) %>%
  merge(MedicalConditions_Grouped_2015, all.x = T) %>% #Year prior to QoL & Expenditure Estimates
  merge(Charlson_2015, all.x = T) #Year prior to QoL & Expenditure Estimates

Master_2016$Year = 2016
head(Master_2016)

#Master File <- Consolidated file 2014 merged with all 2015 event files
Master_2015 = Consolidated_2014 %>%  #Year prior to QoL & Expenditure Estimates
  merge(LONGWT_PANEL_19, all.x = T) %>%
  merge(PrescribedMedicines_Grouped_2015, all.x = T) %>%
  merge(DentalVisits_Grouped_2015, all.x = T) %>%
  merge(OtherMedicalExpenses_Grouped_2015, all.x = T) %>%
  merge(HospitalInpatientStays_Grouped_2015, all.x = T) %>%
  merge(EmergencyRoomVisits_Grouped_2015,  all.x = T) %>%
  merge(OutpatientVisits_Grouped_2015, all.x = T) %>%
  merge(OfficeBasedMedicalProviderVisits_Grouped_2015, all.x = T) %>%
  merge(HomeHealth_Grouped_2015, all.x = T) %>%
  merge(MedicalConditions_Grouped_2014, all.x = T) %>% #Year prior to QoL & Expenditure Estimates
  merge(Charlson_2014, all.x = T) #Year prior to QoL & Expenditure Estimates

Master_2015$Year = 2015
head(Master_2015)

#Master File <- Consolidated file 2013 merged with all 2014 event files
Master_2014 = Consolidated_2013 %>%  #Year prior to QoL & Expenditure Estimates
  merge(LONGWT_PANEL_18, all.x = T) %>%
  merge(PrescribedMedicines_Grouped_2014, all.x = T) %>%
  merge(DentalVisits_Grouped_2014, all.x = T) %>%
  merge(OtherMedicalExpenses_Grouped_2014, all.x = T) %>%
  merge(HospitalInpatientStays_Grouped_2014, all.x = T) %>%
  merge(EmergencyRoomVisits_Grouped_2014,  all.x = T) %>%
  merge(OutpatientVisits_Grouped_2014, all.x = T) %>%
  merge(OfficeBasedMedicalProviderVisits_Grouped_2014, all.x = T) %>%
  merge(HomeHealth_Grouped_2014, all.x = T) %>%
  merge(MedicalConditions_Grouped_2013, all.x = T) %>% #Year prior to QoL & Expenditure Estimates
  merge(Charlson_2013, all.x = T) #Year prior to QoL & Expenditure Estimates

Master_2014$Year = 2014
head(Master_2014)

#Master File <- Consolidated file 2012 merged with all 2013 event files
Master_2013 = Consolidated_2012 %>% #Year prior to QoL & Expenditure Estimates
  merge(LONGWT_PANEL_17, all.x = T) %>%
  merge(PrescribedMedicines_Grouped_2013, all.x = T) %>%
  merge(DentalVisits_Grouped_2013, all.x = T) %>%
  merge(OtherMedicalExpenses_Grouped_2013, all.x = T) %>%
  merge(HospitalInpatientStays_Grouped_2013, all.x = T) %>%
  merge(EmergencyRoomVisits_Grouped_2013,  all.x = T) %>%
  merge(OutpatientVisits_Grouped_2013, all.x = T) %>%
  merge(OfficeBasedMedicalProviderVisits_Grouped_2013, all.x = T) %>%
  merge(HomeHealth_Grouped_2013, all.x = T) %>%
  merge(MedicalConditions_Grouped_2012, all.x = T) %>% #Year prior to QoL & Expenditure Estimates
  merge(Charlson_2012, all.x = T) #Year prior to QoL & Expenditure Estimates

Master_2013$Year = 2013
head(Master_2013)

#Master File <- Consolidated file 2011 merged with all 2012 event files
Master_2012 = Consolidated_2011 %>%  #Year prior to QoL & Expenditure Estimates
  merge(LONGWT_PANEL_16, all.x = T) %>%
  merge(PrescribedMedicines_Grouped_2012, all.x = T) %>%
  merge(DentalVisits_Grouped_2012, all.x = T) %>%
  merge(OtherMedicalExpenses_Grouped_2012, all.x = T) %>%
  merge(HospitalInpatientStays_Grouped_2012, all.x = T) %>%
  merge(EmergencyRoomVisits_Grouped_2012,  all.x = T) %>%
  merge(OutpatientVisits_Grouped_2012, all.x = T) %>%
  merge(OfficeBasedMedicalProviderVisits_Grouped_2012, all.x = T) %>%
  merge(HomeHealth_Grouped_2012, all.x = T) %>%
  merge(MedicalConditions_Grouped_2011, all.x = T) %>% #Year prior to QoL & Expenditure Estimates
  merge(Charlson_2011, all.x = T) #Year prior to QoL & Expenditure Estimates

Master_2012$Year = 2012
head(Master_2012)

#Bind Data
Bind_Data <- rbind(Master_2016, Master_2015, Master_2014, Master_2013, Master_2012) 
table(Bind_Data$Year)

#Total Costs per Payer
Bind_Data$Family_Total = rowSums(Bind_Data[, c("Family_RX", "Family_DV", "Family_OM", "Family_IP", "Family_ER", "Family_OP", "Family_OB", "Family_HH")], na.rm =T)
Bind_Data$Medicare_Total = rowSums(Bind_Data[, c("Medicare_RX", "Medicare_DV", "Medicare_OM", "Medicare_IP", "Medicare_ER", "Medicare_OP", "Medicare_OB", "Medicare_HH")], na.rm =T)
Bind_Data$Medicaid_Total = rowSums(Bind_Data[, c("Medicaid_RX", "Medicaid_DV", "Medicaid_OM", "Medicaid_IP", "Medicaid_ER", "Medicaid_OP", "Medicaid_OB", "Medicaid_HH")], na.rm =T)
Bind_Data$Private_Total = rowSums(Bind_Data[, c("Private_RX", "Private_DV", "Private_OM", "Private_IP", "Private_ER", "Private_OP", "Private_OB", "Private_HH")], na.rm =T)
Bind_Data$AllPayers_Total = rowSums(Bind_Data[, c("Total_RX", "Total_DV", "Total_OM", "Total_IP", "Total_ER", "Total_OP", "Total_OB", "Total_HH")], na.rm =T)

###############################
## Create SF-6D from SF-12V2 ##
###############################

Bind_Data$ADDAYA42[Bind_Data$ADDAYA42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADPWLM42[Bind_Data$ADPWLM42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADMALS42[Bind_Data$ADMALS42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADPAIN42[Bind_Data$ADPAIN42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADNRGY42[Bind_Data$ADNRGY42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADDOWN42[Bind_Data$ADDOWN42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included
Bind_Data$ADSOCA42[Bind_Data$ADSOCA42 < 0] <- NA #All negative values correspond to Not Ascertained, Don't Know, Refused, or Inapplicable and should not be included

#Recode 5 item response in version2 to 2 item response of version1
Bind_Data$ADPWLM42_Recode = ifelse(Bind_Data$ADPWLM42 > 0 & Bind_Data$ADPWLM42 < 5, 1, ifelse(Bind_Data$ADPWLM42 == 5, 2, 0))
Bind_Data$ADMALS42_Recode = ifelse(Bind_Data$ADMALS42 > 0 & Bind_Data$ADMALS42 < 5, 1, ifelse(Bind_Data$ADMALS42 == 5, 2, 0))

Bind_Data$SFPhys = ifelse(Bind_Data$ADDAYA42 == 3, 1, ifelse(Bind_Data$ADDAYA42 == 2, 2, ifelse(Bind_Data$ADDAYA42 == 1, 3, 0)))
Bind_Data$SFRole = ifelse(Bind_Data$ADPWLM42_Recode == 2 & Bind_Data$ADMALS42_Recode == 2, 1, ifelse(Bind_Data$ADPWLM42_Recode == 1 & Bind_Data$ADMALS42_Recode == 2, 2, ifelse(Bind_Data$ADPWLM42_Recode == 2 & Bind_Data$ADMALS42_Recode == 1, 3, ifelse(Bind_Data$ADPWLM42_Recode == 1 & Bind_Data$ADMALS42_Recode == 1, 4, 0))))
Bind_Data$SFSocial = ifelse(Bind_Data$ADSOCA42 == 5, 1, ifelse(Bind_Data$ADSOCA42 == 4, 2, ifelse(Bind_Data$ADSOCA42 == 3, 3, ifelse(Bind_Data$ADSOCA42 == 2, 4, ifelse(Bind_Data$ADSOCA42 == 1, 5, 0)))))
Bind_Data$SFPain = Bind_Data$ADPAIN42
Bind_Data$SFMental = ifelse(Bind_Data$ADDOWN42 == 5, 1, ifelse(Bind_Data$ADDOWN42 == 4, 2, ifelse(Bind_Data$ADDOWN42 == 3, 3, ifelse(Bind_Data$ADDOWN42 == 2, 4, ifelse(Bind_Data$ADDOWN42 == 1, 5, 0)))))
Bind_Data$SFVital = Bind_Data$ADNRGY42

#Create Domains for SF-6D
Bind_Data$pf6d12 = ifelse(Bind_Data$SFPhys == 3, -.045, 0)
Bind_Data$rf6d12 = ifelse(Bind_Data$SFRole >= 2 & Bind_Data$SFRole <= 4, -.063, 0)
Bind_Data$s6d12 = ifelse(Bind_Data$SFSocial == 2, -.063, ifelse(Bind_Data$SFSocial == 3, -.066, ifelse(Bind_Data$SFSocial == 4, -.081, ifelse(Bind_Data$SFSocial == 5, -.093, 0))))
Bind_Data$bp6d12 = ifelse(Bind_Data$SFPain == 3, -.042, ifelse(Bind_Data$SFPain == 4, -.077, ifelse(Bind_Data$SFPain == 5, -.137, 0)))
Bind_Data$v6d12 = ifelse(Bind_Data$SFVital >= 2 & Bind_Data$SFVital <= 4, -.078, ifelse(Bind_Data$SFVital == 5, -.106, 0))
Bind_Data$mh6d12 = ifelse(Bind_Data$SFMental == 2 | Bind_Data$SFMental == 3, -.059, ifelse(Bind_Data$SFMental == 4, -.113, ifelse(Bind_Data$SFMental == 5, -.134, 0)))
Bind_Data$most = ifelse(Bind_Data$SFPhys == 3 | Bind_Data$SFRole >= 3 | Bind_Data$SFPain >= 4 | Bind_Data$SFVital >= 4 | Bind_Data$SFSocial >= 4 | Bind_Data$SFMental >= 4, -.077, 0)

Bind_Data$sf6d = 1 + Bind_Data$pf6d12 + Bind_Data$rf6d12 + Bind_Data$s6d12 + Bind_Data$bp6d12 + Bind_Data$v6d12 + Bind_Data$mh6d12 + Bind_Data$most
head(Bind_Data)
summary(Bind_Data$sf6d)

#############################
## Set Priority Conditions ##
#############################

#Priority conditions: time period between age at time of survey and age of diagnosis
Bind_Data$DIABAGED[Bind_Data$DIABAGED < 0] <- NA
Bind_Data$MIAGED[Bind_Data$MIAGED < 0] <- NA
Bind_Data$ANGIAGED[Bind_Data$ANGIAGED < 0] <- NA
Bind_Data$CHDAGED[Bind_Data$CHDAGED < 0] <- NA
Bind_Data$STRKAGED[Bind_Data$STRKAGED < 0] <- NA
Bind_Data$AGE31X_DIABAGED = Bind_Data$AGE31X - Bind_Data$DIABAGED #Diabetes
Bind_Data$AGE31X_MIAGED = Bind_Data$AGE31X - Bind_Data$MIAGED #Myocardial Infarction <- Use if interested in time interval between priority condition (15 total) and age at survey
Bind_Data$AGE31X_CHDAGED = Bind_Data$AGE31X - Bind_Data$CHDAGED #Coronary Heart Disease
Bind_Data$AGE31X_ANGIAGED = Bind_Data$AGE31X - Bind_Data$ANGIAGED #Angina/Angina Pectoris
Bind_Data$AGE31X_STRKAGED = Bind_Data$AGE31X - Bind_Data$STRKAGED #Stroke/TIA

#Add yes/no values for priority conditions
Bind_Data$PCDiabetes = ifelse(Bind_Data$AGE31X_DIABAGED == 0 | Bind_Data$AGE31X_DIABAGED == 1, 1, ifelse(Bind_Data$AGE31X_DIABAGED > 1, 2, 0))
Bind_Data$PCHeartAttackMI = ifelse(Bind_Data$AGE31X_MIAGED == 0 | Bind_Data$AGE31X_MIAGED == 1, 1, ifelse(Bind_Data$AGE31X_MIAGED > 1, 2, 0))
Bind_Data$PCCoronaryHeartDisease = ifelse(Bind_Data$AGE31X_CHDAGED == 0 | Bind_Data$AGE31X_CHDAGED == 1, 1, ifelse(Bind_Data$AGE31X_CHDAGED > 1, 2, 0))
Bind_Data$PCStrokeTIAMiniStroke = ifelse(Bind_Data$AGE31X_STRKAGED == 0 | Bind_Data$AGE31X_STRKAGED == 1, 1, ifelse(Bind_Data$AGE31X_STRKAGED > 1, 2, 0))
Bind_Data$PCAngina = ifelse(Bind_Data$AGE31X_ANGIAGED == 0 | Bind_Data$AGE31X_ANGIAGED == 1, 1, ifelse(Bind_Data$AGE31X_ANGIAGED > 1, 2, 0))

####################
## Create Classes ##
####################

#Age
Bind_Data$AGE31X[Bind_Data$AGE31X == -1] <- NA
Bind_Data$AGE31X_Classes <- factor(ifelse(Bind_Data$AGE31X >= 0 & Bind_Data$AGE31X <= 4, 1, ifelse(Bind_Data$AGE31X >= 5 & Bind_Data$AGE31X <= 17, 2, ifelse(Bind_Data$AGE31X >= 18 & Bind_Data$AGE31X <= 24, 3, ifelse(Bind_Data$AGE31X >= 25 & Bind_Data$AGE31X <= 44, 4, ifelse(Bind_Data$AGE31X >= 45 & Bind_Data$AGE31X <= 64, 5, ifelse(Bind_Data$AGE31X >= 65  & Bind_Data$AGE31X <= 85, 6, 0)))))), labels = c("Age 0-4", "Age 5-17", "Age 18-24", "Age 25-44", "Age 45-64", "Age 65-85"))
table(Bind_Data$AGE31X_Classes)

#Sex
Bind_Data$SEX_Classes <- factor(Bind_Data$SEX, labels = c("Male", "Female"))
table(Bind_Data$SEX_Classes)

#Race/Ethnicity
Bind_Data$RACETHX_Classes <- factor(Bind_Data$RACETHX, labels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other Race or Multiple Race"))

#Marital Status
Bind_Data$MARRY31X[Bind_Data$MARRY31X < 0] <- NA
Bind_Data$MARRY31X_Classes <- factor(ifelse(Bind_Data$MARRY31X==1 | Bind_Data$MARRY31X == 7, 1 , ifelse((Bind_Data$MARRY31X >= 2 & Bind_Data$MARRY31X <= 4) | (Bind_Data$MARRY31X >= 8 & Bind_Data$MARRY31X <= 10), 2, 3)), labels = c("Married", "Widowed/divorced/separated", "Never married"))

#Full Year Insurance Coverage Status
Bind_Data$INSURC_Classes <- factor(ifelse(Bind_Data$INSURC == 1 | Bind_Data$INSURC == 8 , 1, ifelse(Bind_Data$INSURC >= 4 & Bind_Data$INSURC <= 6 , 2, ifelse(Bind_Data$INSURC == 2, 3, ifelse(Bind_Data$INSURC == 3 | Bind_Data$INSURC == 7,4, 0)))), labels=c("Any Private", "Medicare", "Other Public", "Uninsured"))

#Family's Total Income
Bind_Data$POVCAT_Classes <- factor(Bind_Data$POVCAT, labels = c("Poor/Negative","Near Poor","Low Income", "Middle Income","High Income"))
summary(Bind_Data$POVCAT_Classes)

#Education (Highest Degree)
Bind_Data$EDUCATION[Bind_Data$EDUCATION == 0] <- NA
Bind_Data$EDUCATION_Classes <- factor(Bind_Data$EDUCATION, labels = c("No Degree", "GED/HS", "Associate/Bachelor", "Master/Doctorate"))

#Body Mass Index (BMI)
Bind_Data$BMINDX53[Bind_Data$BMINDX53 < 0] = NA
Bind_Data$BMINDX53_Classes <- factor(ifelse(Bind_Data$BMINDX53 <= 0, 0, ifelse(Bind_Data$BMINDX53 > 0 & Bind_Data$BMINDX53 < 18.5, 1, ifelse(Bind_Data$BMINDX53 >= 18.5 & Bind_Data$BMINDX53 < 25, 2, ifelse(Bind_Data$BMINDX53 >= 25 & Bind_Data$BMINDX53 < 30, 3, ifelse(Bind_Data$BMINDX53 >= 30, 4, 0))))), labels=c("Underweight","Normal weight","Overweight","Obese"))

#Charlson Comorditiy Index
Bind_Data$Charlson_Classes <- factor(ifelse(Bind_Data$Charlson == 0, 0, ifelse(Bind_Data$Charlson == 1, 1, ifelse(Bind_Data$Charlson == 2, 2, 3))), labels=c("0", "1", "2", "3 to 10"))

#Priority Condition Class
Bind_Data$PriorityCondition_Recent_Previous_Classes <- factor(ifelse(Bind_Data$PCHeartAttackMI == 0 | Bind_Data$PCHeartAttackMI == 1, 0, 1), labels=c("Priority Condition Recent", "Priority Condition Previous")) #Need to update for priority condition of interest

#Myocardial Infarction Class
Bind_Data$MyocardialInfarction_Classes <- factor(ifelse(Bind_Data$MedicalCondition_1_Grouped == 0, 0, 1), labels=c("No Myocardial Infarction", "Myocardial Infarction"))

#Ischemic Stroke Class
Bind_Data$IschemicStroke_Classes <- factor(ifelse(Bind_Data$MedicalCondition_2_Grouped == 0, 0, 1), labels=c("No Ischemic Stroke", "Ischemic Stroke"))

#Heart Failure Class
Bind_Data$HeartFailure_Classes <- factor(ifelse(Bind_Data$MedicalCondition_3_Grouped == 0, 0, 1), labels=c("No Heart Failure", "Heart Failure"))

#Cardiac Dysrhythmias Class
Bind_Data$CardiacDysrhythmias_Classes <- factor(ifelse(Bind_Data$MedicalCondition_4_Grouped == 0, 0, 1), labels=c("No Cardiac Dysrhythmias", "Cardiac Dysrhythmias"))

#Angina Pectoris Class
Bind_Data$AnginaPectoris_Classes <- factor(ifelse(Bind_Data$MedicalCondition_5_Grouped == 0, 0, 1), labels=c("No Angina Pectoris", "Angina Pectoris"))

#Peripheral Artery Disease Class
Bind_Data$PeripheralArteryDisease_Classes <- factor(ifelse(Bind_Data$MedicalCondition_6_Grouped == 0, 0, 1), labels=c("No Peripheral Artery Disease", "Peripheral Artery Disease"))

#Diabetes Class
Bind_Data$Diabetes_Classes <- factor(ifelse(Bind_Data$MedicalCondition_7_Grouped == 0, 0, 1), labels=c("No Diabetes", "Diabetes"))

#Define Survey Design
options(survey.lonely.psu='adjust')

######################
## Analysis Section ##
######################

n_years = 5

Bind_Data$LONGWT_n_years <- Bind_Data$LONGWT/n_years #LONGWT = Longitudinal Weights to weight patients for the 5 years of data 
Bind_Data$LONGWT_n_years <- ifelse(is.na(Bind_Data$LONGWT_n_years)==T,0,Bind_Data$LONGWT_n_years)

Bind_Data$AllPayers_Total_plus1 = ifelse(round(Bind_Data$AllPayers_Total) == 0, 1, round(Bind_Data$AllPayers_Total)) #switch for gamma distribution for annual expenditures

mepsdsgn = svydesign(id = ~VARPSU,
                     strata = ~VARSTR,
                     weights = ~LONGWT_n_years,
                     data = Bind_Data,
                     nest = TRUE)

#####################################
## Define Population and Subgroups ##
#####################################

#ICD Method
Population_Analysis = mepsdsgn %>% #The 2nd year of each panel is selected in this step
  subset(((PANEL == 16 & Year == 2012) | (PANEL == 17 & Year == 2013) | (PANEL == 18 & Year == 2014) | (PANEL == 19 & Year == 2015) | (PANEL == 20 & Year == 2016)) & (AGE31X >= 18))

Subgroup_Analysis = mepsdsgn %>% #Change the Medical Condition #(s) to the medical condition(s) of interest
  subset(((PANEL == 16 & Year == 2012) | (PANEL == 17 & Year == 2013) | (PANEL == 18 & Year == 2014) | (PANEL == 19 & Year == 2015) | (PANEL == 20 & Year == 2016)) & ((MedicalCondition_1_Grouped == 1))& (AGE31X >= 18))

#Priority Conditions Method
Subgroup_Analysis_Priority_Recent = mepsdsgn %>%
  subset((PCAngina == 0 | PCAngina == 1) & (AGE31X >= 18)) #Select priority condition- make sure difference between age at time of survey and age of diagnosis is 0 or positive and the age of diagnosis is positive

Subgroup_Analysis_Priority_Previous = mepsdsgn %>%
  subset(((PCAngina == 2)) & (AGE31X >= 18)) #Select priority condition- make sure difference between age at time of survey and age of diagnosis is 0 or positive and the age of diagnosis is positive

#Weighted Total
svytotal(~AGE31X_Classes, design = Population_Analysis)
svytotal(~AGE31X_Classes, design = Subgroup_Analysis)

#######################
## Manuscript Tables ##
#######################

#Table 1: Medical Conditions including Cardiovascular Disease (CVD) Diagnoses and Diabetes <- Contains all medical condition definitions (in manuscript)

#Table 2: SF-6D Utility Scores by CVD Type
svymean(~sf6d, design = Population_Analysis, na.rm = T)
svyquantile(~sf6d, design = Population_Analysis, quantiles = c(.25, .5, .75), na.rm = T)
confint(svymean(~sf6d, design = Population_Analysis, na.rm = T))

svymean(~sf6d, design = Subgroup_Analysis, na.rm = T)
svyquantile(~sf6d, design = Subgroup_Analysis, quantiles = c(.25, .5, .75), na.rm = T)
confint(svymean(~sf6d, design = Subgroup_Analysis, na.rm = T))

svymean(~sf6d, design = Subgroup_Analysis_Priority_Recent, na.rm = T)
svyquantile(~sf6d, design = Subgroup_Analysis_Priority_Recent, quantiles = c(.25, .5, .75), na.rm = T)
confint(svymean(~sf6d, design = Subgroup_Analysis_Priority_Recent, na.rm = T))
dim(Subgroup_Analysis_Priority_Recent)

svymean(~sf6d, design = Subgroup_Analysis_Priority_Previous, na.rm = T)
svyquantile(~sf6d, design = Subgroup_Analysis_Priority_Previous, quantiles = c(.25, .5, .75), na.rm = T)
confint(svymean(~sf6d, design = Subgroup_Analysis_Priority_Previous, na.rm = T))
dim(Subgroup_Analysis_Priority_Previous)

Subgroup_Analysis_Priority_PValue = svyglm(sf6d ~ PriorityCondition_Recent_Previous_Classes, design = Population_Analysis) #p value calculation
summary(Subgroup_Analysis_Priority_PValue)

#Table 3: Annual Total Expenditures by CVD Type
svymean(~AllPayers_Total, design = Population_Analysis, family = Gamma(link = "log")) #Mean total expenditures with standard error (Gamma distribution)
svyquantile(~AllPayers_Total, design = Population_Analysis, quantiles = c(.25, .5, .75), na.rm = T) #Median total expenditures with 25th and 75th percentiles
confint(svymean(~AllPayers_Total, design = Population_Analysis, na.rm = T)) #Confidence interval (95%)

svymean(~AllPayers_Total, design = Subgroup_Analysis, family = Gamma(link = "log"))

svymean(~AllPayers_Total, design = Subgroup_Analysis_Priority_Recent, family = Gamma(link = "log"))

svymean(~AllPayers_Total, design = Subgroup_Analysis_Priority_Previous, family = Gamma(link = "log"))

Subgroup_Analysis_Priority_PValue = svyglm(AllPayers_Total ~ PriorityCondition_Recent_Previous_Classes, design = Population_Analysis) #p value calculation
summary(Subgroup_Analysis_Priority_PValue)

#Table 4: Impact of Diagnoses and Patient Characteristics on Utility Values (SF-6D) and Total Annual Expenditures
Population_LinearRegression_sf6d = svyglm(sf6d ~ MyocardialInfarction_Classes + IschemicStroke_Classes + HeartFailure_Classes + CardiacDysrhythmias_Classes + AnginaPectoris_Classes + PeripheralArteryDisease_Classes + Diabetes_Classes + AGE31X_Classes + SEX_Classes + RACETHX_Classes + INSURC_Classes + POVCAT_Classes + EDUCATION_Classes + BMINDX53_Classes + Charlson_Classes, design = Population_Analysis)
summary(Population_LinearRegression_sf6d)
confint(Population_LinearRegression_sf6d)

Population_LinearRegression_AllPayers_Total = svyglm(AllPayers_Total_plus1 ~ MyocardialInfarction_Classes + IschemicStroke_Classes + HeartFailure_Classes + CardiacDysrhythmias_Classes + AnginaPectoris_Classes + PeripheralArteryDisease_Classes + Diabetes_Classes + AGE31X_Classes + SEX_Classes + RACETHX_Classes + INSURC_Classes + POVCAT_Classes + EDUCATION_Classes + BMINDX53_Classes + Charlson_Classes, design = Population_Analysis, family = Gamma(link = "log"))
summary(Population_LinearRegression_AllPayers_Total)
confint(Population_LinearRegression_AllPayers_Total)

#####################
## Appendix Tables ##
#####################

#Appendix Table 1: Population and Sample Characteristics at Baseline
Population_Table_2 <- svymean(~factor(Diabetes_Classes) + factor(AGE31X_Classes) + factor(SEX_Classes) +factor(RACETHX_Classes) + factor(INSURC_Classes) + factor(POVCAT_Classes) + factor(EDUCATION_Classes) + factor(BMINDX53_Classes) + factor(Charlson_Classes), design = Population_Analysis, na.rm = T)
View(Population_Table_2)

Subgroup_Table_2 <- svymean(~factor(Diabetes_Classes) + factor(AGE31X_Classes) + factor(SEX_Classes) +factor(RACETHX_Classes) + factor(INSURC_Classes) + factor(POVCAT_Classes) + factor(EDUCATION_Classes) + factor(BMINDX53_Classes) + factor(Charlson_Classes), design = Subgroup_Analysis, na.rm = T)
View(Subgroup_Table_2)

#Appendix Table 2: Multivariable Linear Regression of SF-6D Utility Scores by CVD Type and Patient Characteristics 
Population_LinearRegression_sf6d = svyglm(sf6d ~ MyocardialInfarction_Classes + IschemicStroke_Classes + HeartFailure_Classes + CardiacDysrhythmias_Classes + AnginaPectoris_Classes + PeripheralArteryDisease_Classes + Diabetes_Classes + AGE31X_Classes + SEX_Classes, design = Population_Analysis) #Model 1
summary(Population_LinearRegression_sf6d)
confint(Population_LinearRegression_sf6d)
plot(Population_LinearRegression_sf6d)

Population_LinearRegression_sf6d = svyglm(sf6d ~ MyocardialInfarction_Classes + IschemicStroke_Classes + HeartFailure_Classes + CardiacDysrhythmias_Classes + AnginaPectoris_Classes + PeripheralArteryDisease_Classes + Diabetes_Classes + AGE31X_Classes + SEX_Classes + RACETHX_Classes + INSURC_Classes + POVCAT_Classes + EDUCATION_Classes, design = Population_Analysis) #Model 2
summary(Population_LinearRegression_sf6d)
confint(Population_LinearRegression_sf6d)

#Appendix Table 3: Multivariable Generalized Gamma Regression of Total Annual Expenditures by CVD Type and Patient Characteristics 
Population_LinearRegression_AllPayers_Total = svyglm(AllPayers_Total_plus1 ~ MyocardialInfarction_Classes + IschemicStroke_Classes + HeartFailure_Classes + CardiacDysrhythmias_Classes + AnginaPectoris_Classes + PeripheralArteryDisease_Classes + Diabetes_Classes + AGE31X_Classes + SEX_Classes + RACETHX_Classes + INSURC_Classes + POVCAT_Classes + EDUCATION_Classes, design = Population_Analysis, family = Gamma(link = "log")) #Model 1
summary(Population_LinearRegression_AllPayers_Total)
confint(Population_LinearRegression_AllPayers_Total)

Population_LinearRegression_AllPayers_Total = svyglm(AllPayers_Total_plus1 ~ MyocardialInfarction_Classes + IschemicStroke_Classes + HeartFailure_Classes + CardiacDysrhythmias_Classes + AnginaPectoris_Classes + PeripheralArteryDisease_Classes + Diabetes_Classes + AGE31X_Classes + SEX_Classes + RACETHX_Classes + INSURC_Classes + POVCAT_Classes + EDUCATION_Classes + BMINDX53_Classes + Charlson_Classes, design = Population_Analysis, family = Gamma(link = "log")) #Model 2
summary(Population_LinearRegression_AllPayers_Total)
confint(Population_LinearRegression_AllPayers_Total)

###############################################################