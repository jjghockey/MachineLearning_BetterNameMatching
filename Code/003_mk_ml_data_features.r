#####################################################################################################	
# 	Project: 	Name Matching via Machine Learning													#
#	Author: 	Jeremy Guinta																		#
#	Date: 		6/6/2019																			#
#	Script: 	003_mk_ml_data_features.r															#
#	Purpose: 	Build a dataset for machine learning purposes										#		        
#####################################################################################################

# I. Program Setup  -------------------------------------------------------

#	Remove Objects
rm(list=ls())

# Clear Memory
gc(reset=TRUE)

# Set Working Directory
setwd("C:/h2o/")

#Load Packages
require(data.table)
require(tidyverse)
require(dtplyr)
require(stringr)
require(reshape2)
require(data.table)

#Set Options
options(scipen=30) #Sets numeric width to 30 before scientific notation starts

#Load Custom Functions
source("./name_stringdist.r")
source("./name_stringdist_namepiece.r")

# II. Load Data -----------------------------------------------------------
#rvw<-fread("//wdc1islfls02/CHI1FLS02_TSP/LosAngeles/Admin/001_Users/axa/002_ml_data_rvw.csv", na.strings=c("", NULL, NA))
rvw<-fread("./002_ml_data_rvw.csv", na.strings=c("", NULL, NA))
rvw<-rvw[is.na(category)==FALSE, ]  #Drop unlabeled values


# III. Data Processing -----------------------------------------------------
#A. Build a full name 
	rvw[, full_name1:=paste(first_name1, ifelse(is.na(middle_name1)==TRUE," ", middle_name1), last_name1, sep=" ")]
	rvw[, full_name1:=str_trim(full_name1)]
	rvw[, full_name2:=paste(first_name2, ifelse(is.na(middle_name2)==TRUE," ", middle_name2), last_name2, sep=" ")]
	rvw[, full_name2:=str_trim(full_name2)]
	
	for (i in 1:10) {
		rvw[, full_name1:=gsub("  ", "", full_name1)] #Get rid of excess spaces within names
		rvw[, full_name2:=gsub("  ", "", full_name2)] #Get rid of excess spaces within names
	}

#A. Build String Distance Features
	#1. First Name Feature
	rvw<-name_stringdist(dta=c("rvw"), string1=c("first_name1"), string2=c("first_name2"))
	rvw<-name_stringdist(dta=c("rvw"), string1=c("last_name1"), string2=c("last_name2"))
	rvw<-name_stringdist(dta=c("rvw"), string1=c("middle_name1"), string2=c("middle_name2"))
	rvw<-name_stringdist(dta=c("rvw"), string1=c("full_name1"), string2=c("full_name2"))
	
	rvw<-name_stringdist(dta=c("rvw"), string1=c("first_name1"), string2=c("last_name2"))
	rvw<-name_stringdist(dta=c("rvw"), string1=c("first_name2"), string2=c("last_name1"))
		
	rvw<-name_stringdist_namepiece(dta=c("rvw"), string1=c("first_name1"), string2=c("first_name2"), group_name=c("f1_f2"))
	rvw<-name_stringdist_namepiece(dta=c("rvw"), string1=c("last_name1"), string2=c("last_name2"), group_name=c("l1_l2"))
	rvw<-name_stringdist_namepiece(dta=c("rvw"), string1=c("middle_name1"), string2=c("middle_name2"), group_name=c("m1_m2"))
	rvw<-name_stringdist_namepiece(dta=c("rvw"), string1=c("full_name1"), string2=c("full_name2"), group_name=c("fn1_fn2"))

	rvw<-name_stringdist_namepiece(dta=c("rvw"), string1=c("first_name1"), string2=c("last_name2"), group_name=c("f1_l2"))
	rvw<-name_stringdist_namepiece(dta=c("rvw"), string1=c("first_name2"), string2=c("last_name1"), group_name=c("f2_l1"))
	
	#2. Replace NA with zero
	rvw<-as.data.frame(rvw)
	rvw[is.na(rvw)]<-0
	rvw<-as.data.table(rvw)
	
# IV. Data Analysis ---------------------------------------------------------

# V. Data Output ------------------------------------------------------------
saveRDS(file="./003_ml_data.rds", rvw)

