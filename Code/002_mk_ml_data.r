#####################################################################################################	
# 	Project: 	Name Matching via Machine Learning													#
#	Author: 	Jeremy Guinta																		#
#	Date: 		6/6/2019																			#
#	Script: 	002_mk_ml_data.r																	#
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

# II. Load Data -----------------------------------------------------------

name1<-readRDS("./001_name1.rds")
name2<-readRDS("./001_name2.rds")
name3<-readRDS("./001_name3.rds")

# III. Data Processing -----------------------------------------------------

#A. Explode the data and perform matches across sets and within set by phone number
	#1. Rename columns so match does not create .x and .y columns
	name1[, first_name1:=first_name]
	name1[, last_name1:=last_name]
	name1[, middle_name1:=middle_name]
	name1[, c("first_name", "last_name", "middle_name"):=NULL]
	
	name2[, first_name2:=first_name]
	name2[, last_name2:=last_name]
	name2[, middle_name2:=middle_name]
	name2[, c("first_name", "last_name", "middle_name"):=NULL]
	
	name3[, first_name3:=first_name]
	name3[, last_name3:=last_name]	
	name3[, middle_name3:=middle_name]	
	name3[, c("first_name", "last_name", "middle_name"):=NULL]	
	
	#2. Initial matching across files
	set1<-merge(name1, name2, by=c("phone_number"), allow.cartesian=TRUE)
	set2<-merge(name1, name3, by=c("phone_number"), allow.cartesian=TRUE)
	names(set2)<-c("phone_number", "first_name1", "last_name1", "middle_name1", "first_name2", "last_name2", "middle_name2")
	
	set3<-merge(name2, name3, by=c("phone_number"), allow.cartesian=TRUE)	
	names(set3)<-c("phone_number", "first_name1", "last_name1", "middle_name1", "first_name2", "last_name2", "middle_name2")
	
#B. Explode the data within file by matching within data set by phone number
	#1. Prepare data for second matching within files.  Only name1 and name2 have multiple rows per phone number
	name1[, first_name2:=first_name1]
	name1[, last_name2:=last_name1]
	name1[, middle_name2:=middle_name1]
	
	name1a<-name1[, .(phone_number, first_name1, last_name1, middle_name1)]
	name1b<-name1[, .(phone_number, first_name2, last_name2, middle_name2)]

	name2[, first_name1:=first_name2]
	name2[, last_name1:=last_name2]
	name2[, middle_name1:=middle_name2]
	
	name2a<-name2[, .(phone_number, first_name1, last_name1, middle_name1)]
	name2b<-name2[, .(phone_number, first_name2, last_name2, middle_name2)]
	
	#2. Second matching within each file
	set4<-merge(name1a, name1b, by=c("phone_number"), allow.cartesian=TRUE)
	set5<-merge(name2a, name2b, by=c("phone_number"), allow.cartesian=TRUE)
	
#C. Combine all of the sets
	ml_data<-rbind(set1,set2,set3,set4,set5)
	rm(set1,set2,set3,set4,set5)
	
# IV. Data Analysis ---------------------------------------------------------
	#A. Do an initial string distance comparison
	ml_data[, str_len1:=pmax(str_length(first_name1), str_length(first_name2))]
	ml_data[, str_len2:=pmax(str_length(last_name1), str_length(last_name2))]
	
	ml_data[, str_dist1:=round(1-((stringdist(first_name1, first_name2, method=c("lv")))/str_len1),1)]
	ml_data[, str_dist2:=round(1-((stringdist(last_name1, last_name2, method=c("lv")))/str_len2),1)]

	ml_data[, str_dist_all:=(stringdist(first_name1, first_name2, method=c("lv")))+(stringdist(last_name1, last_name2, method=c("lv")))]
	ml_data[, str_dist_all:=round(1-(str_dist_all/(str_len1+str_len2)),1)]
	
	#B. Tier sample selection by each percentile tier
		#1. Order the data by unique key	
		setkey(ml_data, phone_number)
		
		#2. Build Random
		set.seed(1)
		rand<-runif(nrow(ml_data), 0,1)
		ml_data<-cbind(ml_data, rand)
	
		#3. Build selection tier
		ml_data[, tot:=.N, by=str_dist_all]
		ml_data[, tot_sel:=tot*0.04]
		setkey(ml_data, str_dist_all,rand)
		ml_data[, ord:=1]
		ml_data[, ord:=cumsum(ord), by=list(str_dist_all)]
		ml_data[, keep:=0]
		ml_data[ord<=tot_sel, keep:=1]
		ml_data[str_dist_all==1, keep:=0]  #No need to review exact matches
		
	#C. Add specific buckets that are interesting (These are consider edge cases)
		ml_data[keep==0 & str_dist_all>=0.30 & str_dist_all<=0.60 & str_dist2>=0.80 & str_dist2<=0.95 & str_dist1>=0.40 & str_dist1<=0.60,keep:=1]
		ml_data[keep==0 & str_dist_all>=0.70 & str_dist_all<=0.85 & str_dist2>=0.70 & str_dist2<=0.95 & str_dist1>=0.60 & str_dist1<=0.85,keep:=1]
		ml_data[keep==0 & str_dist_all>=0.60 & str_dist_all<=0.70 & str_dist2>=0.70 & str_dist2<=0.85 & str_dist1>=0.50 & str_dist1<=0.75,keep:=1]
		
# V. Data Output ------------------------------------------------------------
	fwrite(file="./002_ml_data_norvw.csv", ml_data[keep==0, .(phone_number, first_name1, first_name2, middle_name1, middle_name2, last_name1, last_name2)][order(phone_number)])
	fwrite(file="./002_ml_data_rvw.csv", ml_data[keep==1, .(phone_number, first_name1, first_name2, middle_name1, middle_name2, last_name1, last_name2)][order(phone_number)])

	saveRDS(file="./002_ml_data.rds", ml_data[, .(phone_number, first_name1, first_name2, middle_name1, middle_name2, last_name1, last_name2, keep)])