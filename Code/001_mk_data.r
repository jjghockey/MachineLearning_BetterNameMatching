#####################################################################################################	
# 	Project: 	Name Matching via Machine Learning													#
#	Author: 	Jeremy Guinta																		#
#	Date: 		6/6/2019																			#
#	Script: 	001_mk_data.r																		#
#	Purpose: 	Prepare the raw data into a usable format											#	
#				Phone Numbers will be been anonomized 												#        
#####################################################################################################

# I. Program Setup  -------------------------------------------------------

#	Remove Objects
rm(list=ls())

# Clear Memory
gc(reset=TRUE)

# Set Working Directory
setwd("C:/users/jguinta/desktop/")

#Load Packages
require(data.table)
require(tidyverse)
require(dtplyr)
require(stringr)
require(reshape2)

#Set Options
options(scipen=30) #Sets numeric width to 30 before scientific notation starts

#Set Functions 
no_na <- function(data, desiredCols) { #This function elimnates a row of data if ALL the desired columns are NA
  data$no_na<-0
  for(i in desiredCols){
    data$temp <- !is.na(data[[i]])
    data$no_na <- data$no_na + data$temp
  }
  data <- data %>% filter(no_na>0) %>% select(-no_na,-temp)
  return(data)
}

# II. Load Data -----------------------------------------------------------
exp <- fread("C:/users/jguinta/desktop/RPA_FinalOutput.txt", stringsAsFactors=FALSE, colClasses="character", na.strings = c("", " ", "  ", "-", "NULL", "NA", "'na"))
mcr <- fread("C:/users/jguinta/desktop/2017.09.26 BATCH REQUEST_output.csv", stringsAsFactors = FALSE, colClasses="character", na.strings = c("", " ", "  ", "-", "NULL", "NA", "'na"))
tu <- fread("C:/users/jguinta/desktop/address append.csv", stringsAsFactors = FALSE, colClasses="character", na.strings = c("", " ", "  ", "-", "NULL", "NA", "'na"))

# III. Process Data ---------------------------------------------------

#A. Columns to lowercase and separated by _
  #exp 
  names(exp) <- unlist(lapply(names(exp), function(x) {
                        return(tolower(str_replace_all(x, "[.]", "_"))) #replace periods with underscore, lowercase
                      }))
  
  #mcr
  names(mcr)<- unlist(lapply(names(mcr), function(x){
          w <- NULL
          if(str_detect(x, regex("^i"))) { #removes leading "i" on some variable names
            w <- substr(x, 2, nchar(x))
          } else{
            w <- x
          }
          #pastes all words in variable name together with underscores; lowercase
          return(tolower(paste(unlist(str_extract_all(w, "[A-Z][a-z]+[0-9]+|[A-Z][a-z]+|[a-z]+|[A-Z]+")), collapse = "_")))
        }))
  
  #tu
  names(tu) <- unlist(lapply(names(tu), function(x) {
          tmp <- str_replace_all(x,"Tlo", "_") #Replaces all "Tlo" strings with underscore
          if(substr(tmp, 1,1)=="_"){
            tmp <- substr(tmp, 2, nchar(tmp)) #removes leading underscore
          }
          #pastes all words in variable name together with underscores;lowercase
          return(tolower(paste(unlist(str_extract_all(tmp,"[A-Z][a-z]+[0-9]+|[A-Z][a-z]+|[a-z]+|[A-Z]+")), collapse="_")))
        }))
  
#B. Change date variables to date

date_vars <- names(tu)[str_detect(names(tu), "date_first_seen|date_last_seen")]
  for(i in seq_along(date_vars)){
    tu[[date_vars[i]]] <- as.Date(tu[[date_vars[i]]], format="%m/%d/%Y")
  }
  
date_vars_mcr <- names(mcr)[str_detect(names(mcr), "first_seen|last_seen")]
  for(i in date_vars_mcr){
    mcr[[i]] <- as.Date(mcr[[i]], format="%m/%d/%Y")
  }

#C. TransUnion - Transform to Long
addr <- unique(str_replace_all(str_replace_all(str_replace_all(names(tu),"subject[0-9]_",""),"address[0-9]_",""),"address[0-9][0-9]_",""))
names <- addr[str_detect(addr,"name_")]
addr <- addr[!addr %in% c(names,"phonenumber")]

tu_l <- data.frame()
tu<-as.data.frame(tu)
for(i in c(1:5)){
  for(a in c(1:10)){
    name_add <- paste0("subject",i,"_",names)
    addr_add <- paste0("subject",i,"_","address",a,"_",addr)
    x <- tu[,c("phonenumber",name_add,addr_add)]
    
    x <- no_na(x, c(name_add,addr_add))
    
    names(x) <- c("phonenumber",names,addr)
    
    x$tu_subject <- i
    x$tu_address <- a
    
    tu_l <- bind_rows(tu_l,x)
  }
}

tl_noaddr <- tu_l %>% 
distinct_(.dots=names(tu_l)[names(tu_l)!="tu_address"]) %>% 
group_by_(.dots=c("phonenumber",names, "tu_subject")) %>%
mutate(count=n()) %>%
filter(count==1) %>%
select_(.dots=c("phonenumber",names, "tu_subject")) %>%
ungroup() %>%
group_by_(.dots=c("phonenumber",names)) %>%
mutate(full_address_in="**No Address Information Provided for this Phone Number and Name**",
	 tu_address_in="1-10",
	 tu_subject_in=paste(tu_subject, collapse=", ")) %>%
ungroup() 
  
tu_l <- full_join(tu_l, tl_noaddr, by=c("phonenumber",names, "tu_subject"))

tu_l <- tu_l %>%
mutate(full_address = ifelse(!is.na(full_address_in), full_address_in, full_address),
	 tu_address = ifelse(!is.na(tu_address_in), tu_address_in, tu_address),
	 tu_subject = ifelse(!is.na(tu_subject_in), tu_subject_in, tu_subject),
	 tu_address = paste0("tu_address", tu_address),
	 tu_subject = paste0("tu_subject", tu_subject)) %>%
select(-full_address_in,
	 -tu_address_in,
	 -tu_subject_in) %>%
distinct()

tu_l <- no_na(tu_l, c(addr))

# B. MicroBilt Transform to Long
x <- names(mcr)[17:40]
nm_mcr <- c("status_detail", "phone_type", "listing_type", "use_type", "carrier_name", "first_seen", "last_seen", "times_seen", 
            "alert_code1", "alert_message1", "alert_code2", "alert_message2", "alert_code3", "alert_message3", "first_name", 
            "middle_name", "last_name", "suffix", "business_name", "business_category", "business_captions", "street_address", 
            "city", "state")

mcr_l <- data.frame()
mcr<-as.data.frame(mcr)
for(i in c(1:10)){
  add <- paste0("phone_listing",i,"_",nm_mcr)
  x <- mcr[,c("key","phone",add)]
  names(x) <- c("key","phone", nm_mcr)
  
  x <- no_na(x, c(nm_mcr))
  
  x$mcr_ord<-paste0("phone_listing",i)
  mcr_l <- bind_rows(mcr_l, x)
}

# C. Make Experian Transform to Long
nm_exp <- names(exp)[!(names(exp) %in% c("primary_key", "primary_phone", "match_code"))]
exp_l <- no_na(exp, nm_exp)

# D. Clean data inputs and limit data to necessecy columns

mcr_l<-as.data.table(mcr_l)
exp_l<-as.data.table(exp_l)
tu_l<-as.data.table(tu_l)

exp_l<-exp_l[, .(phone_number=primary_phone, first_name=`first name`, last_name=`last name`, middle_name=`middle initial`)]
mcr_l[, phone_number:=gsub("\\'","", phone) ]
mcr_l<-mcr_l[, .(phone_number, first_name, middle_name, last_name, first_seen, last_seen)]

tu_l<-tu_l[, .(phone_number=phonenumber, first_name=name_first_name, last_name=name_last_name, middle_name=name_middle_name, first_seen=date_first_seen, last_see=date_last_seen)]

#E. Anonomize Phone Numbers via simple algorithm
	#1. Combine all unique phone numbers into a single data source
	phn<-unique(rbind(unique(exp_l[, .(phone_number)]), unique(mcr_l[, .(phone_number)]), unique(tu_l[, .(phone_number)])))
	phn[, phone_number_new:=as.character(phone_number)]
		
	#2. Replace all numbers with that number plus some random noise
	rnd<-function(dta=c(), seed=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), vr=c("all") ) {

		dta<-as.data.table(get(dta))
		for (i in seed) {
			j<-as.numeric(i)
			dta[, val:=as.numeric(substr(phone_number_new,j,j))]
			rand<-rnorm(nrow(phn), 0,2)
			dta<-cbind(dta, rand)
			
			dta[, val:=round(val+rand,0)]
			dta[val>10, val:=val-10,]
			dta[val<0, val:=10+val]	
			dta[val==10, val:=round(rand,0)]
						
			newvar<-paste(vr, j, sep="")
			dta[, c(newvar):=val]
			dta[, rand:=NULL]
			
			cnt<-dta[val>=10 | val<0, .N]
			stopifnot(cnt==0)
			
		}
		return(dta)
	}
	
	phn<-rnd(dta=c("phn"), seed=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), vr=c("all"))
	
	
	phn[all1==0, all1:=1] #Replace any leading zeros with a one
	phn[, phone_number_new:=paste(all1,all2,all3,all4,all5,all6,all7,all8,all9,all10, sep="")]
	cnt<-phn[phone_number_new==phone_number, .N]
	stopifnot(cnt==0)
	phn<-phn[, .(phone_number, phone_number_new)]
	
	#3. Match back and replace phone numbers
	
	phn[, m:=1]
	exp_l[, m:=1]
	mcr_l[, m:=1]
	tu_l[, m:=1]
	exp_l<-merge(exp_l, phn, by=c("phone_number"), all=TRUE)
	mcr_l<-merge(mcr_l, phn, by=c("phone_number"), all=TRUE)
	tu_l<-merge(tu_l, phn, by=c("phone_number"), all=TRUE)
	
	exp_l[, phone_number:=phone_number_new]
	exp_l[, phone_number_new:=NULL]

	mcr_l[, phone_number:=phone_number_new]
	mcr_l[, phone_number_new:=NULL]

	tu_l[, phone_number:=phone_number_new]
	tu_l[, phone_number_new:=NULL]

#F. Basic cleaning and standardization
	
	tu_l[, first_name:=tolower(first_name)]
	tu_l[, last_name:=tolower(last_name)]
	tu_l[, middle_name:=tolower(middle_name)]

	exp_l[, first_name:=tolower(first_name)]
	exp_l[, last_name:=tolower(last_name)]
	exp_l[, middle_name:=tolower(middle_name)]

	mcr_l[, first_name:=tolower(first_name)]
	mcr_l[, last_name:=tolower(last_name)]
	mcr_l[, middle_name:=tolower(middle_name)]
	
	tu_l[, phone_number:=as.numeric(phone_number)]
	exp_l[, phone_number:=as.numeric(phone_number)]
	mcr_l[, phone_number:=as.numeric(phone_number)]
	
#G. Remove Duplicates and missings
	exp_l<-exp_l[is.na(first_name)==FALSE & is.na(last_name)==FALSE]
	mcr_l<-mcr_l[is.na(first_name)==FALSE & is.na(last_name)==FALSE]
	tu_l<-tu_l[is.na(first_name)==FALSE & is.na(last_name)==FALSE]
	
	exp_l<-unique(exp_l[, .(phone_number, first_name, middle_name, last_name)])
	mcr_l<-unique(mcr_l[, .(phone_number, first_name, middle_name, last_name)])
	tu_l<-unique(tu_l[, .(phone_number, first_name, middle_name, last_name)])
	
#IV. Data Output -------------------------------------------------------------------------------

saveRDS(file="C:/users/jguinta/desktop/001_name1.rds", mcr_l)
saveRDS(file="C:/users/jguinta/desktop/001_name2.rds", tu_l)
saveRDS(file="C:/users/jguinta/desktop/001_name3.rds", exp_l)

