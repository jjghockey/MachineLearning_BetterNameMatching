#Version		: 	1.0
#Author			: 	jjg 
#Last Update	: 	8/9/2019
#Purpose		: 	Build function to take name input to predict probability of match.
#Argument(s)	:	dataset of names, mapping of fields to names
#Input 			:	dataset 
#Output			:	dataset with probability of match and match decision based on user theshold
#External		:	
#Example Syntax	: 	name_match(dta=c("dta"), fn1=c("first_name1"), fn2=c("first_name2")
#										   , ln1=c("last_name1"), ln2=c("last_name2")
#										   , mn1=c("last_name1"), mn2=c("last_name2")
#										   , h2o=c("C:/h2o/namematch/"), nthreads=2, min_mem_size="8G", match_threshold=0.50)

#Notes			:  
#Packages		: 	stringdist, tidyverse, data.table, dtplyr, stringr, sqldf
 
name_match<-function(dta=c(""), fn1=c(""), fn2=c(""), ln1=c(""), ln2=c(""), mn1=c(""), mn2=c(""), h2o=c("C:/h2o/namematch/"), model=c(""), nthreads=2, min_mem_size="8G", match_threshold=c(0.50)) {

#0. Parameter Input checks
	options(scipen=999)

#1. Required Functions and Packages
	require(stringdist)
	require(tidyverse)
	require(data.table)
	require(dtplyr)
	require(stringr)
	require(sqldf)
	require(rJava)
	require(h2o)
	
	x<-as.data.table(get(dta))
	maxcol<-ncol(x)

#2. Standardize Inputs
	#a. Build names
	x[, first_name1:=get(fn1)]
	x[, first_name2:=get(fn2)]
	x[, last_name1:=get(ln1)]
	x[, last_name2:=get(ln2)]
	x[, middle_name1:=get(mn1)]
	x[, middle_name2:=get(mn2)]

	x[, first_name1:=tolower(str_trim(first_name1))]
	x[, first_name2:=tolower(str_trim(first_name2))]
	x[, last_name1:=tolower(str_trim(last_name1))]
	x[, last_name2:=tolower(str_trim(last_name2))]
	x[, middle_name1:=tolower(str_trim(middle_name1))]
	x[, middle_name2:=tolower(str_trim(middle_name2))]	
	
#3. Build features
	#a. Build a full name 
	x[, full_name1:=paste(first_name1, ifelse(is.na(middle_name1)==TRUE," ", middle_name1), last_name1, sep=" ")]
	x[, full_name1:=str_trim(full_name1)]
	x[, full_name2:=paste(first_name2, ifelse(is.na(middle_name2)==TRUE," ", middle_name2), last_name2, sep=" ")]
	x[, full_name2:=str_trim(full_name2)]
	
	for (i in 1:10) {
		x[, full_name1:=gsub("  ", "", full_name1)] #Get rid of excess spaces within names
		x[, full_name2:=gsub("  ", "", full_name2)] #Get rid of excess spaces within names
	}

	#b. Build String Distance Features
		#1. First Name Feature
		x<-name_stringdist(x, string1=c("first_name1"), string2=c("first_name2"))
		x<-name_stringdist(x, string1=c("last_name1"), string2=c("last_name2"))
		x<-name_stringdist(x, string1=c("middle_name1"), string2=c("middle_name2"))
		x<-name_stringdist(x, string1=c("full_name1"), string2=c("full_name2"))
		
		x<-name_stringdist(x, string1=c("first_name1"), string2=c("last_name2"))
		x<-name_stringdist(x, string1=c("first_name2"), string2=c("last_name1"))
			
		x<-name_stringdist_namepiece(x, string1=c("first_name1"), string2=c("first_name2"), group_name=c("f1_f2"))
		x<-name_stringdist_namepiece(x, string1=c("last_name1"), string2=c("last_name2"), group_name=c("l1_l2"))
		x<-name_stringdist_namepiece(x, string1=c("middle_name1"), string2=c("middle_name2"), group_name=c("m1_m2"))
		x<-name_stringdist_namepiece(x, string1=c("full_name1"), string2=c("full_name2"), group_name=c("fn1_fn2"))

		x<-name_stringdist_namepiece(x, string1=c("first_name1"), string2=c("last_name2"), group_name=c("f1_l2"))
		x<-name_stringdist_namepiece(x, string1=c("first_name2"), string2=c("last_name1"), group_name=c("f2_l1"))
		
		#2. Replace NA with zero
		x<-as.data.frame(x)
		x[is.na(x)]<-0
		x<-as.data.table(x)
	
#4. Build Prediction 
	#1. Start h20
		h2o.init(nthreads=nthreads, min_mem_size=min_mem_size)	
	#2. Load all models 
	
		load("C:/h2o/namematch/model_paths.h2o")
		
	#3. Load Specific Model 
		if (model=="NN") {
				mod<-h2o.loadModel(nn1_best_save) 
		} else if (model=="GBM") {
				mod<-h2o.loadModel(gbm1_best_save) 
		} else if (model=="DRF") {
				mod<-h2o.loadModel(gbm1_best_save) 
		} else if (model=="RF") {
				mod<-h2o.loadModel(gbm1_best_save) 
		} else if (model=="GLM") {
				mod<-h2o.loadModel(gbm1_best_save) 
		} else if (model=="ENS") {
				mod<-h2o.loadModel(gbm1_best_save) 
		} else {
			stop('Error.  Must select from the following: "NN," "GBM," "DRF," "RF," "GLM," or "ENS"')
		}		
		
	#4. Predict on data
	x<-as.h2o(x)
	pred<-as.data.table(h2o.predict(mod, newdata=x))
	pred<-as.data.table(pred)
	pred[, predict:=NULL]
	pred[, No.Match:=NULL]
	pred[, prob:=Match][, Match:=NULL]
	
	x<-as.data.table(x)
	x<-x[, 1:maxcol]
	
	x<-cbind(x, pred)
	x[, decision:="No Match"]
	x[prob>match_threshold, decision:="Match"]
	return(x)
}

#String Dist Name Piece

	#Helper Functions

	#Detects Longest String
	detect_long <- function(w1, w2){
	  nchar(w1)>3 & nchar(w2)>3 & (str_detect(w1,w2) | str_detect(w2,w1))
	}

	#Determines if first character matches
	f_char_match <- function(w1, w2){
	  str_sub(w1, 1, 1) == str_sub(w2, 1, 1)
	}

	#Create string distane metric function
	sd_norm <- function(w1, w2){
	  stringdist(w1,w2)/pmax(nchar(w1),nchar(w2))
	}

	#Determine if first initial matches
	f_initial_match <- function(w1, w2){
	  min_len <- pmin(nchar(w1), nchar(w2))
	  min_len<3 & (str_sub(w1,1,min_len) == str_sub(w2,1,min_len))
	}

	name_stringdist_namepiece <- function(dta, string1=c(), string2=c(), group_name=c()){
		require(stringdist)
		require(tidyverse)

		combo<-as.data.table(dta)
		combo[, compare_id:=1,]
		combo[, compare_id:=cumsum(compare_id), ]
	  
	#A. Create Temp Columns  
		combo[, col1:=get(string1)]
		combo[, col2:=get(string2)]
		
		combo[, col1:=gsub("[&_.-]", " ", col1)]
		combo[, col2:=gsub("[&_.-]", " ", col2)]
		
		for (i in 1:10) {
			combo[, col1:=gsub("  ", " ", col1)]
			combo[, col2:=gsub("  ", " ", col2)]
		}
		
		combo<-as.data.frame(combo)
	  
	#B. Count Words in Each Name-Unit (Only counts at the space)
		combo <- combo %>%
		mutate(name_count1 = ifelse(is.na(col1),0,str_count(col1," ") + 1),
			   name_count2 = ifelse(is.na(col2),0,str_count(col2," ") + 1))

	#C. Identify Max Words for Test to Columns
		max_words <- max(max(c(combo$name_count1),max(combo$name_count2)))

		v1 <- paste0("v1_",1:max_words)
		v2 <- paste0("v2_",1:max_words)

	#D. Make Combo long (One row per name unit)
		mash_long <- combo %>% 
		  select(compare_id, col1, col2) %>%      
		  separate(
			col1, into = v1,sep = " ", fill = "right"
		  ) %>%
		  separate(
			col2, into = v2,sep = " ", fill = "right"
		  ) %>%
		  gather(key, val, -compare_id) %>%
		  filter(!is.na(val),
				 val != "") %>%
		  mutate(key = str_sub(key,1,2))

	#E. Expand Mash Object (One Row Per Name Unit Comparison)
		mash_expand <- inner_join(
		  mash_long %>% filter(key=="v1") %>% select(-key),
		  mash_long %>% filter(key=="v2") %>% select(-key),
		  by = "compare_id",
		  suffix = c("_v1","_v2")
		) %>%
		  arrange(compare_id)


	#F. Apply Metric Functions and Match Criteria
		mash_expand <- mash_expand %>%
		  mutate(
			strdist = sd_norm(val_v1, val_v2),
			detect = detect_long(val_v1, val_v2),
			f_initial_match = f_initial_match(val_v1, val_v2),
			exact_match = ifelse(val_v1 == val_v2,1,0)
			)
			
		mash_expand_counts <- mash_expand %>%
		  #1. Only take the best case (a name unit can't match two name units)
		  group_by(compare_id, val_v1) %>%
		  filter(strdist == min(strdist)) %>%
		  ungroup() %>%
		  group_by(compare_id, val_v2) %>%
		  filter(strdist == min(strdist)) %>%
		  ungroup() %>%
		  #2. If there is a tie for #1 best case, just count the first one (a name unit can't match two name units)
		  group_by(compare_id, val_v1) %>%
		  filter(row_number() == 1) %>%
		  ungroup() %>%
		  group_by(compare_id, val_v2) %>%
		  filter(row_number() == 1) %>%
		  ungroup() %>%
		  #3. Summarize amount of matches (by type) per compare_id
		  group_by(compare_id) %>%
		  summarize(f_initial_match = sum(f_initial_match),
					strdist = mean(strdist, na.rm=TRUE),
					detect = sum(detect),
					exact_match = sum(exact_match)
					) %>%
		  ungroup()		

			
	#G. Add Match Stats Onto Combo

	#1. Join back 
	combo <- combo %>%
		left_join(mash_expand_counts,
				 by = "compare_id") 

	combo[,c(paste0(string1,"_name_count"), paste0(string2, "_name_count"))] <- NULL

	#2. Drop and Name Columns, Set NAs to Zero		 
	combo <-combo %>%
		mutate(
				#Adjust algorithm to only account for names with multiple values
				 strdist=ifelse(name_count1>1 | name_count2>1, 1-(strdist), NA)
				,detect=ifelse(name_count1>1 | name_count2>1, detect, NA)
				,f_initial_match=ifelse(name_count1>1 | name_count2>1, f_initial_match, NA)
				,exact_match=ifelse(name_count1>1 | name_count2>1, exact_match, NA)
			)
			
	combo<-as.data.table(combo)	
	for (i in c("name_count1", "name_count2", "f_initial_match", "strdist", "detect", "exact_match") ) {
		var_name<-paste(group_name, i, sep="_")
		combo[, (var_name):=get(i)]
		combo[, (i):=NULL]
	}
	combo[, c("compare_id", "col1", "col2"):=NULL]
								 
	#H. Output Data Frame
	return(as.data.table(combo))
	}



	#String Distance
	name_stringdist<-function(dta
								, string1=c(), string2=c()
								, sd_options=c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")) {

		require(stringdist)
		require(tidyverse)
		require(data.table)
		require(dtplyr)
		require(stringr)
		require(purrr)  
		require(sqldf)
								
		x<-as.data.table(dta)
		x[, wrk:=1]

		#Length of the longest string (used to transform string distance into a matching percentage)
		x[, slen_long:=pmax(str_length(get(string1)), str_length(get(string2)))]

		#Loop through each string distance method
		for(j in sd_options) { 
		
			#String Distance calculation	
			if (j %in% c("dl", "lcs", "lv", "osa", "qgram")) {  
			 
				x[, sdist:= sd_f(get(string1), get(string2), method=j) ]
			 
				#Percentage
				x[, spct:= 1- (sdist/slen_long) ]

				#Final percentage
				var_t <- paste(string1, string2, j, "pct", sep="_")
				x[, (var_t):=spct]
			  
			}
			if (j %in% c("jaccard", "jw", "cosine")) {
				x[, sdist:= sd_f(get(string1), get(string2), method=j) ]
			
				var_t <- paste(string1, string2, j, "pct", sep="_")
				x[, (var_t):=1-sdist]		
			}
			if (j %in% c("soundex") ) {
				
				x[, sdist:=sqldf(paste("select difference (", string1,",", string2, ") from x", sep=""))]
				x[sdist==0, sdist_cat:="No Match"]
				x[sdist==1, sdist_cat:="Weak Match"]
				x[sdist==2, sdist_cat:="Weak Similar Match"]
				x[sdist==3, sdist_cat:="Similar Match"]
				x[sdist==4, sdist_cat:="Strong Match"]
										
				var_t <- paste(string1, string2, j, "cat", sep="_")
				x[, (var_t):=sdist_cat]
				
			}
			
		}
		x[, c("slen_long", "sdist", "spct", "wrk", "sdist_cat"):=NULL]
		return(x)
	}

	#Sub-function that computes actual string distances based on method typoes
	sd_f  <- function(str1, str2, method) {
	  s <- unlist(map2(str1,str2, method=method, stringdist))
	  return(s)
	}