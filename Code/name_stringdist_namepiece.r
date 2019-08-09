#Version		: 	1.0
#Author			: 	jjg / nxp
#Last Update	: 	6/6/2019
#Purpose		: 	String Distance Helper Function for mass production of various string distance algos.
#Argument(s)	:	dataset, string1, string2, new group name
#Input 			:	dataset 
#Output			:	dataset with string distance metrics 
#External		:	
#Example Syntax	: 	name_stringdist_namepiece("dta", "name1", "name2" , group_name = "f1_l2")

#Notes			:   combo = data.frame(), col1 = string1, col2 = string2, group_name = new column names for matching algorithm
#Packages		: 	stringdist, tidyverse, data.table, dtplyr, stringr, purrr, sqldf


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

name_stringdist_namepiece <- function(dta=c(), string1=c(), string2=c(), group_name=c()){
	require(stringdist)
	require(tidyverse)

	combo<-as.data.table(get(dta))
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
