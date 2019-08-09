#Version		: 	1.0
#Author			: 	jjg / nxp
#Last Update	: 	6/6/2019
#Purpose		: 	String Distance Helper Function for mass production of various string distance algos.
#Argument(s)	:	dataset, string1, string2, string distance methods
#Input 			:	dataset 
#Output			:	dataset with string distance metrics 
#External		:	
#Example Syntax	: 	name_stringdist(dta=c("dta"), string1=c("first_name"), string2=c("first_name_alt"), sd_options=c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"))

#Notes			:   soundex option DOES NOT use stringdist soundex().  It uses the sqldf() implementation of soundex
#Packages		: 	stringdist, tidyverse, data.table, dtplyr, stringr, purrr, sqldf
 
name_stringdist<-function(dta=c()
							, string1=c(), string2=c()
							, sd_options=c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")) {

	require(stringdist)
	require(tidyverse)
	require(data.table)
	require(dtplyr)
	require(stringr)
	require(purrr)  
	require(sqldf)
							
	x<-as.data.table(get(dta))
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
			
			x[, sdist:=sqldf(paste("select difference (", string1,",", string2, ") from ", dta, sep=""))]
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
