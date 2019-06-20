#####################################################################################################
#Engagement		-	Machine Learning Name Matching													#
#FileName		-								  													#
#By				- 	Jeremy Guinta 																	#
#																	  								#
#Last Update Date:	5/31/2019									  									#
#																	  								#
#Purpose:		-	Modeling 																		#											
#				- 																					#	
#																									#
#####################################################################################################



#I. Setup -------------------------------------------------------------------------------------------

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	setwd("C:/h2o/")

	#Package Install
	require(tidyverse)		#All things tidy 
	require(data.table)		#Data table is better
	require(dtplyr)			#Make sure Data table and dplyr work together
	require(ggplot2)		#Graphing Utilities
	require(stringr)		#String Functions
	require(reshape2)		#Data Reshape
	require(h2o)			#Auto ML
	require(openxlsx)		#

	#Set Options
	options(scipen=20)

	
	#Graphics
	out_theme <- theme_bw() + 
		  theme(panel.grid.major=element_line(color="white"), 
				text=element_text(family="ArialMT"), 
				legend.position="bottom",
				plot.title = element_text(size = rel(1.0)),
				axis.text.x = element_text(size= rel(1.0)),
				axis.text.y = element_text(size= rel(1.0)))
				
	color_scheme <- c("#6495ED", "#C90E17", "#001933", "#691b14", "#08519c", "#778899", "#B0C4DE", 
							  "#999999", "#000000",  "#800000", "#B23232")   	

#II.  Data Loading ---------------------------------------------------------------------------------

#A. All data
dta<-readRDS("./003_ml_data.rds")

#III. Data Processing ---------------------------------------------------------------------------

#A. Normalize the parameters
lst<-names(dta)
lst<-lst[grepl("category|phone_number|\\<first_name1\\>|\\<first_name2\\>|middle_name1\\>|middle_name2\\>|\\<last_name1\\>|\\<last_name2\\>|\\<full_name1\\>|\\<full_name2\\>|first_name1_first_name2_soundex_cat|last_name1_last_name2_soundex_cat|middle_name1_middle_name2_soundex_cat|full_name1_full_name2_soundex_cat|first_name1_last_name2_soundex_cat|first_name2_last_name1_soundex_cat", lst)==FALSE]
for (i in c(lst) ) {
	print(i)
	v<-paste(i, "norm", sep="_")
	dta[, std:=sd(get(i), na.rm=TRUE)]
	dta[, avg:=mean(get(i), na.rm=TRUE)]
	dta[, c(v):=(get(i)-avg)/std]
}
dta[, c("avg", "std"):=NULL]
dta<-as.data.frame(dta)
dta[is.na(dta)]<-0
dta<-as.data.table(dta)

#B. Normalize Match to No Match Samples - Resample match population until distribution of match / no match is approx. 50%
dta_nm<-dta[category=="No Match"]
dta_m<-dta[category=="Match"]

i<-0.0
j<-1
while (i < 0.45) {
	set.seed(j)
	tmp<-sample_n(dta_m, 50)
if (j==1) {
	out<-tmp
}
else {
	out<-rbind(out,dta_m)
}
	rm(tmp)
	i<- nrow(out)/(nrow(dta_nm)*2)
	print(i)
	j<-j+1
}

dta<-rbind(dta_nm, out)
rm(out)

dta[, .N, category]
  # category    N
# 1: No Match 1805
# 2:    Match 1738

dta[, ord:=1]
dta[, ord:=cumsum(ord)]
		
#IV. Data Analysis ------------------------------------------------------------------------------

#B. Train / Test Sets
	#1. Determine size of train / testing set
	r<-nrow(dta)
	r1<-round(r*0.70,0)

	#2. Draw random sample from data for the training set
	set.seed(1)
	trn<-sample_n(dta, r1)
	trn<-as.data.table(trn)
	
	#3. Remaining observations are for the testing set
	trn[, m:="trn"]
	dta[, m:="dta"]
	tst<-merge(trn[, .(ord, m)], dta, by="ord", all=TRUE)
	tst[, .N, by=list(m.x, m.y)]

	tst<-tst[is.na(m.x)==TRUE & m.y=="dta", ][, c("m.x", "m.y"):=NULL]
	trn[, m:=NULL]
	tst[, m:=NULL]
	
#C. Set h2o for modeling
	
	#1. Prepare the data for h2o
	setwd("C:/h2o/")  	#The network pathways are too long.  Setting directory to local C:/h2o
						#All h2o objects will be saved here	

	write.csv(file="./trn.csv", trn)
	write.csv(file="./tst.csv", tst)
	
	setwd("C:/h2o/")  	#The network pathways are too long.  Setting directory to local C:/h2o
						#All h2o objects will be saved here	
	
	h2o.init(nthreads=6, min_mem_size="24G")	

	#2. Load into h2o
	trn<-h2o.importFile("./trn.csv")
	tst<-h2o.importFile("./tst.csv")
	
	#3. Set parameters
	
	#Generalize Linear Models (Binomial) - GLM
	hyper_params_glm <- list(
		  alpha = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
		)

	#Random Forest
	hyper_params_rf <- list(
	  ntrees = c(5, 10, 25,50,75,100), 
	  max_depth = c(10,15,20), 
	  min_rows = c(5,10,30,70,100), 
	  sample_rate = c(.95, .975,.99,.995,1),
	  col_sample_rate_per_tree = c(.5, .6,.7,.8,.9,1),
	  nbins=c(2,5,10,15,20),
	  mtries=c(-1,5,10,15,20,25,30,35),
	  nbins_cats = c(64, 128, 256, 512,1024,1536)
	)	

	#GBM
	hyper_params_gbm <- list(
	  ntrees = c(5, 10, 25, 50,100,150,200), 
	  max_depth = c(5, 10,15,20), 
	  min_rows = c(2, 5,10,15,20), 
	  learn_rate = c(.06,.07,.08,.09,.1,.11,.12),
	  sample_rate = c(.95, .975,.99,.995,1),
	  col_sample_rate = c(.3,.4,.5,.6,.7),
	  col_sample_rate_per_tree = c(.6,.7,.8,.9,1),
	  nbins_cats = c(32,64,128,256),
	  learn_rate_annealing=c(0.25,0.5,0.75, 1)	
	)	
	
	#DRF
	hyper_params_drf <- list(
	  ntrees = c(5, 10, 25,50,75,100), 
	  max_depth = c(10,15,20), 
	  min_rows = c(5,10,30,70,100), 
	  sample_rate = c(.95, .975,.99,.995,1),
	  col_sample_rate_per_tree = c(.5, .6,.7,.8,.9,1),
	  nbins=c(2,5,10,15,20),
	  mtries=c(-1,5,10,15,20,25,30,35),
	  nbins_cats = c(64, 128, 256, 512,1024,1536)
	)	

	#NN
	hyper_params_nn <- list(
	  epochs=20,
	  overwrite_with_best_model=FALSE,
	  hidden=list(c(32,32,32),c(64,64), c(128,128,128)),
	  max_w2=10,
	  score_duty_cycle=0.025,
	  activation=c("Rectifier","Tanh","TanhWithDropout"),
	  input_dropout_ratio=c(0,0.05),
	  score_validation_samples=10000,
	  l1=c(.00001,.000001,.0000001),
	  l2=c(.00001,.000001,.0000001),
	  rho = c(.99,.975,1,0.95),
	  rate=c(.005,.0005,.00005),
	  rate_annealing=c(.00000001,.0000001,.000001),
	  momentum_start=c(.5,.1,.01,.05,.005),
	  momentum_stable=c(0.1, 0.2, 0.3, 0.4,0.5), 
	  momentum_ramp=c(1000000,100000)
	)	
	
	#Search Criteria
	search_criteria <- list(
	  strategy = "RandomDiscrete",
	  max_runtime_secs = 30*60,  #30 minutes per run
	  max_models = 500
	)	
	
	#X variables
	xnames<-names(trn)
	xnames<-xnames[grepl("C1|ord|category|phone_number|\\<first_name1\\>|\\<first_name2\\>|middle_name1|middle_name2|\\<last_name1\\>|last_name2\\>|\\<full_name1\\>|\\<full_name2\\>", xnames)==FALSE]

#D. Conduct Models	
	#1. Random Forest
		#a. Model
		rf1 <- h2o.grid(algorithm = "randomForest", 
							x = xnames, y = "category", 
							training_frame = trn, 
							hyper_params = hyper_params_rf,
							search_criteria = search_criteria,
							stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
							stopping_rounds = 3,
							seed = -1,
							nfolds = 5, fold_assignment = "Modulo", 
							distribution = "binomial",
							keep_cross_validation_predictions = TRUE
			)
			
		rf1_sort <- h2o.getGrid(grid_id = rf1@grid_id, sort_by = "err", decreasing = FALSE)
		rf1_sort
			
		rf1_best <- h2o.getModel(rf1_sort@model_ids[[1]])
		summary(rf1_best)	
		
		#b. Prediction
		pred_rf1<-h2o.predict(rf1_best, newdata = tst)
		pref_rf1<-h2o.performance(rf1_best, newdata=tst)	

	#2. Gradient Boosted Machine
		#a. Model
		gbm1 <- h2o.grid(algorithm = "gbm", 
						x = xnames, y = "category", 
						training_frame = trn,
						hyper_params = hyper_params_gbm,
						search_criteria = search_criteria,
						stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
						stopping_rounds = 3,
						seed = -1,
						nfolds = 5, fold_assignment = "Modulo",
						distribution = "binomial",
						keep_cross_validation_predictions = TRUE
		)
		
		gbm1_sort <- h2o.getGrid(grid_id = gbm1@grid_id, sort_by = "err", decreasing = FALSE)
		gbm1_sort
		
		gbm1_best <- h2o.getModel(gbm1_sort@model_ids[[1]])
		summary(gbm1_best)	
	
		#b. Prediction
		pred_gbm1 <- h2o.predict(gbm1_best, newdata = tst)
		pref_gbm1<-h2o.performance(gbm1_best, newdata=tst)

	#3. Distributed Random Forest
		#a. Model
		drf1 <- h2o.grid(algorithm = "drf", 
							x = xnames, y = "category", 
							training_frame = trn, 
							hyper_params = hyper_params_drf,
							search_criteria = search_criteria,
							stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
							stopping_rounds = 3,
							seed = -1,
							nfolds = 5, fold_assignment = "Modulo", 
							distribution = "binomial",
							keep_cross_validation_predictions = TRUE
			)
			
		drf1_sort <- h2o.getGrid(grid_id = drf1@grid_id, sort_by = "err", decreasing = FALSE)
		drf1_sort
			
		drf1_best <- h2o.getModel(drf1_sort@model_ids[[1]])
		summary(drf1_best)	
		
		#b. Prediction
		pred_drf1<-h2o.predict(drf1_best, newdata = tst)
		pref_drf1<-h2o.performance(drf1_best, newdata=tst)		
		
	#4. Neural Net	
		#a. Model
		nn1 <- h2o.grid(algorithm = "deeplearning", 
							x = xnames, y = "category", 
							training_frame = trn, 
							hyper_params = hyper_params_nn,
							search_criteria = search_criteria,
							stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
							stopping_rounds = 3,
							seed = -1,
							nfolds = 5, fold_assignment = "Modulo", 
							distribution = "binomial",
							keep_cross_validation_predictions = TRUE
			)
			
		nn1_sort <- h2o.getGrid(grid_id = nn1@grid_id, sort_by = "err", decreasing = FALSE)
		nn1_sort
			
		nn1_best <- h2o.getModel(nn1_sort@model_ids[[1]])
		summary(nn1_best)	
		
		#b. Prediction
		pred_nn1<-h2o.predict(nn1_best, newdata = tst)
		pref_nn1<-h2o.performance(nn1_best, newdata=tst)		

	#5. GLM
		#a. Model 
	    glm1 <- h2o.grid(algorithm = "glm", 
                        x = xnames, y = "category", 
						training_frame = trn,
                        hyper_params = hyper_params_glm,
                        search_criteria = search_criteria,
                        stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
                        stopping_rounds = 3,
                        seed = 1,
                        nfolds = 5, fold_assignment = "Modulo", 
                        keep_cross_validation_predictions = TRUE,
						family = "binomial",
						lambda_search=TRUE 
                        )
						
        glm1_sort <- h2o.getGrid(grid_id = glm1@grid_id, sort_by = "err", decreasing = TRUE)
        glm1_sort

        glm1_best <- h2o.getModel(glm1_sort@model_ids[[1]])
        summary(glm1_best)	

		#b. Prediction
		pred_glm1<-h2o.predict(glm1_best, newdata = tst)
		pref_glm1<-h2o.performance(glm1_best, newdata=tst)		
		
#E. Ensemble
	#1. Stack the models
	ens1<-h2o.stackedEnsemble(x = xnames, y="category",
							  training_frame = trn,
							  base_models = list(rf1_best, gbm1_best, drf1_best, nn1_best, glm1_best)
				)
					
	#2. Run prediction 
		pred_ens1 <- h2o.predict(ens1, newdata = tst)
		pref_ens1 <-h2o.performance(ens1, newdata=tst)	

#F. Ensemble 2
	#1. Stack the models
	ens2<-h2o.stackedEnsemble(x = xnames, y="category",
							  training_frame = trn,
							  base_models = list(gbm1_best, nn1_best)
				)
					
	#2. Run prediction 
		pred_ens2 <- h2o.predict(ens2, newdata = tst)
		pref_ens2 <-h2o.performance(ens2, newdata=tst)		

#G. Graphics
	#1. Ensemble 1 
	rocr<-data.frame(fpr=pref_ens1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_ens1@metrics$thresholds_and_metric_scores$tpr)
	
	#ROCR Curve
	plt<-ggplot(rocr, aes(fpr,tpr)) + geom_line()+theme_bw()
	plt<-plt+geom_abline(intercept=0, color="red")
	plt<-plt+scale_color_manual(values= c(color_scheme))
	plt<-plt+labs(colour = "Default", x="False Positive Rate", y="True Positive Rate", title="ROCR Curve - Full Name")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))	
	ens1_graph2<-plt	

	#2. Individual Models
	rocr_ens<-data.frame(fpr=pref_ens1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_ens1@metrics$thresholds_and_metric_scores$tpr)
	rocr_ens<-as.data.table(rocr_ens)
	rocr_ens[, tp:="Ensemble - Best Performance of Each Algorithm"]
	
	rocr_gbm<-data.frame(fpr=pref_gbm1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_gbm1@metrics$thresholds_and_metric_scores$tpr)
	rocr_gbm<-as.data.table(rocr_gbm)
	rocr_gbm[, tp:="GBM"]
	
	rocr_nn<-data.frame(fpr=pref_nn1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_nn1@metrics$thresholds_and_metric_scores$tpr)
	rocr_nn<-as.data.table(rocr_nn)
	rocr_nn<-rbind(data.table(fpr=0.00, tpr=0.00),rocr_nn)
	rocr_nn[, tp:="Deep Learning"]
	
	rocr_rf<-data.frame(fpr=pref_rf1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_rf1@metrics$thresholds_and_metric_scores$tpr)
	rocr_rf<-as.data.table(rocr_rf)
	rocr_rf<-rbind(data.table(fpr=0.00, tpr=0.00),rocr_rf)
	rocr_rf[, tp:="Random Forest"]
	
	rocr_drf<-data.frame(fpr=pref_drf1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_drf1@metrics$thresholds_and_metric_scores$tpr)
	rocr_drf<-as.data.table(rocr_drf)
	rocr_drf<-rbind(data.table(fpr=0.00, tpr=0.00),rocr_drf)
	rocr_drf[, tp:="Distributed Random Forest"]
		
	rocr_glm<-data.frame(fpr=pref_glm1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_glm1@metrics$thresholds_and_metric_scores$tpr)
	rocr_glm<-as.data.table(rocr_glm)
	rocr_glm[, tp:="Binomial Regression"]
	
	rocr_all<-rbind(rocr_ens, rocr_gbm, rocr_nn, rocr_rf, rocr_drf, rocr_glm)
	
	#ROCR Curve
	plt<-ggplot(rocr_all, aes(fpr,tpr, color=tp, group=tp)) + geom_line()+theme_bw()
	plt<-plt+geom_abline(intercept=0, color="red")
	plt<-plt+scale_color_manual(values=c(color_scheme))
	plt<-plt+labs(colour = "Default", x="False Positive Rate", y="True Positive Rate", title="ROCR Curve - Full Name")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))	
	ens1_graph3<-plt

	#ROCR Curve
	plt<-ggplot(rocr_all, aes(fpr,tpr)) + geom_line()+theme_bw()
	plt<-plt+facet_wrap(~tp)
	plt<-plt+geom_abline(intercept=0, color="red")
	plt<-plt+scale_color_manual(values=c(color_scheme))
	plt<-plt+labs(colour = "Default", x="False Positive Rate", y="True Positive Rate", title="ROCR Curve - Full Name")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))	
	ens1_graph4<-plt
	
#H. Summary Output
	#1. Confusion Matrices
	
	glm1_conf_matrix<-as.data.table(h2o.confusionMatrix(glm1_best, newdata=tst))
	glm1_conf_matrix[, tp:="GLM"]
	gbm1_conf_matrix<-as.data.table(h2o.confusionMatrix(gbm1_best, newdata=tst))
	gbm1_conf_matrix[, tp:="GBM"]
	rf1_conf_matrix<-as.data.table(h2o.confusionMatrix(rf1_best, newdata=tst))
	rf1_conf_matrix[, tp:="Random Forest"]
	drf1_conf_matrix<-as.data.table(h2o.confusionMatrix(drf1_best, newdata=tst))
	drf1_conf_matrix[, tp:="Distributed Random Forest"]
	nn1_conf_matrix<-as.data.table(h2o.confusionMatrix(nn1_best, newdata=tst))
	nn1_conf_matrix[, tp:="Deep Learning"]
	ens1_conf_matrix<-as.data.table(as.data.frame(h2o.confusionMatrix(ens1, newdata=tst)))
	ens1_conf_matrix[, tp:="Ensemble - Best Performance of Each Algorithm"]
	
	conf_matrix<-rbind(glm1_conf_matrix,gbm1_conf_matrix,rf1_conf_matrix,drf1_conf_matrix,nn1_conf_matrix,ens1_conf_matrix)
	conf_matrix<-conf_matrix[, .(tp, Match, `No Match`, Error, Rate)]
		
#V. Data Output
#A. Save Models
	gbm1_best_save <- h2o.saveModel(
	  object = gbm1_best,
	  path = "C:/h2o/namematch/gbm1.h2o", 
	  force =TRUE
	)
	
	rf1_best_save <- h2o.saveModel(
	  object = rf1_best,
	  path = "C:/h2o/namematch/rf1.h2o", 
	  force =TRUE
	)		
	
	drf1_best_save <- h2o.saveModel(
	  object = drf1_best,
	  path = "C:/h2o/namematch/drf1.h2o", 
	  force =TRUE
	)		

	nn1_best_save <- h2o.saveModel(
	  object = nn1_best,
	  path = "C:/h2o/namematch/nn1.h2o", 
	  force =TRUE
	)

	glm1_best_save <- h2o.saveModel(
	  object = glm1_best,
	  path = "C:/h2o/namematch/glm1.h2o", 
	  force =TRUE
	)
	
	ens1_best_save <- h2o.saveModel(
	  object = ens1,
	  path = "C:/h2o/namematch/ens1.h2o", 
	  force =TRUE
	)

	ens2_best_save <- h2o.saveModel(
	  object = ens2,
	  path = "C:/h2o/namematch/ens2.h2o", 
	  force =TRUE
	)	
		
	save(file="C:/h2o/namematch/model_paths.h2o",gbm1_best_save,rf1_best_save,drf1_best_save,nn1_best_save,ens1_best_save,ens2_best_save,glm1_best_save)
	
#B. Apply Predictions to Actual Data
	tst_pred<-cbind(as.data.table(tst), as.data.table(pred_ens2))
	tst_pred<-tst_pred[, .(phone_number, full_name1, full_name2, category, predict, Match, first_name1_first_name2_lv_pct, last_name1_last_name2_lv_pct)]
	saveRDS(file="./004_test_predictions.rds", tst_pred)
	
#C. Output Lists of Areas where Model Helped
	lst1<-tst_pred[Match<0.50 & first_name1_first_name2_lv_pct>0.50 & last_name1_last_name2_lv_pct>0.50,]
	lst2<-tst_pred[Match>0.95 & first_name1_first_name2_lv_pct<0.80 & last_name1_last_name2_lv_pct<0.80,]
	lst3<-tst_pred[Match>0.95 & first_name1_first_name2_lv_pct<0.80 & last_name1_last_name2_lv_pct>=0.90,]
		
#D. Summary Objects
	ggsave(file="./004_rocr1.png", ens1_graph3, height=8, width=11)
	ggsave(file="./004_rocr2.png", ens1_graph4, height=8, width=11)

	wb <- createWorkbook()
	addWorksheet(wb,"004_confmatrix")
	writeData(wb, "004_confmatrix", conf_matrix)
	saveWorkbook(wb, "./004_ConfusionMatrix.xlsx", overwrite=TRUE)
	
	wb <- createWorkbook()
	addWorksheet(wb,"004_examplelist1")
	addWorksheet(wb,"004_examplelist2")
	addWorksheet(wb,"004_examplelist3")
	writeData(wb, "004_examplelist1", lst1)
	writeData(wb, "004_examplelist2", lst2)
	writeData(wb, "004_examplelist3", lst3)
	saveWorkbook(wb, "./004_ExampleLists.xlsx", overwrite=TRUE)
	


