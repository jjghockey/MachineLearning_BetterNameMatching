This folder contains the R code used to develop the training and testing data for the machine learning model.  It also contains the machine learning models and output from the models.

001_mk_data.r - This code uses proprietary data (not provided) to develop the source data for the models.

002_mk_ml_data.r - This code uses the proprietary data (not provided) created in 001 to develop name comparisons that the model will compare

003_mk_mk_data_features.r - This code adds string distance features onto the data.
  name_stringdist.r - Function to automate and standardize the string distance features created
  name_stringdist_namepiece.r - Function to create a new string distance algorithm that compares name components (John Michael to Michael John) that ignores the actual order of the name components

004_ml_model.r - This code creates the training and test sets.  Sets up the hyper parameters and grid search options and conducts the machine learning model using h2o. This code also prepares the ensemble Super Learning and conducts the final performance predicting and scoring on the test set.  

  
  
