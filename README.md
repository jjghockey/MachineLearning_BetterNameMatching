# MachineLearning_BetterNameMatching
Machine learning along with string distance algorithms to detect and link name information from different disparate sources.

Data warehouse repositories can be built in a variety of different ways with different requirements for user input.  Users who input their information into one service's data warehouse may change or modify how they input their information into a different serivice's data warehouse.  This repositories general goal is to develop a better method of linking disparate name information using string distance algorithms and machine learning.  

The main goal will be accomplished by using string distance algorithm output as features to a machine learning model. The model will use a variety of algorithms to develop a Match / No Match decision when comparison two names. 

In a sample example, "Johnathan Smith" would parse into a first name of "Jonathan" and a last name of "Smith."  If we were comparing that name to the name "Jon Smyth" (which would parse to "Jon" and "Smyth"), a simple comparison would return a No Match.  A string distance comparision using Levenshteins distance would return of 0.33 (1-(6/9)) for the first name comparison and a string distance of 0.75 (1-(1/4)) for the last name comparison.  These particular values may or may not be considered a match, but the string distance scores are quite low.  However, using various string distance algorithms (osa, lv, dl, hamming, lcs, cosine, jaccard, jw, and soundex), we can improve on simple comparisons to develop better probability scores for name comparisons. 
