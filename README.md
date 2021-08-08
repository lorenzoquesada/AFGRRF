# AFGRRF CODE

Area and Feature Guided Regularized Random Forest (AFGRRF)  is a R toolbox to select the best features that miminize the area while maximizing the overall accuracy.

Code associate to the manuscript "AREA AND FEATURE GUIDED REGULARISED RANDOM FOREST: A NOVEL METHOD FOR PREDICTIVE MODELLING OF BINARY PHENOMENA"

**Script first purpose**: New machine learning feature selection method applied to the mapping of binary geographic  phenomenon (occurrence vs absence).                         
**Script second purpose**:Selecting a feature subset that optimise both the overall accuracy and the potentially occurrence area, using the Success Rate                           
**Script filename**: AFGRRF.R
**Version Date**: August, 2021

**I N S T R U C T I O N S**

This script consists of several parts and sub-parts.

**Part 0 **   Load the neccesaries r packages. 

**Part 1**    is the setting section, in which the User should replace the marked values (e.g. directory paths)

**Part 1.1**  defines environmental variables, such as raw data. The user should modify these values.

**Part 1.2**  Inclusion of gamma and lambda values

**Part 1.3**  Arrays placeholder to keep feature selection results from the different combinations of gamma and lambda values

**Part 2**    Obtain the Random Forest embedded importance

**Part 3**    Guide Grid search regularisation (Initialization loop). 

**a)** Initialize an empty subset of selected features and a threshold gain (G^*=0)

**b)** Fix the values of λ and γ to calculate α.

**c)** 	Computation of G_GRRF

**d)** If G_GRRF (Xj,ν)> G^*  the feature j is selected, and the threshold gain is updated to the GRRF gain. Otherwise, the feature is not selected.

**Part 3.1**  Guide Grid search regularisation

**Part 3.2**  Models generation since features selected. Multiple soft RF models are built from the different feature subsets.

**Part 3.3**  Success Rate calculation. 

**a.**	Every soft map is reclassified into multiple binary hard maps considering different percentages of affected area (pixel quantiles).

**b.**	True Posite Ratios is computed for all binary maps at increasing areal percentages for every feature subset.

**Part 4**    Summary tables. Model selection based on a trade-off between True Posite Ratios and minimal area from Succes Rate.


**Note:     When we refer to occurrence areas it corresponds to affected areas in the study case.

