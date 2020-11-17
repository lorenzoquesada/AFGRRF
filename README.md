# AFGRRF CODE

**Project**: Teledetección y clima: análisis de series temporales para la evaluación de la vulnerabilidad forestal al cambio climático y la estimación de cosechas en Andalucía -TelClim- (Ref: US-1262552)

* * o _ Code associate to the manuscript "AREA AND FEATURE GUIDED REGULARISED RANDOM FOREST: A NOVEL METHOD FOR PREDICTIVE MODELLING OF BINARY CLASSIFICATION"    * * o _                     
**Authors**: Lorenzo Carlos Quesada-Ruiz*, Victor Rodriguez-Galiano, Raúl Zurita-Milla, Emma Izquierdo-Verdiguier

**Script first purpose**: New machine learning feature selection method applied to the mapping of binary geographic  phenomenon (occurrence vs absence).                         
**Script second purpose**:Selecting a feature subset that optimise both the overall accuracy and the potentially occurrence area, using the Success Rate                           

**Run time**: the script may run daily  or may be used when needed 
**Authors code**: Lorenzo C. Quesada Ruiz, Víctor Rodríguez Galiano 
**Organization**: Departamento de Geografía física y Análisis Geográfico Regional de la Unviersidad de Sevilla 
**Address**: Doña María de Padilla, 40002, Seville, Spain
**Project Contact**: Víctor Rodríguez Galiano
**Email**: lquesada@.es
**Phone number**: (+34) 6303035751366


**cript filename**: AFGRRF.R
**Version Date**: November, 2020


#**I N S T R U C T I O N S**

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

