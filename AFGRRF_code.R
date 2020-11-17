
###########################################################################################################################
## Project: Teledetección y clima: análisis de series temporales para la evaluación de la vulnerabilidad forestal al     ##
#           cambio climático y la estimación de cosechas en Andalucía -TelClim- (Ref: US-1262552)
# Code associate to the manuscript "AREA AND FEATURE GUIDED REGULARISED RANDOM FOREST: A NOVEL METHOD FOR PREDICTIVE     ##    
# MODELLING OF BINARY CLASSIFICATION"                                                                                    ##  
# Authors: Lorenzo Carlos Quesada-Ruiz*, Victor Rodriguez-Galiano, Raúl Zurita-Milla, Emma Izquierdo-Verdiguier          ##
##                                                                                                                       ##
## Script first purpose: New machine learning feature selection method applied to the mapping of binary geographic       ##     
##                       phenomenon (occurrence vs absence).                                                             ##
## Script second purpose:Selecting a feature subset that optimise both the overall accuracy and the potentially          ##
##                       occurrence area, using the Success Rate                                                         ##
## Run time: the script may run daily  or may be used when needed                                                        ##
##_______________________________________________________________________________________________________________________##
## Authors: Lorenzo C. Quesada Ruiz, Víctor Rodríguez Galiano                                                            ##
## Organization: Departamento de Geografía física y Análisis Geográfico Regional de la Unviersidad de Sevilla            ##
## Address: Doña María de Padilla, 40002, Seville, Spain                                                                 ##
## Project Contact: Víctor Rodríguez Galiano                                                                             ##
## Email: lquesada@.es                                                                                                   ##
## Phone number: (+34) 6303035751366                                                                                     ##
##_______________________________________________________________________________________________________________________##
## Script filename: AFGRRF.R                                                                                             ##
## Version Date: November, 2020                                                                                          ##
###########################################################################################################################
# > > > > > > > > > > > > > >           I N S T R U C T I O N S           < < < < < < < < < < < < < < < < < < < < < < < < #
#
# This script consists of several parts and sub-parts.
#
# Part 0    Load the neccesaries r packages. 
# Part 1    is the setting section, in which the User should replace the marked values (e.g. directory paths)
# Part 1.1  defines environmental variables, such as raw data. The user should modify these values.
# Part 1.2  Inclusion of gamma and lambda values
# Part 1.3  Arrays placeholder to keep feature selection results from the different combinations of gamma and lambda values
# Part 2    Obtain the Random Forest embedded importance
# Part 3    Guide Grid search regularisation (Initialization loop). 	
#            a) Initialize an empty subset of selected features and a threshold gain (G^*=0)
#            b) Fix the values of λ and γ to calculate α.
#            c) 	Computation of G_GRRF
#            d) If G_GRRF (Xj,ν)> G^*  the feature j is selected, and the threshold gain is updated to the GRRF gain. 
#               Otherwise, the feature is not selected.
# Part 3.1  Guide Grid search regularisation
# Part 3.2  Models generation since features selected. Multiple soft RF models are built from the different feature subsets.
# Part 3.3  Success Rate calculation. 
#            a.	Every soft map is reclassified into multiple binary hard maps considering different 
#               percentages of affected area (pixel quantiles).
#            b.	True Posite Ratios is computed for all binary maps at increasing areal 
#               percentages for every feature subset.
# Part 4    Summary tables. Model selection based on a trade-off between True Posite Ratios and minimal area from Succes Rate.

# Note:     When we refer to occurrence areas it corresponds to affected areas in the study case.


# > > > > > > > > > > > > > > > > > > > > > > > > > > > < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < #
##                                          # PART 0 #
## ______________________________________________________________________________________________________________________##
##                                  Load the necessaries packages
## ______________________________________________________________________________________________________________________##

library(raster)
library(rgdal)
library(pROC)
library(RRF) 
##                                        # END PART 0 #
###########################################################################################################################


###########################################################################################################################

##                                          # PART 1.1 #
## ______________________________________________________________________________________________________________________##
##                                  Setting environmental features
## ______________________________________________________________________________________________________________________##

## USE: set the following paths of origin  data, replacing values with yours
# WARNING: Proper setting up of the following paths and values is crucial. Please, replace the strings correctly
#

# ----------- DATA PATH ----------------------------------------------------------------------# REPLACE THE FOLLOWING PATHS

# Set the working directory 
setwd("C:/R/")

getwd()

# Read the database (existing file in the working directory)
database <- read.table("database.txt", header = TRUE)

# Define the relative path in the working directory where the rasters are
path_layer_stack <- "independant_features_layer_stack.tif" 

# ----------- DATA SPLIT ----------------------------------------------------------------------# REPLACE THE FOLLOWING PATHS

# Three subsets were generated from the initial database to train and assess the method's performance: 
#training (60%), test 1 (20%) and test 2 (20%). 
# Test 1 was used as an internal validation, and test 2 to compare AGRRF with other RF based methods.
set.seed(1)
n <- sample(3, nrow(database), replace = TRUE, prob = c(0.6, 0.2, 0.2))

# Data training for classification.
data.train <- database[n == 1, ] 

# Data test 1 for classification.
data.test.clasif <- database[n == 2, ]

# Data test 2 for comparing with other methods. It is a supplementary dataset but it is not needed for AFGRRF.
data.test.coor.modelos <- database[n == 3, ]  

# ----------- TRAINING DATA ARRANGEMENTS ---------------------------------------------------------------------------------# 

# Remove the dependent feature from the training database.
data.train.sv <- subset(data.train, select = -V) 

# Remove coordinates from the training database, corresponding to columns X and Y.
data.train <- subset(data.train, select = c(-X, -Y))

# ----------- TEST DATA ARRANGEMENTS -------------------------------------------------------------------------------------#

# Remove coordinates from the TEST 1 database, corresponding to columns X and Y. It is used for an internal validation 
# in the GRRF computation.
data.test <- subset(data.test.clasif, select = c(-X, -Y)) 


# ----------- TEST DATA ARRANGEMENTS FOR SUCCESS RATE --------------------------------------------------------------------#

# Remove the dependent variable from the TEST 1 database.
data.test.clasif.V <- subset(data.test.clasif, select = V)

# Extract coordinates from the TEST 1 database. It is used to extrapolate the values of the different exploration areas.
data.test.clasif.coor <- subset(data.test.clasif, select = c(X, Y))


## WARNING: data.test.clasif.V and data.test.clasif.coor are joined after for calculating success rate.


# ----------------------------------- LOAD LAYER STACK -------------------------------------------------------------------#

img <- brick(path_layer_stack)

# Naming the layers of the image corresponding to the independent features.
names(img) <- c('V', 'd_via5', 'd_via2', 'd_ruid','d_rui5', 'd_rui2', 'd_ind', 'd_inv5', 'd_inv2', 'd_eled', 'd_ele5',
                'd_ele2', 'd_edid', 'd_edi5', 'd_edi2', 'd_ccur', 'd_ccar', 'd_cc90', 'd_cc12', 'd_cc06', 'd_cc00',
                'd_1cur', 'd_1car', 'd_1c90', 'd_1c12', 'd_1c06', 'd_1c00', 'e_trai', 'e_teli', 'e_segi', 'e_saeq',
                'e_reeq', 'e_rceq', 'e_pozo', 'e_monu', 'E_INAR', 'e_hico', 'e_eneq', 'e_emba', 'e_edeq', 'e_depi',
                'e_cueq', 'e_come', 'e_char', 'e_cein', 'e_cant', 'e_bals', 'e_altu', 'e_admi', 'e_zove', 'e_zho7',
                'e_zho5', 'e_zho3', 'e_zh35', 'e_zh15', 'e_vial', 'e_veil', 'e_suag', 'e_plim', 'e_nurb', 'e_miar',
                'e_espr', 'e_elei', 'e_cost', 'e_carr', 'e_barr', 'e_auto', 'S_VSOC', 'S_VPOC', 'S_VIVI', 'S_VIDE',
                'S_PRBA', 'S_HOTE', 'S_COVA', 'S_CARU', 'S_BAPH', 'S_APAR', 'V_VERT', 'V_TOTA', 'V_SEIN', 'V_MONU',
                'V_LICO', 'V_ELIN', 'V_EDIF', 'V_CARR', 'V_CAMI', 'V_ALTU', 'M_TIPC', 'M_TGPC', 'M_TAPC', 'M_ITPC',
                'M_IPPC', 'M_IMPC', 'M_IDPC', 'M_GSPC', 'M_GPPC', 'M_BLMU', 'M_BIMU', 'M_BAMU', 'F_SAVI', 'F_SOCI',
                'f_pegr', 'f_micu', 'f_mdtg', 'f_macu', 'F_ZLIC', 'F_ZARI', 'F_NDVI', 'F_ESNA', 'F_ARSE', 'H_IBIR',
                'H_IBIU', 'C_TIPO_Cat', 'X1i_turi', 'X1i_indu', 'X1i_etme', 'X1i_come', 'X1i_coma', 'X1i_acec',
                'X1h_rpcd', 'X1h_rpca', 'X1h_dppe', 'X1h_dppa','X1s_prba', 'X1s_cova', 'X1s_baph', 'C_TIPO_Cat') 

##                                        # END PART 1.1 #
###########################################################################################################################


###########################################################################################################################

##                                          # PART 1.2 #
## ______________________________________________________________________________________________________________________##
##                                  Inclusion of gamma and lambda values
## ______________________________________________________________________________________________________________________##

## WARNING: The number of gamma and lambda combination define the number of possible models.

#  --------------------------------- Set gamma and lambda combinations ---------------------------------------------------- 

gamma <- seq(0.1, 1, 0.1)

lambda <- seq(0.1, 1, 0.1)

##                                        # END PART 1.2 #
###########################################################################################################################


###########################################################################################################################

##                                          # PART 1.3 #
## ______________________________________________________________________________________________________________________##
##                            MATRIX AND ARRAY PLACEHOLDER TO STORE FEATURE SELECTION RESULTS 
## ______________________________________________________________________________________________________________________##

# Matrix to store overall acurracy results.
overall_accuracy <- matrix(0, 10, 10) 

# Matrix to store the number of features selected for each combination of gamma and lambda values.
nfeatures <- matrix(0, 10, 10) 

# Matrix to store the minimum area obtained by each combination of gamma and lambda values for a success rate ratio of 90%.
area_sr_data90 <- matrix(0, 10,10) 

# Matrix to store the minimum area obtained by each combination of gamma and lambda values for a success rate ratio of 85%.
area_sr_data85 <- matrix(0, 10,10)

# Matrix to store the minimum area obtained by each combination of gamma and lambda values for a success rate ratio of 80%.
area_sr_data80 <- matrix(0, 10,10)

# Array to store list that contain the names of the features selected for each combination of gamma and lambda values.
features_selected <- as.array(matrix(list(), 10, 10))


## -------------- Matrix to save alternatives measures obtained by each combination of gamma and lambda values -----------

#ROC obtained by each combination of gamma and lambda values.
matrix_ROC <- matrix(0, 10, 10)

#Maximum true positive rate obtained by each combination of gamma and lambda values.
maxtpr <- matrix(0, 10, 10)

#Maximum true negative rate obtained by each combination of gamma and lambda values.
maxtnr <- matrix(0, 10, 10)

#Maximum false positive rate obtained by each combination of gamma and lambda values.
maxfpr <- matrix(0, 10, 10)

#Maximum false negative rate obtained by each combination of gamma and lambda values.
maxfnr <- matrix(0, 10, 10)

#Minimum true positive rate obtained by each combination of gamma and lambda values.
mintpr <- matrix(0, 10, 10)

#Minimum true negative rate obtained by each combination of gamma and lambda values.
mintnr <- matrix(0, 10, 10)

#Minimum false positive rate obtained by each combination of gamma and lambda values.
minfpr <- matrix(0, 10, 10)

#Minimum false negative rate obtained by each combination of gamma and lambda values.
minfnr <- matrix(0, 10, 10)



##                                        # END PART 1.3 #
###########################################################################################################################


###########################################################################################################################

##                                          # PART 2 #
## ______________________________________________________________________________________________________________________##
##                                  Random Forest Embedded importance
## ______________________________________________________________________________________________________________________##

## WARNING: This a first regularisation, similar to GRRF algorithm, which equally affects to all features.

# Seed settlement
set.seed(77)

# -----------------------------  Ordinary random forest for normalising the feature importance score ---------------------- 

rf <- RRF(data.train.sv, data.train$V, flagReg = 0)

# List that stores the feature importance score by the mean decrease Gini.
impRF <- rf$importance
impRF <- impRF[,"MeanDecreaseGini"]

# Normalisation of the feature importance score.
imp <- impRF / (max(impRF))


##                                        # END PART 2 #
###########################################################################################################################


###########################################################################################################################
##                                          # PART 3 #
## ______________________________________________________________________________________________________________________##
##                                 Guided Grid search regularisation
## ______________________________________________________________________________________________________________________##

## USE:After the first regularisation,AFGRRF carries out an exhaustive grid search identically to the GRRF method, 
#      generating multiple models based on different feature subsets for different combinations of the gamma and lambda 
#      values. 
## WARNING: - The initial loop goes over the initial gamma and lambda values, training multiple soft classification models 
#             with RF using the different feature subsets generated from the Guided Grid search regularisation.
#           - The loop allows to extract the success ratios of each model, overall accuracy and the selected features.
##  
#  ---------------------------------------------------- Main loop  -------------------------------------------------------- 
for (i in 1:length(gamma)) {
    for (j in 1:length(lambda)) {
##                                          # PART 3.1 #
## ______________________________________________________________________________________________________________________##
##                                 Guided Grid search regularisation
## ______________________________________________________________________________________________________________________##
     
#  ------------------------------------------------- Computation of GRRF --------------------------------------------------
      
        # Weighted average used in GRRF function.
        coefReg <- (1 - gamma[i]) * lambda[j] + gamma[i] * imp 
        
        # Compute the GRRF by using the previously defined regularisation coefficient.
        set.seed(117)
        grrf <- RRF(data.train.sv,data.train$V, coefReg=coefReg, ntree=500, mtry=11, stepfactor=1, flagReg=1, improve=0.01,
                    do.trace=TRUE, plot=TRUE, doBest=TRUE)

        # Storing list with the names of the selected features.
        features_selected[i,j] <- list(names(data.train.sv[grrf$feaSet]))
        
        # Order of importance of the selected features.
        sort(importance(grrf))
        
        # Visualization of the importance of the selected features.
        varImpPlot(grrf, n.var=min(length(grrf$feaSet)))  
        
        # Generation of new data train that contain only the selected features for each combination of lambda and gamma values.
        CV.data.train <- subset(data.train.sv, select=c(grrf$feaSet))
        V=data.train$V
        Table <- cbind(V, CV.data.train)
        genericname <- "Data"
        tempname <- paste(genericname, "-", as.character(i), as.character(j), ".txt")
        write.table(Table,file=tempname) 
        
##                                        # END PART 3.2 #
###########################################################################################################################
        
        
###########################################################################################################################
        
        
##                                          # PART 3.2 #
## ______________________________________________________________________________________________________________________##
##                                 Models generation since selected features. 
## ______________________________________________________________________________________________________________________##
        
  
## Models generation from the different selected feature subsets by each combination of gamma and lambda values.
        #  Tunecontrol Parameter.
        tunecontrol <- tune.control("cross", cross=10, performances=TRUE, best.model=TRUE)
        
        #  Multiple soft Random Forest models are built from the different feature subsets.
        rfmod <- RRF(Tabla$V ~., data=Tabla, set.seed(17), mtry=seq(2,5,by=1), proximity=TRUE, tunecontrol=tunecontrol)
        
        # Store number of selected features.
        nfeatures[i,j] <- length(rfmod$importance)
        
        # Validation of each soft Random Forest model from data.test.
        validation <- predict(rfmod, data.test)
        
        # Generation of the contingency tables to obtain the overall accuracy for each combination of gamma and lambda values.
        C_table <- table(observed = data.test[,1], predicted = validation)
        TN <- C_table[1]
        TP <- C_table[4]
        overall_accuracy[i,j] <- (TN+TP) / length(validation) 
        
        # Generation of the soft map for each combination of gamma and lambda values.
        soft_map <- predict(img, rfmod, na.rm=FALSE, type='prob', index=1:2, progress='text')
        
        # Store the soft map associate to the possitive occurrence of the dependant feature.
        soft_map=soft_map$SI    

        
##                                        # END PART 3.2 #
###########################################################################################################################
        
        
###########################################################################################################################
##                                          # PART 3.3 #
## ______________________________________________________________________________________________________________________##
##                                     Success Rate calculation
## ______________________________________________________________________________________________________________________##
## USE:The success ratios is a graph in which the true positive rate (TPR) for different occurrence area percentages is represented.
#      The True Positve Rate (True Positives/(True Positives + False Negatives)) is computed by finding the binary class 
#      probability membership threshold value that splits the map in different quantiles or occurrence area percentages. 
#        
## WARNING: - The TPR value is computed for every map reclassified as affected and non-affected using an independent test. 
#           - The model that is finally selected by AFGRRF is the one that is obtained from the feature subset that leads 
#             to the minimum occurrence area at a TPR equal or higher than the range of probability specified.
        
        
        # Percentiles used to divide the soft maps by area percentages
        area_percentages <- quantile(soft_map, probs=seq(0.70,0.99, by=0.01), na.rm=TRUE)    

        # Storing list with the different reclassifed soft maps for the different values of area_percentages.
        soft_map_reclassifed_areas <- list(rep(0, length(area_percentages))) 
        
        # Loop that stores the reclassifed maps from the soft maps for the different values of the area percentages in a layer stack.  
        for (k in 1:length(area_percentages)){
            soft_map_reclassifed_areas[k] <- soft_map > area_percentages[k]
        }
        soft_map_reclassifed_areas_stack <- stack(soft_map_reclassifed_areas)
        
        # Extraction of the soft_map_reclassifed_areas_stack values to the data.test.coor.
        area_values <- extract(soft_map_reclassifed_areas_stack, data.test.clasif.coor)
        
        # Transformation of the dependant feature to numeric (occurrence = 1, not occurrence = 0).
        v <- c(as.numeric(data.test.clasif.V))
        
        # Creation of a new subdataset that includes the coordinates from data.test.coor and the area values for the
        # soft_map_reclassifed_areas stack with the original values of the dependant feature in the data.test.clasif.
        sr_data <- cbind(area_values, v)
        
        # Remove missing values.    
        sr_data <- na.omit(sr_data)
        
        # Convert sr_data to data frame .
        sr_data <- data.frame(sr_data)

        # Change the original data extracted from the area_values {1, 2} to {0, 1}.
        sr_data$v <- sr_data$v - 1

        # Total number of positive occurrence cases for calculating the True Positive value.
        V1 <- sum(sr_data$v == 1)
        
        # Total number of negative occurrence cases.
        V0 <- sum(sr_data$v == 0)

        # Naming the soft_map_reclassifed_areas_stack by area percentage.
        areas <- c(names(soft_map_reclassifed_areas_stack))

        # Vector to store the success rate by areas.
        success_rate <- as.numeric(rep(0, length(areas)))
        
        # Vector to store ROC results.
        ROC <- as.numeric(rep(0, length(areas)))
        
        # Vectors to store the TPR, TNR, FPR, FNR for each soft map reclassified by areas:
        tpr.v <- as.numeric(rep(0, length(areas)))
        tnr.v <- as.numeric(rep(0, length(areas)))
        fpr.v <- as.numeric(rep(0, length(areas)))
        fnr.v <- as.numeric(rep(0, length(areas)))
        
        # Vector to store the reference of the soft maps.
        binary_maps <- c(names(sr_data[1:length(area_percentages)])) 
 

        # Loop to obtain the true positive rate.
        for (l in 1:length(binary_maps)) {
            succes_rate[l] <- (nrow(sr_data[sr_data$v == 1 &  sr_data[l] == 1, ])) / V1 * 100
            
            # Calculating of the ROC.
            AUC <- roc(sr_data[, i], sr_data$v)
            ROC[i] <- AUC$auc
            
            # Calculating of the tpr, tnr, fnr, fnr.      
            TP <- nrow(sr_data[sr_data$v == 1 &  sr_data[i] == 1, ])
            TN <- nrow(sr_data[sr_data$v == 0 &  sr_data[i] == 0, ])
            FP <- nrow(sr_data[sr_data$v == 0 &  sr_data[i] == 1, ])
            FN <- nrow(sr_data[sr_data$v == 1 &  sr_data[i] == 0, ])
            tpr.v[i] <- (TP)/(TP+FN)*100
            tnr.v[i] <- (TN)/(TN+FP)*100
            fpr.v[i] <- (FP)/(FP+FN)*100
            fnr.v[i] <- (FN)/(TP+FN)*100
        }
        succes_rate
        
        # Areas reference from the area percentage applied sequence.
        AREAS <- 30:1
        
        # Results of summary information as data frame.    
        RESULTS <- data.frame(cbind(AREAS, succes_rate, ROC, tpr.v, tnr.v, fpr.v, fnr.v))

        # Storing the minimum area value for the defined success ratios for each combination of gamma and lambda values models.
        area_sr_data90[i, j] <- min(RESULTS$AREAS[which(RESULTS$succes_rate >= 90)])
        area_sr_data85[i, j] <- min(RESULTS$AREAS[which(RESULTS$succes_rate >= 85)])
        area_sr_data80[i, j] <- min(RESULTS$AREAS[which(RESULTS$succes_rate >= 80)])

        # Storing the best ROC value for each combination of gamma and lambda values models.
        matrix_ROC[i, j] <- max(RESULTS$ROC)

        # Storing the maximum TPR, TNR, FPR and FNR values for each combination of gamma and lambda values models.
        maxtpr[i, j] <- max(RESULTS$tpr.v)
        maxtnr[i, j] <- max(RESULTS$tnr.v)
        maxfpr[i, j] <- max(RESULTS$fpr.v)
        maxfnr[i, j] <- max(RESULTS$fnr.v)
        
        # Storing the minimum TPR, TNR, FPR and FNR values for each combination of gamma and lambda values models
        mintpr[i, j] <- min(RESULTS$tpr.v)
        mintnr[i, j] <- min(RESULTS$tnr.v)
        minfpr[i, j] <- min(RESULTS$fpr.v)
        minfnr[i, j] <- min(RESULTS$fnr.v)    
    }
}
##                                        # END PART 3.3 #
###########################################################################################################################


###########################################################################################################################
##                                          # PART 4 #
## ______________________________________________________________________________________________________________________##
##                                        Summary tables
## ______________________________________________________________________________________________________________________##

## --------------------------------------- Arrangements of the summary tables ---------------------------------------------
#

# The summary matrix of numbers of selected features for each combination of gamma and lambda values is transposed to a vector.
fselection <- nfeatures 
pfselection <- expand.grid(fselection) 

# The summary matrix of overall accuracy results for each combination of gamma and lambda values is transposed to a vector.
overall_accuracy <- expand.grid(overall_accuracy) 

# The summary matrices of ROC results for each combination of gamma and lambda values are transposed to a vector.
ROC_auc.expand <- expand.grid(matrix_ROC)
tpr_max.expand <- expand.grid(tpr_max)
tnr_max.expand <- expand.grid(tnr_max)
fpr_max.expand <- expand.grid(fpr_max)
fnr_max.expand <- expand.grid(fnr_max)
tpr_min.expand <- expand.grid(tpr_min)
tnr_min.expand <- expand.grid(tnr_min)
fpr_min.expand <- expand.grid(fpr_min)
fnr_min.expand <- expand.grid(fnr_min)

# The summary matrices of the minimum area values for the defined success ratios for each combination of gamma and lambda values
# are transposed to a vector.
A90 <- expand.grid(area_sr_data90)
A85 <- expand.grid(area_sr_data85)
A80 <- expand.grid(area_sr_data80)

# Arrangement of the names of gamma and lambda.
gala <- merge(gamma,lambda)

# Summary table merging the previously defined vectors.
final_results <- cbind(gala,overall_accuracy,pfselection,ROC_auc.expand,tpr_max.expand,tnr_max.expand,fpr_max.expand,fnr_max.expand,tpr_min.expand,tnr_min.expand,fpr_min.expand,fnr_min.expand,A90,A85,A80)

# Naming final_results columns.
names(final_results, c("Gamma", "Lambda", "OA", "FN", "ROC", "TPRmax", "TNRmax", "FPRmax", "FNRmax", "TPRmin", "TNRmin", "FPRmin", "FNRmin", "A90", "A85", "A80"))

# Export global summary results to a file.
write.table(final_results, file="global_results.txt")
