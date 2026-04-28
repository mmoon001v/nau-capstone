library(readr)
library(dplyr)
library(randomForest)
library(gbm)
library(caret)
library(caretEnsemble)
library(pdp)
library(ggplot2)

# ----------------------------------------------------------------------------------------------------
# LOAD DATA AND PREPROCESS
# ----------------------------------------------------------------------------------------------------

#Load data
data <- read_csv("TheEntireDataSet.csv")

#Convert dataframe to numeric for analysis (i.e. we can't have character values in there such as <LOD)
i<-c(17:75)
data[,i]<-apply(data[ , i], 2, function(x) as.numeric(as.character(x)))

#For now, let us just set all NA values to 0 - in the future, we might want to remove columns/rows with many empty measurements
data[is.na(data)]<-0

#Select only elements that are common for both XRF and Curiosity datasets
data<-data %>% group_by(`File #`) %>% summarise(TopBottom = first(TopBottom),
                                                Mg=mean(Mg),
                                                Al=mean(Al),
                                                Si=mean(Si),
                                                P=mean(P),
                                                S=mean(S),
                                                K=mean(K),
                                                Ca=mean(Ca),
                                                Ti=mean(Ti),
                                                Mn=mean(Mn),
                                                Fe=mean(Fe),
                                                Ni=mean(Ni),
                                                Zn=mean(Zn),
                                                Cr=mean(Cr))
  
  
# ----------------------------------------------------------------------------------------------------
# CREATE MACHINE LEARNING HYPERGRIDS
# ----------------------------------------------------------------------------------------------------

#Parameter grids
rfGrid<-expand.grid(mtry=c(1:13)) 

gbmGrid <- expand.grid(interaction.depth=c(1, 3, 5), 
                       n.trees = (0:50)*50,
                       shrinkage=c(0.01, 0.001),
                       n.minobsinnode=10)

svmGrid<-expand.grid(C=c(0,0.25,0.5,0.75,1),
                     sigma=c(.5, 1, 2))

nnetGrid<-expand.grid(size = seq(from = 1, to = 50, by = 10),
                      decay = seq(from = 0, to = 1, by = 0.1))

#-----------------------------------------------------------------------------------------------------------
#CREATE TRAINING MODELS AND ANALYSE
#-----------------------------------------------------------------------------------------------------------

#10-fold CV
fit_control <- trainControl(p = 0.8,
                            method = "cv",
                            number = 3,
                            savePredictions = "final",
                            classProbs=TRUE)

#Set the seed for reproducible models
set.seed(825)

#Create random forest classificaiton tree
model_rf<-train(x=data[,c(3:15)],
                y=data$TopBottom,
                method='rf',
                importance=T,
                trControl = fit_control) #,
               # tuneGrid = rfGrid)

model_gbm<-train(x=data[,c(3:15)],
                 y=data$TopBottom,
                 method='gbm',
                 trControl = fit_control,
                 tuneGrid = gbmGrid)

model_svm<-train(x=data[,c(3:15)],
                 y=data$TopBottom,
                 method='svmRadial',
                 trControl = fit_control,
                 tuneGrid = svmGrid,
                 tuneLength = 10)

model_nnet<-train(x=data[,c(3:15)],
                  y=data$TopBottom,
                  method = "nnet", 
                  trControl = fit_control,
                  tuneGrid = nnetGrid,
                  #linout = TRUE,
                  maxit = 500)

#See model summary
model_rf
model_gbm
model_svm
model_nnet

#Compare models
models_compare<-resamples(list(RF=model_rf, GBM=model_gbm, SVM=model_svm, NNET=model_nnet))
summary(models_compare)

#Create ensemble model from individual ML methods
stacked_models<-caretList(x=data[,c(3:15)],
                          y=data$TopBottom,
                          trControl=fit_control,
                          tuneList=list(rf=caretModelSpec(method="rf", tuneGrid=rfGrid),
                                        gbm=caretModelSpec(method="gbm", tuneGrid=gbmGrid),
                                        svm=caretModelSpec(method="svmRadial",tuneGrid=svmGrid,tuneLength = 10),
                                        nnet=caretModelSpec(method="nnet", tuneGrid=nnetGrid, maxit=500)))

stacking_results<-resamples(stacked_models)
summary(stacking_results)
dotplot(stacking_results)
modelCor(stacking_results)

# stack using glm
model_ensemble<-caretStack(stacked_models, method="glm", trControl=fit_control)
print(model_ensemble)

#-----------------------------------------------------------------------------------------------------------
#ANALYSE INDIVIDUAL MODELS AND CREATE CONFUSION MATRICES
#-----------------------------------------------------------------------------------------------------------

#Create dataframe for 10-fold CV
rf_pred<-model_rf$pred
gbm_pred<-model_gbm$pred
svm_pred<-model_svm$pred
nnet_pred<-model_nnet$pred

#Create confusion matrix
rf_cm<-confusionMatrix(data = rf_pred$pred, reference = rf_pred$obs)
gbm_cm<-confusionMatrix(data = gbm_pred$pred, reference = gbm_pred$obs)
svm_cm<-confusionMatrix(data = svm_pred$pred, reference = svm_pred$obs)
nnet_cm<-confusionMatrix(data = nnet_pred$pred, reference = nnet_pred$obs)

rf_cm
gbm_cm
svm_cm
nnet_cm

#-----------------------------------------------------------------------------------------------------------
# PLOT VARIABLE IMPORTANCE
#-----------------------------------------------------------------------------------------------------------

plot(varImp(object=model_rf),main="RF - Variable Importance")
plot(varImp(object=model_gbm),main="GBM - Variable Importance")
plot(varImp(object=model_nnet),main="NNET - Variable Importance")

#-----------------------------------------------------------------------------------------------------------
# PLOT PARTIAL DEPENDENCE PLOTS
#-----------------------------------------------------------------------------------------------------------

#Partial dependence plots are useful for looking at how each predictor influences the model
#Need to rejig for classification exercise

var<-"Al"

partial(model_rf, pred.var = var, ice = TRUE, center = TRUE, 
            plot = TRUE, rug = TRUE, alpha = 0.2, plot.engine = "ggplot2", 
            train = data[,c(3:15)])

partial(model_gbm, pred.var = var, ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.2, plot.engine = "ggplot2", 
        train = data[,c(3:15)])

partial(model_svm, pred.var = var, ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.2, plot.engine = "ggplot2", 
        train = data[,c(3:15)])

partial(model_nnet, pred.var = var, ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.2, plot.engine = "ggplot2", 
        train = data[,c(3:15)])




