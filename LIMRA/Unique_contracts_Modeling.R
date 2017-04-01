###### LIMRA_SC Modeling script ############
# LIMRA_SC = read.csv("LIMRA_SC.csv")
View(LIMRA_SC)
# install.packages("pROC")
install.packages("rmarkdown")
install.packages("h2o")
library(foreach)
library(Matrix)
library(glmnet)
library(dplyr)
library(splitstackshape)
library(caret)
library(pROC)
library(h2o)

data_partion_list = stratified(LIMRA_SC,"TARGET",0.1,bothSets = T)
str(data_partion_list)
test_SC = as.data.frame(data_partion_list[1])
train_SC = as.data.frame(data_partion_list[2])
train_SC_0 = train_SC[train_SC$SAMP2.TARGET == 0,]
train_SC_1 = train_SC[train_SC$SAMP2.TARGET == 1,]

nrow(train_SC[train_SC$SAMP2.TARGET == 1,])
nrow(train_SC[train_SC$SAMP2.TARGET == 0,])

train_SC_0_copy = train_SC_0

bal_data_chunks_com = data.frame()
for(i in seq(1,22))
{
train_SC_0_equal_sample = train_SC_0_copy[sample(x = nrow(train_SC_0_copy),
                                          size = ifelse(nrow(train_SC_0_copy) < 65000, nrow(train_SC_0_copy),65000)
                                          ,replace = F),]
train_SC_0_copy = train_SC_0_copy[!(train_SC_0_copy$SAMP2.CONTRACT %in% train_SC_0_equal_sample$SAMP2.CONTRACT),]
temp = rbind(train_SC_1,train_SC_0_equal_sample) 
bal_data_chunks_com = rbind(bal_data_chunks_com,temp)
}

##########################################################
str(LIMRA_SC)
LIMRA_SC$MOISS = as.factor(LIMRA_SC$MOISS)
LIMRA_SC$DAYISS = as.factor(LIMRA_SC$DAYISS)

indp.var = c("LIMRA","M_E_1","FIXED_ACCT","NUM_SUBACCTS","NUM_SUBACCTS.NA","SURBASIS","SURCHG_YR1","SURCHG_YR0","STATE",
             "MOISS","DAYISS","Date_ISSUE","ANNUITY_AGE","MKTYPE","AVBOY","PWITHD","SWITHD","BIRTH","SEX",
             "DISTRIB","COSTSTR","S_ANNUITANT","CURPREM.NA","CUMLPREMWITH.NA","CURPREM","FPREM","CUMLPREM","CUMLWITHD",
             "WB_DAY_EFF","WB_BENBASE_BOY","WB_MAX_PERC","WB_MAX_AMT","WB_LIFEPAYOUT","WBL_MO_INTR","WBL_ELECT_POSTISSUE",
             "WBL_CANCEL","WBL_MAX_AGE_ELECT","WBL_MIN_AGE_ONSET","WBL_MAX_AGE_ONSET","WBL_SPOUSAL","WBL_YRS_PREMAPPLIED",
             "WBL_ALLOC_REST_FORCED","WBL_ALLOC_REST_LIMITS","WBL_ALLOC_REST_OTHER","WBL_BEN_REDUCT_D4D","WBL_BEN_REDUCT_PR",
             "WBL_BEN_REDUCT_RMD","WBL_BEN_REDUCT_OTH","WBL_STEPUP_AVAIL","WBL_STEPUP_FREQ","WBL_STEPUP_AUTO","WBL_STEPUP_CHANGE",
             "WBL_STEPUP_COST","WBL_STEPUP_WINDOW","WBL_MAX_WITHD_PCT1","WBL_MAX_WITHD_PCT_COMPLEXITY","WBL_LTC","OWNER_AGE",
             "AGE_AT_PURCHASE","PCT_CALC_MAX_WITHDRAWN","RAT_BENBASE_BOY","Rollup_Ind","Rollup_WP","ITMrange")
str(LIMRA_SC[,indp.var])

test = stratified(LIMRA_SC,"TARGET",0.1,bothSets = F)
test = as.data.frame(test)
train = LIMRA_SC[!LIMRA_SC$CONTRACT %in% test$CONTRACT,]

train_0 = train[train$TARGET == 0,]
train_1 = train[train$TARGET == 1,]
train_0_equal_no_of_1 = train_0[sample(x = nrow(train_0), size = nrow(train_1)),]

Balanced_sample = rbind(train_0_equal_no_of_1,train_1)

######## Creating different samples of varying proportion of 0's and 1's ####################################
# k=1
# train_0_random = train_0[sample(x = nrow(train_0), size = nrow(train_0)),]
# nrow(train_0_random)
# sample_6_plus = rbind(train_0_random[((nrow(train_1)*15)+1):nrow(train_0_random),],train_1)
# nrow(sample_6_plus)
# write.csv(sample_6_plus,file = "sample_6_plus.csv",row.names = F)
# rm(sample_6_plus)
# rm(train_0_random)

###############################################################################################################

########################## Logistic Regression with LASSO #############################
## For Logistic model with Regularization prepare data, by creating Dummy Variables
bol = c()
for(i in indp.var)
{
  bol = c(bol,ifelse(class(LIMRA_SC[,i]) == "factor",TRUE,FALSE))
  
}
indp.var.factor = indp.var[bol]
str(LIMRA_SC[,indp.var.factor])
indp.var.cont = indp.var[!bol]  
str(LIMRA_SC[,indp.var.cont])

data_type_change = function(dataframe,indp.var.factor)
{
  for(i in c(indp.var.factor,"TARGET"))
  {
    dataframe[,i] = as.factor(dataframe[,i])
  }
  dataframe
}


c = ""
for(item in indp.var.factor)
{
  c = ifelse(c=="",item,paste(c,item,sep="+"))
}

formula = paste("~",c,sep = "")
dum = dummyVars(formula,data = Balanced_sample)
dummy = head(predict(dum,Balanced_sample),n = nrow(Balanced_sample))
dim(dummy) 
str(dummy)

dum.test = dummyVars(formula,data = test)
dummy.test = head(predict(dum.test,test),n = nrow(test))
nrow(dummy.test)

indp.var.matrix = cbind(dummy,Balanced_sample[,indp.var.cont])
indp.var.matrix = as.matrix(indp.var.matrix)
str(indp.var.matrix)

indp.var.matrix.test = cbind(dummy.test,test[,indp.var.cont])
indp.var.matrix.test = as.matrix(indp.var.matrix.test)
str(indp.var.matrix.test)

dgcmatrix = as(indp.var.matrix,"dgCMatrix")
head(indp.var.matrix)
str(dgcmatrix)
rm(dummy)
rm(dummy.test)
glmmod <- glmnet(dgcmatrix, y=Balanced_sample[,"TARGET"], alpha=1, family="binomial")
plot(glmmod, xvar="lambda")

cv.glmmod = cv.glmnet(dgcmatrix, y=Balanced_sample[,"TARGET"], alpha=1, family="binomial")
# plot(cv.glmmod)
best.lambda <- cv.glmmod$lambda.min
cv.glmmod$glmnet.fit
colnames(indp.var.matrix)[which(coef(cv.glmmod, s = "lambda.min") == 0)]


glmnetPredict.train <- predict(cv.glmmod, indp.var.matrix, s="lambda.min",type="response")
glmnetPredict.train = as.vector(glmnetPredict.train)
roc_obj.train <- roc(Balanced_sample[,"TARGET"], glmnetPredict.train)
auc(roc_obj.train)

glmnetPredict.test = predict(cv.glmmod,indp.var.matrix.test,s="lambda.min",type="response")
glmnetPredict.test = as.vector(glmnetPredict.test)
roc_obj.test <- roc(test[,"TARGET"], glmnetPredict.test)
auc(roc_obj.test)

test$LL_prediction_prob = glmnetPredict.test
plot(density(test[test$TARGET == 0,"LL_prediction_prob"]),col="blue")
lines(density(test[test$TARGET == 1,"LL_prediction_prob"]),col="red")

test$LL_prediction = ifelse(test$LL_prediction_prob > 0.4,1,0)
table(test$TARGET,test$LL_prediction)
sum(table(test$TARGET,test$LL_prediction))

##########################################################
###################################################################
#################################################################
########################## Reference  #############################
## For Logistic model with Regularization prepare data, by creating Dummy Variables
summary(train)
dum = dummyVars(~profession+marital+schooling+default+housing+loan+contact+month+day_of_week+
                  poutcome,data=train)
dummy=cbind(train[c("custAge","campaign","pdays","previous","emp.var.rate","cons.price.idx",
                    "cons.conf.idx","euribor3m","nr.employed","pmonths","pastEmail","IsAgeMissing",
                    "responded")],
            head(predict(dum,train),
                 n=nrow(train)))
# Convert all the variables to Numeric as the input to glmnet is Matrix
dummy$IsAgeMissing=as.numeric(as.character(dummy$IsAgeMissing))
dummy=as.matrix(dummy)
dummy = as(dummy, "dgCMatrix")
indp.var=setdiff(colnames(dummy),(dep.var))
# Fit model using Ridge regression 
glmnetModel <- cv.glmnet(x=dummy[,indp.var],y=dummy[,dep.var], alpha = 0, family = "binomial", type.measure = "auc")
# Observe the summary of model with Lambda and CV scores
summary(glmnetModel)
str(glmnetModel)
# Predict using glmnet on training data to see how probabilities are given
# This can be used to decide Probability cutoff later
glmnetPredict1 <- predict(glmnetModel, dummy[,indp.var], s="lambda.min",type="response")
# Calculate and see ROC
roc_obj <- roc(dummy[,"responded"], glmnetPredict1)
auc(roc_obj)
# Prepare test data similar to training data to get predictions using glmnet
dum_test = dummyVars(~profession+marital+schooling+default+housing+loan+contact+month+day_of_week+
                       poutcome,data=test)
dummy_test=cbind(test[c("custAge","campaign","pdays","previous","emp.var.rate","cons.price.idx",
                        "cons.conf.idx","euribor3m","nr.employed","pmonths","pastEmail","IsAgeMissing")],
                 head(predict(dum_test,test),
                      n=nrow(test)))
dummy_test$IsAgeMissing=as.numeric(as.character(dummy_test$IsAgeMissing))
# Create Dummy Variables to get the matix in test also having same dimension as training
dummy_test$default.yes = 0
dummy_test$schooling.illiterate = 0
dummy_test=as.matrix(dummy_test)
dummy_test = as(dummy_test, "dgCMatrix")
# Predict on test data using the model being created
glmnetPredict <- predict(glmnetModel, dummy_test[,indp.var], s="lambda.min",type="response")
head(glmnetPredict)


############################################################3
##########################################
##############################################################################33

# Use H2o package for building Random Forest with 5 fold cross Validation
# Allocate 4 GB memory for H2o
# This will take HUGE Runtime ..!!
localh2o = h2o.init(nthreads = -1, max_mem_size = "4G")
train_h2o = as.h2o(Balanced_sample)
test_h2o = as.h2o(test)
# Fit Random Forest model and tune the parameters based on Cross Validation Scores
indp.var_2 = c("NUM_SUBACCTS","SURBASIS","SURCHG_YR0","ANNUITY_AGE","AVBOY","COSTSTR","RAT_BENBASE_BOY","WBL_CANCEL",
               "WBL_MIN_AGE_ONSET","WBL_MAX_AGE_ELECT","SURCHG_YR1","WBL_MAX_WITHD_PCT_COMPLEXITY","OWNER_AGE",
               "PCT_CALC_MAX_WITHDRAWN","Rollup_WP","S_ANNUITANT")

rf_fit = h2o.randomForest(x = indp.var_2,
                          y = "TARGET",
                          training_frame = train_h2o,
                          model_id = "rf_fit",
                          ntrees = 300,
                          keep_cross_validation_predictions = T,
                          score_each_iteration = T,
                          seed = 1000,
                          stopping_metric = "AUC",
                          nfolds=5)
# Observe the Summary of Random Forest model and predict on Test set
summary(rf_fit)
h2o.varimp(rf_fit)
rf_Predict <- h2o.predict(object=rf_fit,newdata = test_h2o)
test$prediction = as.data.frame(rf_Predict)$predict
table(test$TARGET,test$prediction)

##############################################################################
sum(table(test$TARGET,test$prediction))

#######################################################################
######################################################################

indp.var_1 = c("LIMRA","M_E_1","FIXED_ACCT","NUM_SUBACCTS","NUM_SUBACCTS.NA","SURBASIS","SURCHG_YR1","SURCHG_YR0","STATE",
               "MOISS","DAYISS","Date_ISSUE","ANNUITY_AGE","MKTYPE","AVBOY","PWITHD","SWITHD","BIRTH","SEX",
               "DISTRIB","COSTSTR","S_ANNUITANT","CURPREM.NA","CUMLPREMWITH.NA","CURPREM","FPREM","CUMLPREM","CUMLWITHD",
               "WB_DAY_EFF","WB_BENBASE_BOY","WB_MAX_PERC","WB_MAX_AMT","WB_LIFEPAYOUT","WBL_MO_INTR","WBL_ELECT_POSTISSUE",
               "WBL_CANCEL","WBL_MAX_AGE_ELECT","WBL_MIN_AGE_ONSET","WBL_MAX_AGE_ONSET","WBL_SPOUSAL","WBL_YRS_PREMAPPLIED",
               "WBL_ALLOC_REST_FORCED","WBL_ALLOC_REST_LIMITS","WBL_ALLOC_REST_OTHER","WBL_BEN_REDUCT_D4D","WBL_BEN_REDUCT_PR",
               "WBL_BEN_REDUCT_RMD","WBL_BEN_REDUCT_OTH","WBL_STEPUP_AVAIL","WBL_STEPUP_FREQ","WBL_STEPUP_AUTO","WBL_STEPUP_CHANGE",
               "WBL_STEPUP_COST","WBL_STEPUP_WINDOW","WBL_MAX_WITHD_PCT1","WBL_MAX_WITHD_PCT_COMPLEXITY","WBL_LTC","OWNER_AGE",
               "AGE_AT_PURCHASE","PCT_CALC_MAX_WITHDRAWN","RAT_BENBASE_BOY","Rollup_Ind","Rollup_WP","ITMrange")

indp.var_2 = c("NUM_SUBACCTS","SURBASIS","SURCHG_YR0","ANNUITY_AGE","AVBOY","COSTSTR","RAT_BENBASE_BOY","WBL_CANCEL",
               "WBL_MIN_AGE_ONSET","WBL_MAX_AGE_ELECT","SURCHG_YR1","WBL_MAX_WITHD_PCT_COMPLEXITY","OWNER_AGE",
               "PCT_CALC_MAX_WITHDRAWN","Rollup_WP","S_ANNUITANT")

All_models_prediction = data.frame(TARGET = test$TARGET)
for(i in c("sample_1.0.csv","sample_2.0.csv","sample_3.0.csv","sample_4.0.csv","sample_5.0.csv","sample_6_plus.csv"))
{
train_h2o = as.h2o(data_type_change(read.csv(i),indp.var.factor))
rf_fit_1 = h2o.randomForest(x = indp.var_2,
                          y = "TARGET",
                          training_frame = train_h2o,
                          model_id = "rf_fit",
                          ntrees = 100,
                          seed = 1000,
                          stopping_metric = "AUC",
                          nfolds=5)

rf_Predict_1 <- h2o.predict(object=rf_fit_1,newdata = test_h2o)
a = as.data.frame(rf_Predict_1)$predict
All_models_prediction = cbind(All_models_prediction,a)
}

######### Combining all models ##########################

##################################################################


colnames(All_models_prediction) = c("TARGET","sample_1.0","sample_2.0","sample_3.0","sample_4.0","sample_5.0","sample_6_plus")
View(All_models_prediction)
temp = All_models_prediction[,c("sample_1.0","sample_2.0","sample_3.0","sample_4.0","sample_5.0","sample_6_plus")]
temp = apply(temp, 2, function(x) as.numeric(as.character(x)))
temp = as.data.frame(temp)
All_models_prediction$avg = apply(temp,1,mean)

All_models_prediction$vote = ifelse(All_models_prediction$avg > 0.90,1,0)
table(All_models_prediction$TARGET,All_models_prediction$vote)

plot(density(All_models_prediction[All_models_prediction$TARGET==0,"avg"]),col="blue")
lines(density(All_models_prediction[All_models_prediction$TARGET==1,"avg"]),col="red")


for( i in seq(0,1,0.05))
{
  
}
######### Insight - Predict into three classes, High risk, Med risk, Low risk #################
