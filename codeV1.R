setwd('/Users/knguyen1/Documents/Kaggle/AsiaActuary')
library(data.table)

train <- fread('SAStraining.csv')
test <- fread('SAStest.csv')


train <- subset(train, select = -c(diag_1_desc, diag_2_desc, diag_3_desc))
test <- subset(test, select = -c(diag_1_desc, diag_2_desc, diag_3_desc))

ID <- test$patientID
train <- train[, ":="(admissionDate = NULL,
                      patientID = NULL, 
                      race = as.numeric(as.factor(race)), 
                      gender = ifelse(gender == "Male", 1, 0), 
                      age = as.numeric(as.factor(age)),
                      weight = NULL, 
                      admission_type_id = as.numeric(as.factor(admission_type_id)),
                      discharge_disposition_id = as.numeric(as.factor(discharge_disposition_id)),
                      admission_source_id = as.numeric(as.factor(admission_source_id)),
                      time_in_hospital = as.numeric(time_in_hospital), 
                      payer_code = as.numeric(as.factor(payer_code)),
                      medical_specialty = as.numeric(as.factor(medical_specialty)),
                      num_lab_procedures = as.numeric(num_lab_procedures), 
                      num_procedures = as.numeric(num_procedures), 
                      num_medications = as.numeric(num_medications), 
                      number_outpatient = as.numeric(number_outpatient), 
                      number_emergency = as.numeric(number_emergency), 
                      number_inpatient = as.numeric(number_inpatient), 
                      diag_1 = as.numeric(diag_1), 
                      diag_2 = as.numeric(diag_2), 
                      diag_3 = as.numeric(diag_3),
                      number_diagnoses = as.numeric(number_diagnoses),
                      max_glu_serum = as.numeric(as.factor(max_glu_serum)),
                      A1Cresult = as.numeric(as.factor(A1Cresult)),
                      insulin = as.numeric(ordered(insulin, levels = c("No", "Down", "Steady", "Up"))),
                      metformin = as.numeric(ordered(metformin, levels = c("No", "Down", "Steady", "Up"))),
                      repaglinide = as.numeric(ordered(repaglinide, levels = c("No", "Down", "Steady", "Up"))),  
                      nateglinide = as.numeric(ordered(nateglinide, levels = c("No", "Down", "Steady", "Up"))), 
                      chlorpropamide = as.numeric(ordered(chlorpropamide, levels = c("No", "Down", "Steady", "Up"))), 
                      glimepiride = as.numeric(ordered(glimepiride, levels = c("No", "Down", "Steady", "Up"))), 
                      acetohexamide = as.numeric(ordered(acetohexamide, levels = c("No", "Down", "Steady", "Up"))), 
                      glipizide = as.numeric(ordered(glipizide, levels = c("No", "Down", "Steady", "Up"))), 
                      glyburide = as.numeric(ordered(glyburide, levels = c("No", "Down", "Steady", "Up"))), 
                      tolbutamide = as.numeric(ordered(tolbutamide, levels = c("No", "Down", "Steady", "Up"))), 
                      pioglitazone = as.numeric(ordered(pioglitazone, levels = c("No", "Down", "Steady", "Up"))),
                      rosiglitazone = as.numeric(ordered(rosiglitazone, levels = c("No", "Down", "Steady", "Up"))),
                      acarbose = as.numeric(ordered(acarbose, levels = c("No", "Down", "Steady", "Up"))),   
                      miglitol = as.numeric(ordered(miglitol, levels = c("No", "Down", "Steady", "Up"))), 
                      troglitazone = as.numeric(ordered(troglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      tolazamide = as.numeric(ordered(tolazamide, levels = c("No", "Down", "Steady", "Up"))), 
                      examide = as.numeric(ordered(examide, levels = c("No", "Down", "Steady", "Up"))), 
                      citoglipton = as.numeric(ordered(citoglipton, levels = c("No", "Down", "Steady", "Up"))), 
                      glyburide.metformin = as.numeric(ordered(glyburide.metformin, levels = c("No", "Down", "Steady", "Up"))), 
                      glipizide.metformin = as.numeric(ordered(glipizide.metformin, levels = c("No", "Down", "Steady", "Up"))), 
                      glimepiride.pioglitazone = as.numeric(ordered(glimepiride.pioglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      metformin.rosiglitazone = as.numeric(ordered(metformin.rosiglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      metformin.pioglitazone = as.numeric(ordered(metformin.pioglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      change = ifelse(change == "Ch", 1, 0), 
                      diabetesMed = ifelse(diabetesMed == "Yes", 1, 0)
                      )]


test <- test[, ":="(patientID = NULL,
                    admissionDate = NULL,
                      race = as.numeric(as.factor(race)), 
                      gender = ifelse(gender == "Male", 1, 0), 
                      age = as.numeric(as.factor(age)),
                      weight = NULL, 
                      admission_type_id = as.numeric(as.factor(admission_type_id)),
                      discharge_disposition_id = as.numeric(as.factor(discharge_disposition_id)),
                      admission_source_id = as.numeric(as.factor(admission_source_id)),
                      time_in_hospital = as.numeric(time_in_hospital), 
                      payer_code = as.numeric(as.factor(payer_code)),
                      medical_specialty = as.numeric(as.factor(medical_specialty)),
                      num_lab_procedures = as.numeric(num_lab_procedures), 
                      num_procedures = as.numeric(num_procedures), 
                      num_medications = as.numeric(num_medications), 
                      number_outpatient = as.numeric(number_outpatient), 
                      number_emergency = as.numeric(number_emergency), 
                      number_inpatient = as.numeric(number_inpatient), 
                      diag_1 = as.numeric(diag_1), 
                      diag_2 = as.numeric(diag_2), 
                      diag_3 = as.numeric(diag_3),
                      number_diagnoses = as.numeric(number_diagnoses),
                      max_glu_serum = as.numeric(as.factor(max_glu_serum)),
                      A1Cresult = as.numeric(as.factor(A1Cresult)),
                    insulin = as.numeric(ordered(insulin, levels = c("No", "Down", "Steady", "Up"))),
                      metformin = as.numeric(ordered(metformin, levels = c("No", "Down", "Steady", "Up"))),
                      repaglinide = as.numeric(ordered(repaglinide, levels = c("No", "Down", "Steady", "Up"))),  
                      nateglinide = as.numeric(ordered(nateglinide, levels = c("No", "Down", "Steady", "Up"))), 
                      chlorpropamide = as.numeric(ordered(chlorpropamide, levels = c("No", "Down", "Steady", "Up"))), 
                      glimepiride = as.numeric(ordered(glimepiride, levels = c("No", "Down", "Steady", "Up"))), 
                      acetohexamide = as.numeric(ordered(acetohexamide, levels = c("No", "Down", "Steady", "Up"))), 
                      glipizide = as.numeric(ordered(glipizide, levels = c("No", "Down", "Steady", "Up"))), 
                      glyburide = as.numeric(ordered(glyburide, levels = c("No", "Down", "Steady", "Up"))), 
                      tolbutamide = as.numeric(ordered(tolbutamide, levels = c("No", "Down", "Steady", "Up"))), 
                      pioglitazone = as.numeric(ordered(pioglitazone, levels = c("No", "Down", "Steady", "Up"))),
                      rosiglitazone = as.numeric(ordered(rosiglitazone, levels = c("No", "Down", "Steady", "Up"))),
                      acarbose = as.numeric(ordered(acarbose, levels = c("No", "Down", "Steady", "Up"))),   
                      miglitol = as.numeric(ordered(miglitol, levels = c("No", "Down", "Steady", "Up"))), 
                      troglitazone = as.numeric(ordered(troglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      tolazamide = as.numeric(ordered(tolazamide, levels = c("No", "Down", "Steady", "Up"))), 
                      examide = as.numeric(ordered(examide, levels = c("No", "Down", "Steady", "Up"))), 
                      citoglipton = as.numeric(ordered(citoglipton, levels = c("No", "Down", "Steady", "Up"))), 
                      glyburide.metformin = as.numeric(ordered(glyburide.metformin, levels = c("No", "Down", "Steady", "Up"))), 
                      glipizide.metformin = as.numeric(ordered(glipizide.metformin, levels = c("No", "Down", "Steady", "Up"))), 
                      glimepiride.pioglitazone = as.numeric(ordered(glimepiride.pioglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      metformin.rosiglitazone = as.numeric(ordered(metformin.rosiglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      metformin.pioglitazone = as.numeric(ordered(metformin.pioglitazone, levels = c("No", "Down", "Steady", "Up"))), 
                      change = ifelse(change == "Ch", 1, 0), 
                      diabetesMed = ifelse(diabetesMed == "Yes", 1, 0)
)]

target <- as.numeric(train$readmitted)
train$readmitted = NULL
test$readmitted = NULL
test = test[, .SDcols=names(train)]

train[is.na(train)] <- -1
test[is.na(test)] <- -1

model_xgb_cv <- xgb.cv(data=as.matrix(train), 
                       label=as.matrix(target), 
                       nfold=5, 
                       objective="binary:logistic", 
                       nrounds=750, 
                       eta=0.025, 
                       max_depth=2, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       eval_metric="auc")


preds = rep(0, nrow(test))

for (z in 1:40){
  set.seed(z + 80)
  model_xgb <- xgboost(data=as.matrix(train), 
                       label=as.matrix(target), 
                       objective="binary:logistic", 
                       nrounds=750, 
                       eta=0.025, 
                       max_depth=2, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       eval_metric="auc")
  preds <- preds + predict(model_xgb, as.matrix(test))  
}

preds <- preds/40

submission <- data.frame('patientID' = ID, 'readmitted' = preds)
write.csv(submission, 'submission_v1.csv', row.names=F)