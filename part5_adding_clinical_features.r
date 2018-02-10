##### part 5 Combined model with clinical variables #######

library(caret)

clinical_training<-clinic_lilikoi[training_ID,]
clinical_testing<-clinic_lilikoi[testing_ID,]

library(QuantPsyc)
clinical_training$age<-as.numeric(as.character(clinical_training$age))
dummies_racetraining = model.matrix(~clinical_training$race)  ### consider categorical data, making dummy variables ###
combined_training<-cbind(training_diagnosis[,-ncol(training_diagnosis)],clinical_training$age)
combined_training<-cbind(combined_training,dummies_racetraining[,-1]) ### don't need intercept term

combined_training<-as.matrix(combined_training)
class(combined_training)<-"numeric"
### normalize features ###
combined_training<-Make.Z(combined_training)
combined_training<-as.data.frame(combined_training)
combined_training<-cbind(combined_training,training_diagnosis$real_tf)
colnames(combined_training)[ncol(combined_training)]<-c("real_tf")


### feature selection ###
InfoGainAttributeEval(as.logical(combined_training$real_tf) ~ . , data = combined_training)->infogainfeatures_combined
infogainfeatures_combined<-infogainfeatures_combined[order(infogainfeatures_combined,decreasing=T)]
infogainfeatures_combined<-names(infogainfeatures_combined[infogainfeatures_combined>0.55])




### clinic-only model ###
#clinic_training<-clinical_training[,c(2,3,4)]
#training_tf<-as.data.frame(training_diagnosis[,102])
#row.names(training_tf)<-row.names(training_diagnosis)
#colnames(training_tf)<-c("real_tf")
clinic_training<-as.data.frame(clinical_training$age)
colnames(clinic_training)[1]<-c("age")
clinic_training$age<-as.numeric(as.character(clinic_training$age))
clinic_training<-cbind(clinic_training,dummies_racetraining[,-1])
#clinic_training<-clinic_training[,-1]
clinic_training<-as.matrix(clinic_training)
class(clinic_training)<-"numeric"
clinic_training<-Make.Z(clinic_training)
clinic_training<-as.data.frame(cbind(clinic_training,training_diagnosis$real_tf))
colnames(clinic_training)[6]<-c("real_tf")
InfoGainAttributeEval(as.logical(clinic_training$real_tf) ~ . , data = clinic_training)->infogainfeatures_clinic
infogainfeatures_clinic<-infogainfeatures_clinic[order(infogainfeatures_clinic,decreasing=T)]
infogainfeatures_clinic<-names(infogainfeatures_clinic[infogainfeatures_clinic>0.55])


#### testing matrices generation ######

clinical_testing$age<-as.numeric(as.character(clinical_testing$age))
dummies_racetesting = model.matrix(~clinical_testing$race)  ### consider categorical data, making dummy variables ###
combined_testing<-cbind(testing_diagnosis[,-ncol(testing_diagnosis)],clinical_testing$age)
combined_testing<-cbind(combined_testing,dummies_racetesting[,-1]) ### don't need intercept term

combined_testing<-as.matrix(combined_testing)
class(combined_testing)<-"numeric"
### normalize features ###
combined_testing<-Make.Z(combined_testing)
combined_testing<-as.data.frame(combined_testing)
combined_testing<-cbind(combined_testing,testing_diagnosis$real_tf)
colnames(combined_testing)[ncol(combined_testing)]<-c("real_tf")

clinic_testing<-as.data.frame(clinical_testing$age)
row.names(clinic_testing)<-row.names(clinical_testing)
colnames(clinic_testing)[1]<-c("age")
clinic_testing$age<-as.numeric(as.character(clinic_testing$age))
clinic_testing<-cbind(clinic_testing,dummies_racetesting[,-1])
#clinic_testing<-clinic_testing[,-1]
clinic_testing<-as.matrix(clinic_testing)
class(clinic_testing)<-"numeric"
clinic_testing<-Make.Z(clinic_testing)
clinic_testing<-as.data.frame(cbind(clinic_testing,testing_diagnosis$real_tf))
colnames(clinic_testing)[6]<-c("real_tf")


save(clinic_training,combined_training,clinic_testing,combined_testing,file="0209_clinic_combined_model_data.RData")
