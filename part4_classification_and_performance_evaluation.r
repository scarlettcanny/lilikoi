###### part 4: classification and performance evaluation ######


###### training dataset model and prediction #########
zd_training<- as.numeric(training_diagnosis$real_tf)
glm.out.diagnosis = glm(zd_training ~ .,family=binomial, data=training_diagnosis[,selected_pathways])
pred_train<-as.numeric(predict(glm.out.diagnosis, type="response"))
ref_train<-as.numeric(zd_training)
library(pROC)
pred_training_diagnosis<- roc(ref_train,pred_train)
pred_training_diagnosis<-smooth(pred_training_diagnosis,method="fitdistr")
pred_training_diagnosis ### summary of your ROC object, you can use this command to see the AUC of your ROC curve
plot(pred_training_diagnosis,lwd=2,col="black")  ### plot of your ROC object 0.911
ci.auc(pred_training_diagnosis) ### Use bootstrap method to generate confidence intervals ###  0.862~0.954

pred_train[pred_train>=0.5]<-1
pred_train[pred_train<0.5]<-0
ref_train<-as.numeric(!(training_norm_pheno))
confusionmatrix<-table(pred_train,ref_train)
TP<-confusionmatrix[2,2]
TN<-confusionmatrix[1,1]
FP<-confusionmatrix[2,1]
FN<-confusionmatrix[1,2]

sens.diagnosis <- TP/(TP+FN) ### 0.953
spec.diagnosis <- TN/(TN+FP) ### 0.869
MCC.diagnosis <- ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))  #### 0.831
F.diagnosis <-2*TP/(2*TP+FP+FN) ###  0.940


### for testing dataset prediction ###
### need input such as: testing_diagnosis (testing pds matrix data) ###
pred_test<-as.numeric(predict(glm.out.diagnosis,newdata=as.data.frame(testing_diagnosis[,selected_pathways]),type="response")) ### check for
ref_test<-as.numeric(testing_diagnosis$real_tf)

pred_testing_diagnosis<- roc(ref_test,pred_test)
pred_testing_diagnosis<-smooth(pred_testing_diagnosis,method="fitdistr")
pred_testing_diagnosis ### summary of your ROC object, you can use this command to see the AUC of your ROC curve
plot(pred_testing_diagnosis,lwd=2,col="black")  ### plot of your ROC object 0.883
ci.auc(pred_testing_diagnosis) ### Use bootstrap method to generate confidence intervals ### 0.774~0.984

pred_test[pred_test>=0.5]<-1
pred_test[pred_test<0.5]<-0
ref_test<-as.numeric(testing_diagnosis$real_tf)
confusionmatrix<-table(pred_test,ref_test)
TP<-confusionmatrix[2,2]
TN<-confusionmatrix[1,1]
FP<-confusionmatrix[2,1]
FN<-confusionmatrix[1,2]

sens.diagnosis <- TP/(TP+FN) ### 1
spec.diagnosis <- TN/(TN+FP) ### 0.733
MCC.diagnosis <- ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))  ### 0.797
F.diagnosis <-2*TP/(2*TP+FP+FN) ### 0.929


### diagnosis serum testing set ####
serum_matrix<-read.table("serum_PDS_matrix.txt",header=T,sep="\t")
serum_diagnosis<-t(serum_matrix)[,selected_pathways]
zd_serum<-pheno
pred_test<-as.numeric(predict(glm.out.diagnosis,newdata=as.data.frame(serum_diagnosis),type="response")) ### check for
ref_test<-as.numeric(zd_serum)

pred_yourdata_diagnosis<- roc(ref_test,pred_test)
pred_yourdata_diagnosis<-smooth(pred_yourdata_diagnosis,method="fitdistr")
pred_yourdata_diagnosis ### summary of your ROC object, you can use this command to see the AUC of your ROC curve
plot(pred_yourdata_diagnosis,lwd=2,col="black")  ### plot of your ROC object 1

ci.auc(pred_yourdata_diagnosis) ### Use bootstrap method to generate confidence intervals ### 1~1

pred_test[pred_test>=0.5]<-1
pred_test[pred_test<0.5]<-0
confusionmatrix<-table(pred_test,ref_test)
TP<-confusionmatrix[2,2]
TN<-confusionmatrix[1,1]
FP<-confusionmatrix[2,1]
FN<-confusionmatrix[1,2]

sens.diagnosis <- TP/(TP+FN) ### 1
spec.diagnosis <- TN/(TN+FP) ### 1
MCC.diagnosis <- ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))  ### 1
F.diagnosis <-2*TP/(2*TP+FP+FN) ### 1


#### plot for performance ########
plot(pred_training_diagnosis,lwd=2,col="black")
lines(pred_testing_diagnosis,lwd=2,col="blue",add=TRUE)
#lines(pred_yourdata_diagnosis,lwd=2,col="magenta",add=TRUE)
legend("bottomright",legend=c("COH plasma training","COH plasma testing"), lwd=c(2,2),col=c("black","blue"), border=FALSE, cex=1.5)

library(ggplot2)
diagnosis_output<-read.table(text = "Input AUC MCC Sens  Spec F1
                             1   1_diagnosis.training     0.911    0.831     0.953     0.869    0.940
                             2   2_diagnosis.testing     0.883    0.797     1     0.733    0.929
                             3   3_diagnosis.serum     1    1    1     1    1",header = TRUE,sep = "")
library(reshape2)
ggplot(melt(diagnosis_output)) +
  geom_col(aes(x=Input, y=value, fill=variable), stat='identity', position='dodge') +
  scale_fill_brewer(type='qual', palette=3)



diagnosis_output<-read.table(text = "Input AUC MCC Sens  Spec F1
                             1   1_diagnosis.training     0.911    0.831     0.953     0.869    0.940
                             2   2_diagnosis.testing     0.883    0.797     1     0.733    0.929",header = TRUE,sep = "")
library(reshape2)
ggplot(melt(diagnosis_output)) +
  geom_col(aes(x=Input, y=value, fill=variable), stat='identity', position='dodge') +
  scale_fill_brewer(type='qual', palette=3)
