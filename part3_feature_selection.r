##### part 3: feature selection ######
pds_matrix<-read.table("plasma_PDS_matrix.txt",header=T,sep="\t")
real_tf<-!c(training_norm_pheno,testing_norm_pheno)
pds_matrix<-as.data.frame(cbind(t(pds_matrix),real_tf))

training_diagnosis<-pds_matrix[training_ID,]
testing_diagnosis<-pds_matrix[testing_ID,]



library(RWeka)

InfoGainAttributeEval(as.logical(training_diagnosis$real_tf) ~ . , data = training_diagnosis)->infogainfeatures
#GainRatioAttributeEval(as.logical(training_diagnosis$real_tf) ~ . , data = training_diagnosis)->gainratiofeatures
#gainratiofeatures[gainratiofeatures>0.6]
#infogainfeatures[infogainfeatures>0.55]
#selected_pathways<-names(gainratiofeatures[gainratiofeatures>0.6])
selected_pathways<-names(infogainfeatures[infogainfeatures>0.55])


### mutual information about selected pathways ###

library(infotheo)
info.paireddiagnosis.R<-discretize(training_diagnosis[,selected_pathways])
info.paireddiagnosis.R<-cbind(info.paireddiagnosis.R,as.numeric(as.matrix(training_diagnosis[,ncol(training_diagnosis)])))
I.R <- mutinformation(info.paireddiagnosis.R,method= "emp")
I.R.paireddiagnosis<-I.R[,ncol(I.R)]
#names(I.R.paireddiagnosis)<-c(1:ncol(I.R))


####
#barplot(I.R.paireddiagnosis[-c(ncol(I.R))],main=c("selected pathways mutual information"),ylim=c(0,0.5))    #### CFS in R
library(ggplot2)
theTable <- within(as.data.frame(I.R.paireddiagnosis),
                   I.R.paireddiagnosis <- as.numeric(I.R.paireddiagnosis,
                                                     levels=names(sort(table(I.R.paireddiagnosis),
                                                                       decreasing=TRUE))))
theTable<-cbind(row.names(theTable),theTable)
theTable<-theTable[-ncol(I.R),]
colnames(theTable)[1]<-c("name")
theTable <- transform(theTable,
                      name = reorder(name,order(I.R.paireddiagnosis, decreasing = TRUE)))


ggplot(theTable,aes(x=name,y=I.R.paireddiagnosis))+geom_bar(binwidth=1,stat="identity") + theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))




legend("topright",legend=names(I.R.paireddiagnosis[order(I.R.paireddiagnosis,decreasing=T)])[-1], border=FALSE, cex=0.7)
