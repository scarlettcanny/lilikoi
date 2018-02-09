##### part 2: pathway transformation ######
library(pathifier)
PDS<-quantify_pathways_deregulation(as.matrix(plasma_raw), row.names(plasma_raw),metabolites.list,pathway.list,as.logical(c(training_norm_pheno,testing_norm_pheno)), attempts = 5, min_exp=0, min_std=0)
qpdmat <- matrix(as.data.frame(PDS$scores), nrow=length(names(PDS$scores)), byrow=TRUE)
dim(qpdmat)
colnames(qpdmat) <- colnames(plasma_raw)
rownames(qpdmat) <- names(PDS$scores)
write.table(qpdmat, "plasma_PDS_matrix.txt",sep="\t", row.names=TRUE, col.names=TRUE, quote=FALSE)
