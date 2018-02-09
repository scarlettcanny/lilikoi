install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
install.packages(c("digest", "git2r", "usethis"))
devtools::install_github("hadley/devtools")
library(devtools)
has_devel()
devtools::load_all()
install.packages("formatR")
formatR::tidy_dir("R")

devtools::use_package("pathifier")
devtools::use_package("RWeka")


### add function to the package ###

devtools::document()
#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}




con <- file('D:\\\\metabolomics\\serum\\COH Serum Original Combined.gmt')
open(con);
metabolites.list <- list();
pathway.list <- list();
current.line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  tempvec <- (unlist(strsplit(line, split="\t")))
  metabolites.list[[current.line]] <- tempvec[2:length(tempvec)]
  pathway.list[[current.line]] <- tempvec[1]
  current.line <- current.line + 1
}
close(con)



for (i in 1:length(pathway.list))
{
  metabolites.list[[i]]<-setdiff(metabolites.list[[i]],c(" ","NA"))
}
names(metabolites.list)<-pathway.list

library(purrr)
metabolites.list1<-compact(metabolites.list)
pathway.list1<-names(metabolites.list1)

library(pathifier)

load("serum_data_HMDB_lib.RData")
serum<-metabolites_serum[,5:138]
pheno<-c(rep(0,31),rep(1,103))

devtools::use_data(metabolites.list,pathway.list, serum,pheno, internal = TRUE,overwrite = TRUE)

library(pathifier)
PDS<-quantify_pathways_deregulation(as.matrix(serum), row.names(serum),or.metabolites.list,or.pathway.list,!as.logical(pheno), attempts = 5, min_exp=0, min_std=0)
qpdmat <- matrix(as.data.frame(PDS$scores), nrow=length(names(PDS$scores)), byrow=TRUE)
dim(qpdmat)
colnames(qpdmat) <- colnames(serum)
rownames(qpdmat) <- names(PDS$scores)
write.table(qpdmat, "serum_PDS_matrix.txt",sep="\t", row.names=TRUE, col.names=TRUE, quote=FALSE)





##### training data and testing dataset generation #######
pds_matrix<-as.data.frame(cbind(t(pds_matrix),real_tf))
your_case<-pds_matrix[pds_matrix[,"real_tf"]==TRUE,]
your_control<-pds_matrix[pds_matrix[,"real_tf"]==FALSE,]
ncases<-nrow(your_case)
ncontrols<-nrow(your_control)
set.seed(2018)
#### need your number of cases as input
training_case_ID<-sample(row.names(your_case),round(ncases*0.8),replace=F)
#### need your number of controls as input
training_control_ID<-sample(row.names(your_control),round(ncontrols*0.8),replace=F)

testing_case_ID<-setdiff(row.names(your_case),training_case_ID)
testing_ctrl_ID<-setdiff(row.names(your_control),training_control_ID)

training_diagnosis<-rbind(your_case[training_case_ID,],your_control[training_control_ID,])
testing_diagnosis<-rbind(your_case[testing_case_ID,],your_control[testing_ctrl_ID,])

write.csv(training_diagnosis,"training_data.csv",quote=F,row.names=F)
write.csv(training_diagnosis,"training_data.csv",quote=F,row.names=F)


