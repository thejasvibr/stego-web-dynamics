## R script which runs the actual nparLD code on R version 2.15.3

# Remember to install Renv (https://github.com/viking/Renv)
# and R-build https://github.com/viking/R-build
# to actually run this script open up a Terminal, and then
# source ~/.bash_profile
# Renv version 2.15.3
# R 
# .----now youre in the actual R session, then source this file 
# source('poresize_CN_nparLD.R')

library(nparLD)
porsizedata<- read.csv('poresize_nparLD.csv')
poresize_formula <- poresize~days*groupsize
res_poresize<-nparLD(poresize_formula,data=porsizedata,subject="webidnum",  description=T,
             group1.order=c(1,5,10,25))
res_poresize$lastrun<-Sys.time()
print('Pore Size analysis done')

cn_data <- read.csv('cn_nparLD.csv')
cn_formula <- cn~days*groupsize
res_cn<-nparLD(cn_formula,data=cn_data,subject="webidnum", 
                        description=T,
                        group1.order=c(1,5,10,25))
res_cn$lastrun<-Sys.time()
print('CN analysis done')


save(res_cn, res_poresize, file='results_CNandPoresize_nparld.RData')



