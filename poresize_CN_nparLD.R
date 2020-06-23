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

## MEAN DATA nparLD analysis - which TB thinks is not very reliable because the mean is very outlier sensitive. 

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

### D and H suspect that most of the variation comes from the first day and when the spiders are still 'settling' into the web construction.
# What happens if we remove day 1?

## Poresize without day 1 
porsize_woday1 <- subset(porsizedata, days>1)
woday1_poresize_results <-  nparLD(poresize_formula,data=porsize_woday1,subject="webidnum", 
                                description=T,
                                group1.order=c(1,5,10,25))

## CN without day 1 
cn_woday1 <- subset(cn_data, days>1)
woday1_cn_results <- nparLD(cn_formula,data=cn_woday1,subject="webidnum", 
                        description=T,
                        group1.order=c(1,5,10,25))

save(woday1_poresize_results, woday1_cn_results, file='results_CNandPoresize_woday1_nparld.RData')

# CN + poresize without spider group size 1 because they contribute most of the variation
cn_wo1spider <- subset(cn_data, groupsize>1)
wo1spider_res <- nparLD(cn_formula,data=cn_wo1spider,subject="webidnum", 
                        description=T,
                        group1.order=c(5,10,25))

poresize_wo1spider <- subset(porsizedata, groupsize>1)
wo1spider_poresize_results <-  nparLD(poresize_formula,data=poresize_wo1spider,subject="webidnum", 
                                description=T,
                                group1.order=c(5,10,25))

save(wo1spider_res, wo1spider_poresize_results, file='results_CNandPoresize_wo1spider_nparld.RData')


### SUMMARY of Mean data analysis
# It doesn't matter if you remove day 1 or groupsize 1, there's still a lot of variation. This likely because 
# of the inherent outlier sensitivity of the the mean as a measure.


# MEDIAN DATA ANALYSIS 
median_data <- read.csv('median_poresize_cn.csv')
tilld5_mediandata <- subset(median_data, day<6)

results_med_poresize <- nparLD(median_poresize~day*group_size,data=tilld5_mediandata, subject="web_id", 
                                description=T,
                                group1.order=c(1,5,10,25))

results_median_CN <- nparLD(median_CN~day*group_size, data=tilld5_mediandata, subject="web_id", 
                        description=T,
                        group1.order=c(1,5,10,25))

what_was_done<-'The median data was analysed with nparLD on 23-06-2020'
results_median_CN$description <- what_was_done
results_med_poresize$description <- what_was_done

save(results_med_poresize, results_median_CN, file='results_MEDIAN_CNandPoresize_nparld.RData')


