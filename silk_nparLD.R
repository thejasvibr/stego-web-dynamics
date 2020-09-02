# using nparLD :

# will study the effect of groupsize*day of the percapita data :

load('dlenth.Rdata')
load('wd.Rdata')

part_wd<- subset(wd, day<6)
part_wd <- part_wd[order(part_wd$day, -part_wd$groupsize),]

# remove all NA rows 
dlenth <- dlenth[!is.na(dlenth['percap']),]

# ordering the 'days'
dlord<-dlenth[order(dlenth$day,-dlenth$groupsize),]


#removing missing data: data after day 5 goes missing- and so we'll only look at
# all data from day 1-5 
dlord<-subset(dlord,day<6)

library(nparLD)

res<-nparLD(percap~day*groupsize,data=dlord,subject='colonyname',
            description=F)

save(res, file='percapita_nparld_results.Rdata')

overall_silk<- nparLD(Lengthm~day*groupsize,data=part_wd,subject='colonyname',
                      description=F)

save(overall_silk, file='overallsilk_nparLD_results.Rdata')

# Show the R package environment required to recreate this analysis

ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]

print(ip, row.names=FALSE)

## The R version used to run this analysis 
print(R.version)
# also show that the trajectories of the groups are different across time & groupsize 

#wdtil6<-subset(wd,day<7)
#res2<-nparLD(Lengthm~day*groupsize,data=wdtil6,subject='colonyname',description=F )
