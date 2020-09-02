# using nparLD :

# will study the effect of groupsize*day of the percapita data :

comad2<-'C:/Users/Thejasvi/Google Drive/stego web/web-construction FINAL round - raw data/analysis-FINALround/analysis-results/feb_2016_Rmdfiles'
# this will be done using the dlenth dataframe

# run the command below if you're running this script indepedently

#source(paste(comad2,'/typical_variable loader.R',sep=''))

# ordering the 'days'
dlord<-dlenth[order(dlenth$day,-dlenth$groupsize),]


#removing missing data: data after day 5 goes missing- and so we'll only look at
# all data from day 1-5 
dlord<-subset(dlord,day<6)

library(nparLD)

res<-nparLD(percap~day*groupsize,data=dlord,subject='colonyname',
            description=F)

# also show that the trajectories of the groups are different across time & groupsize 

wdtil6<-subset(wd,day<7)
res2<-nparLD(Lengthm~day*groupsize,data=wdtil6,subject='colonyname',description=F )
