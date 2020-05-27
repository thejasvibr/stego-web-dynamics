# file which has commands to generally setup and load the variables 
# for typical analyses :


comad<-'C:/Users/Thejasvi/Google Drive/stego web/web-construction FINAL round - raw data/analysis-FINALround/analysis-results/semifinal_16May/'
comad2<-'C:/Users/Thejasvi/Google Drive/stego web/web-construction FINAL round - raw data/analysis-FINALround/analysis-results/feb_2016_Rmdfiles'
d<-read.csv(paste(comad,'finaldataset.csv',sep=''))
wd<-subset(d,d$GridSize==1)
pixpm<-1536/.6096     # number of pixels per m 
wd$Lengthm<-wd$Length/pixpm
d$Lengthm<-d$Length/pixpm

library(ggplot2)

wddif<-by(wd$Length,wd$colonyname,diff) # apply the function for every colonyname and get the output 

# another useful function:
sbst<-function(V){substr(V,1,nchar(V)-1)}


#now to join up all these vectors:
library(qpcR);
library(reshape2);
fun<-qpcR:::cbind.na
dlength<-do.call(fun,wddif) # a columnwise data.frame with change in lengths
#convert this 'wide' formate to a 'long' format using 'melt' in reshape2
dlenth<-melt(dlength);names(dlenth)<-c('day','colonyname','dLength');

# removes the last alphabet from the colonyname
endremv<-function(nem){ if (is.na(nem)==FALSE) {as.numeric(substr(nem, 1, nchar(nem)-1))} else if (is.na(nem)==TRUE){return(NA)} } 

dlenth$groupsize<-as.numeric(lapply(as.character(dlenth$colonyname),endremv)) # and we have our final dataset:


dlenth$percap<-(dlenth$dLength/dlenth$groupsize)/pixpm # percapita
