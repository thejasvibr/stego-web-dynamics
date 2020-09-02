# script which looks into te weight loss stuf :

wtadrs<-'../../analysis input-data/finalwtsnsizes.csv' # address of the weights and sizes csvfile
wts_raw<-read.csv(wtadrs)

# 2020-09-02 : realised I had left some of the colonies in the dataset with excess spiders (caused by
# spiders ballooning their way from one web to the other) - and these need to be removed. 

excess_spiders <- read.csv('excess_spiders.csv')
colonies_to_remove <- subset(excess_spiders, excess_spiders_observed==TRUE)$colonyname

#load function which removes all rows in the colonynames list/vector
source('remove_colonies.R')

wts <- remove_colonies(wts_raw, as.vector(colonies_to_remove),'colonyname')

wts<-subset(wts,wts$groupsize!='NA') # cleaning up the data frame once more to avoid an extra 'NA' group ! 
wts$wtloss<-wts$b4wt-wts$aftrwt

  
hqt<-round(quantile(wts$wtloss,0.975,na.rm=T),4)
lqt<-round(quantile(wts$wtloss,0.025,na.rm=T),4)
medn<-round(quantile(wts$wtloss,0.5,na.rm=T),4)


# now converting all the wts into milligrams with 1 decimal point accuracy - this is just to ease interpretation

wts$wtlossmg<-round(wts$wtloss*1000,1)
wts$b4wtmg<-round(wts$b4wt*1000,1)
wts$aftrwtmg<-round(wts$aftrwt*1000,1)
wts$wtlossmg<-round(wts$wtloss*1000,1)


# how much of missing data -(for Weightloss)  is there in this dataset ?
rws<-dim(wts)[1]
mssng<-sum(is.na(wts$wtloss))
pstagemiss<-mssng/rws

#are weight loss and spider size connected ? - let's check this out ! 
plot(wts$cephwid,wts$wtloss*1000,pch=20,col='red')
cor.test(wts$cephwid,wts$wtloss,method='spearman')

# however we need to be aware that each colony  was weighed after a different number of days
# let's include this data and then take a look :

#partial correlation :
source(paste(comad2,'/pcor.R',sep=''))
P0<-pcor.test(wts$wtloss,wts$cephwid,wts$wtday,method='s')

P1<-pcor.test(wts$wtloss,wts$b4wt,wts$cephwid,method='s')

# ITS BEST TO CONVERT THE WTLOSS INTO MG ...OTHERWISE IT'S A BIT CONFUSING TO INTERPRET!!

library(lme4)

# both of these results suggest that there's an influence of previous weight, and size on silk investment
# maybe it's best to do some kind of mixed model to figure this out :

M0<-lm(wtlossmg~b4wtmg+wtday+groupsize+cephwid,data=wts)

#M0 suggests that groupsize makes a difference - but this is a very simple model 

ml0<-lmer(wtlossmg~b4wtmg+wtday+groupsize+cephwid+(1|colonyname),data=wts)
#I want to check if 'spider ID' itself has an effect on wt loss !!! 
# now I've to create a mixup of two columns to assign each
wts$uniqid<-paste(wts$colonyname,wts$ID,sep='')

ml1<-lmer(wtlossmg~b4wtmg+wtday+groupsize+cephwid+(1|colonyname)+(1|uniqid),data=wts)

# This shows that there's no effect of colonyname or individual id as such 
# on the amount of weight loss, but rather that smaller sized spiders tend
# to invest more silk, and being in a good condition leads to more silk investment
# let's test for the effect of batch - this might affect the fact that
# different batches were run at different points of time.
lastchar<-function(V){substr(V,nchar(V),nchar(V))}

wts$batchid<-lastchar(as.character(wts$colonyname))

ml0_2<-lmer(wtlossmg~b4wtmg+wtday+groupsize+cephwid+(1|colonyname)+(1|batchid),data=wts)


# and so - batch ID doesn't have an effect on the weight loss...whew.

#diagnostic plots for the ml0_2 :

P0<-hist(resid(ml0_2))
P1<-plot(resid(ml0_2))

# the residual analysis looks pretty decent.

# - just checking if this is caused by some data :

#choosing a subset of cephwidths
wsbt<-subset(wts,cephwid>1.9 & cephwid<2.8)

ml0_2_testcephwids<-lmer(wtlossmg~b4wtmg+wtday+groupsize+cephwid+(1|colonyname)+(1|batchid),data=wsbt)

# --the effects still remain the same

# choosing a subset of weightloss-es

wtlsbt<-subset(wts,wtlossmg<30 & wtlossmg>1)

ml0_2_testwts<-lmer(wtlossmg~b4wtmg+wtday+groupsize+cephwid+(1|colonyname)+(1|batchid),data=wtlsbt)
plot(resid(ml0_2_testwts))

# is it the effect of the error in rouding off of the cephwid?

wtscephtst<-wts;
wtscephtst$cephwidrnd<-round(wtscephtst$cephwid,1)
ml0_2_testceph<-lmer(wtlossmg~b4wtmg+wtday+groupsize+cephwidrnd+(1|colonyname)+(1|batchid),data=wtscephtst)

# the effect of mimicking measurement error still remain the same 
wts$bodycondn<-wts$b4wtmg/wts$cephwid

ml0_2_bodycndn<-lmer(wtlossmg~bodycondn+wtday+groupsize+(1|colonyname)+(1|batchid),data=wts)

# anyway you can still calculated the %age of spiders that were re-weighed:

# number of spiders weighed
numwd<-sum(is.na(wts$b4wt)==F)
# number of spiders re-weighed
numrewd<-sum(is.na(wts$aftrwt)==F)

# percentage of spiders re-weighed: ~80%
pctgrwd<-numrewd/numwd

# -- average weight of spiders b4 :
mean(wts$b4wtmg,na.rm=T)