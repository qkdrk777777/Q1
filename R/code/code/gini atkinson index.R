devtools::install_github("qkdrk777777/DUcj",force=T)
library(DUcj)

devtools::install_github('qkdrk777777/Q1',force=T)
library(Q1)
package(ineq)
library(plyr)

#함수
ineqall<-function(i,type='Gini',plot=F,col='darkred'){
  a<-ineq(tdata[,i],type=type)
  b<-ineq(data[[1]][,i],type=type)
  c<-ineq(data[[2]][,i],type=type)
  d<-ineq(data[[3]][,i],type=type)
  e<-c(a,b,c,d);names(e)<-c('total',names(data[[1]][1]),names(data[[2]][1]),names(data[[3]][1]))
  if(plot!=F) LCplot(i,col=col)
  return(e)}
LCplot<-function(i,col='darkred')
{par(mfrow=c(2,2))
  plot(Lc(tdata[,i]),col=col)
  plot(Lc(data[[1]][,i]),col=col)
  plot(Lc(data[[2]][,i]),col=col)
  plot(Lc(data[[2]][,i]),col=col)
  par(mfrow=c(1,1))}
fix(rowdata)
rowdata$count<-1
#시작
data1<-rowdata[is.na(rowdata$시)!=T,-c(17:18)]
data2<-rowdata[is.na(rowdata$군)!=T,-c(16,18)]
data3<-rowdata[is.na(rowdata$구)!=T,-c(16:17)]
names(data1)[16]<-'구분'
names(data2)[16]<-'구분'
names(data3)[16]<-'구분'
tdata<-rbind(data1,data2,data3)

view2<-ddply(rowdata,~시+군+구,summarise,pop=sum(TOT_STDT_CNT),class=sum(TOT_CLASS_CNT),teacher=sum(TOT_TCHR_CNT),office=sum(TOT_OFWR_CNT),pop.of.class=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT),count=sum(count))
view3<-ddply(rowdata,~SD_EDU_OFFC_NM,summarise,pop=sum(TOT_STDT_CNT),class=sum(TOT_CLASS_CNT),teacher=sum(TOT_TCHR_CNT),office=sum(TOT_OFWR_CNT),pop.of.class=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT))
data<-list(view2[is.na(view2$시)!=T,c(1,4,8)],view2[is.na(view2$군)!=T,c(2,4,8)],view2[is.na(view2$구)!=T,c(3,4,8)])
for(i in 1:3)names(data[[i]])<-c("구분","pop","pop.of.class")
tdata<-rbind(data[[1]],data[[2]],data[[3]])

data<-list(view2[is.na(view2$시)!=T,c(1,4,8)],view2[is.na(view2$군)!=T,c(2,4,8)],view2[is.na(view2$구)!=T,c(3,4,8)])

#지니지수
#시군구별 학생수/학급당학생수
ineqall(i=2,plot=T)
ineqall(i=3,plot=T)

#엣킨슨 지수
ineqall(i=2,type='Atkinson')
ineqall(i=3,type='Atkinson')
#i=3학생수 기준 i=4학급당학생수 기준

#지니지수
data<-view3
i=2
#i=6
ineq(data[,i],type='Gini');plot(Lc(data[,i]),col='darkred')
ineq(data[,i],type='Atkinson')

#예측 자료(all.Rdata)->시군구별 학급당 학생수 예측
#->불평등 지수 예측
unique(all[-1,1])
for(i in 16:18)  rowdata[,i]<-as.character(rowdata[,i])
setdiff(unique(c(rowdata$시,rowdata$군,rowdata$구)),unique(rowdata$SGG_NM))
setdiff(as.character(gsub(" ","",unique(all[-1,1]))),unique(c(rowdata$시,rowdata$군,rowdata$구)))
all

##########
all[,5]-all[,4]
p2017<-all[,4]+(all[,5]-all[,4])/3

qqqq<-all[all[,1]%in%'용인시',]
fix(qqqq)

setdiff(tdata[,1],all[,1])
setdiff(all[,1],tdata[,1])

data.frame(전국=as.character(all[,1]),p2017)
