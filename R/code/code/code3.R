#tot 데이터로
#경로1<-"G:/새 폴더/data/새 폴더"
#total data 생성

tot.data<-alldata(경로1,pattern='101_DT',header=T,stringsAsFactors = F)
names(tot.data)<-c("region","age","year", "pop","male","female" )
tot.data<-tot.data[,-ncol(tot.data)]
tot.data$year<-as.numeric(substr(tot.data$year,1,4))
tot.data<-tot.data[order(tot.data$year),]
#setwd('D:/packages/Q1')
#devtools::use_data(tot.data, internal = F,overwrite=T)
library(stringr)
library(plyr)

tot.data$region<-str_replace(tot.data$region," ","")
pred_fun<-function(pred1,year,real=T)
{if(real==T)temp<-c("6세","7세","8세","9세","10세","11세")
else if (real!=T)temp<-c("3세","4세","5세","6세","7세","8세")
del<-pred1[pred1$year==year,]
del<-del[del$age%in%temp,]
return(ddply(del,~region,summarise,'nchild'=sum(pop)))}
#3년뒤 예상
#q=1이면 year년의 지역별 초등학생수
#q!=1이면 year+3년 뒤의 지역별 초등학생수

#r은 year년 관련 자료 p는 year+3년 관련 자료
r_tot<-merge(merge(pred_fun(tot.data,2010),
                   pred_fun(tot.data,2013),by='region'),
             pred_fun(tot.data,2016),by='region')
colnames(r_tot)=c("region","r_2010","r_2013","r_2016")

p_tot<-merge(merge(pred_fun(tot.data,2010,real=F),
                   pred_fun(tot.data,2013,real=F),by='region'),
             pred_fun(tot.data,2016,real=F),by='region')
colnames(p_tot)=c("region","p_2010","p_2013","p_2016")

tot<-merge(r_tot,p_tot,by='region')
tot[,1]<-gsub('\\(.+\\)','',tot[,1])


#예상/실제
tot[,ncol(tot)+1]<-tot$p_2010/tot$r_2013
tot[,ncol(tot)+1]<-tot$p_2013/tot$r_2016
colnames(tot)<-c("region" ,"r_2010" ,"r_2013", "r_2016" ,"p_2010" ,"p_2013" ,"p_2016","ratio2013","ratio2016")
tot<-cbind(tot,rowMeans(tot[,8:9])*tot$p_2016)
colnames(tot)<-c("region" ,"r_2010" ,"r_2013", "r_2016" ,"p_2010" ,"p_2013" ,"p_2016","ratio2013","ratio2016","pred2019")
tot<-cbind(tot,rowMeans(tot[,8:9],tot$p_2016/tot$pred2019)*tot$pred2019)
colnames(tot)<-c("region" ,"r_2010" ,"r_2013", "r_2016" ,"p_2010" ,"p_2013" ,"p_2016","ratio2013","ratio2016","pred2019","pred2022")
tot[,-c(5:9)]

#시, 군 , 구 초등학생 수
total<-tot[gregexpr("전국$",tot$region)!=-1,]
city<-tot[gregexpr("시$",tot$region)!=-1,]
cont<-tot[gregexpr("군$",tot$region)!=-1,]
dist<-tot[gregexpr("구$",tot$region)!=-1,]
if(ncol(total)!=6)total<-total[-(5:9)]
if(ncol(city)!=6)city<-city[-(5:9)]
if(ncol(cont)!=6)cont<-cont[-(5:9)]
if(ncol(dist)!=6)dist<-dist[-(5:9)]

for(i in 1:6)  {total<-total[order(total[,i],decreasing = T),]
city<-city[order(city[,i],decreasing =T),]
cont<-cont[order(cont[,i],decreasing =T),]
dist<-dist[order(dist[,i],decreasing=T),]  }
all<-rbind(total,city,cont,dist)

#all<-cbind(region=all[,1],round(all[,2:6]))
#devtools::use_data(all, internal = F,overwrite=T)
#write.csv(all,'초등학생수예상.csv')
