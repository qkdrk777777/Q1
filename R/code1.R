devtools::install_github("qkdrk777777/DUcj")
library(DUcj)
package(XML)
t.school<-alldata("D:/학교논문/병아리/data/school",pattern='.xml',stringsAsFactors=F)
t.school<-t.school[(t.school[,1]!="지역구분"),]

# 초등학교데이터 생성
e.school<-t.school[t.school$SCHL_GRAD_NM=="초",]
#학생수가 결측인 자료 제거
e.school<-e.school[e.school$TOT_CLASS_CNT!="",]

#데이터 구조 변경
e.school$OPEN_DATE<-as.Date(e.school$OPEN_DATE)
e.school$year<-format(e.school$OPEN_DATE, format="%Y")
for(i in 11:15)e.school[,i]<-as.numeric(e.school[,i])


#e.school에 시, 도 값 추가
address<-strsplit(gsub("[:,:]"," ",gsub("[:?:]"," ",paste(e.school$ADDRESS))),split=" ")

del<-c("광주","대구","대전","부산","서울","울산","인천","제주")
del2<-c("광주광역시","대구광역시","대전광역시","부산광역시","서울특별시","울산광역시","인천광역시","제주자치")
for(i in 1:length(address))
  {for(j in 1:length(del)){if(address[[i]][1]==del[j])address[[i]][1]<-del2[j]}}

output=NULL
for(i in 1:length(address))
{if(address[[i]][1]=="세종특별자치시"){output<-c(output,address[[i]][1])}
  else output<-c(output,address[[i]][2])}
output

#시군구 출력
unique(regexpr("구$",output))
reg<-cbind(regexpr("시$",output),regexpr("군$",output),regexpr("구$",output))
reg<-matrix(unlist(reg),ncol=3)
colnames(reg)<-c("시","군","구")

for(j in 1:ncol(reg))
{for(i in 1:nrow(reg))
{if(reg[i,j]%in%setdiff(unique(reg[,j]),-1))reg[i,j]<-output[i]
else if(reg[i,j]==-1)reg[i,j]<-NA}}
rowdata<-cbind(e.school,reg)

#시,군,구로 추출
city<-rowdata[is.na(rowdata$시)!=T,]
county<-rowdata[is.na(rowdata$군)!=T,]
district<-rowdata[is.na(rowdata$구)!=T,]
dim(city);dim(county);dim(district)

package(plyr)
#시군구별 출력
view2<-ddply(rowdata,~시+군+구,summarise,pop=sum(TOT_STDT_CNT),class=sum(TOT_CLASS_CNT),teacher=sum(TOT_TCHR_CNT),office=sum(TOT_OFWR_CNT),pop.of.class=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT))
#연도별 공립 사립 시립별 출력
view3<-ddply(rowdata,~ESTB_DIV_NM+year,summarise,pop=sum(TOT_STDT_CNT),class=sum(TOT_CLASS_CNT),teacher=sum(TOT_TCHR_CNT),office=sum(TOT_OFWR_CNT),pop.of.class=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT))
package(rgl)
plot3d(as.factor(view3$ESTB_DIV_NM),view3$year,view3$pop.of.class,col=as.numeric(as.factor(view3$ESTB_DIV_NM)))

#사립 국립 공립 차이
#지역별 차이
setwd("S:/Users/창제/병아리/data")
write.csv(rowdata,'rowdata.csv')



