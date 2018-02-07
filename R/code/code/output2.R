area<-read.csv('C:\\Users\\qkdrk\\Downloads\\도시면적_시도_시_군_구__20180206135943.csv')

names(area)<-c('V11','X2016')
data1<-rowdata[is.na(rowdata$시)!=T,-c(17:18)]
data2<-rowdata[is.na(rowdata$군)!=T,-c(16,18)]
data3<-rowdata[is.na(rowdata$구)!=T,-c(16:17)]
names(data1)[16]<-'구분'
names(data2)[16]<-'구분'
names(data3)[16]<-'구분'
tdata<-rbind(data1,data2,data3)
tdata<-tdata[,-c(5,6,7,8,9,10)]
tdata$구분<-as.character(tdata$구분)
library(DUcj)
#shp파일 이용해서 매칭하기위해 수정
q<-shp_all('G:/새 폴더/병아리/SIG_201602')
q[[1]]@data$SIG_KOR_NM<-as.character(q[[1]]@data$SIG_KOR_NM)
q[[1]]@data$SIG_KOR_NM[regexpr("부천시",q[[1]]@data$SIG_KOR_NM)!=-1]<-"부천시"
q[[1]]@data$SIG_KOR_NM<-gsub(" ","",q[[1]]@data$SIG_KOR_NM)
for(i in 1:length(q[[1]]@data$SIG_KOR_NM)){
if(((regexpr('구$',q[[1]]@data$SIG_KOR_NM)!=-1)|(regexpr('군$',q[[1]]@data$SIG_KOR_NM)!=-1))[i]){
  if(regexpr('시$',q[[1]]@data$SIG_KOR_NM)[i]) q[[1]]@data$SIG_KOR_NM[i]<-gsub('시','시 ',q[[1]]@data$SIG_KOR_NM[i])
}}


tdata[,ncol(tdata)+1]<-tdata$구분
tdata[tdata[,2]%in%unique(substr(setdiff(q[[1]]@data$SIG_KOR_NM,tdata$구분),1,3)),ncol(tdata)]<-paste(
                    tdata[tdata[,2]%in%unique(substr(setdiff(q[[1]]@data$SIG_KOR_NM,tdata$구분),1,3)),2],
                    tdata[tdata[,2]%in%unique(substr(setdiff(q[[1]]@data$SIG_KOR_NM,tdata$구분),1,3)),ncol(tdata)-1])

setdiff(tdata[,11],q[[1]]@data$SIG_KOR_NM)
#강서구 고성군 부천시 북구 남구 서구 동구 중구
change<-function(input,out){
  q[[1]]@data$SIG_KOR_NM[q[[1]]@data$SIG_KOR_NM%in%input]<<-out}
change('강서구',c('서울 강서구', '부산 강서구'))
change('남구',paste(c('광주', '대구' ,'부산' ,'울산' ,'인천'),'남구'))
change('북구',paste(c('광주','대구','부산','울산'),'북구'))
change('서구',paste(c('광주','대구','대전','부산','인천'),'서구'))
change('동구',paste(c('광주','대구','대전','부산','울산','인천'),'동구'))
change('중구',paste(c('대구','대전','부산','울산','서울','인천'),'중구'))
change('고성군',paste(c('강원','경남'),'고성군'))
q[[1]]@data$SIG_KOR_NM

del<-tdata[,2]%in%c('강서구','남구','북구','서구','동구','중구','고성군')

tdata[del,11]<-paste(tdata[del,1],tdata[del,2])
unique(tdata[,11])

setdiff(tdata[,11],q[[1]]@data$SIG_KOR_NM)
tdata<-data.frame(tdata,count=1)

#수원시 안양시 부천시 고양시 안산시 부천시
#면적 자료 제외된 곳#한가하면 크롤링으로 매꾸자
library(DUcj)
package(XML)
package(stringr)
package(RCurl)
package(rvest)

a<-setdiff(unique(tdata$구분),area[,1])
url<-paste0('https://ko.wikipedia.org/wiki/',a)
for(i in 1:length(url)){
  test<-read_html(url[i])
test2<-html_nodes(test,css='.infobox')%>%html_text()
test3<-strsplit(test2,split='\n')
w<-test3[[1]][regexpr('km2',test3[[1]])!=-1]
e<-c(e,w)
}
gsub('[^0-9.]','',e)



tables <- readHTMLTable(url1)
edit(benchmark)
#####
setdiff(area[,1],tdata$구분)
area<-area[!area[,1]%in%setdiff(area[,1],tdata$구분),]

t<-merge(tdata,area,by='V11',all.x=T)
dim(t)

library(ineq)
library(plyr)
ddply(t,~V11,summarise,학교당면적=sum(count)/mean(X2016))
ineq(ddply(t,~V11,summarise,count=sum(count)/mean(X2016))[,2],'Atkinson')
plot(Lc(ddply(t,~V11,summarise,count=sum(count)/mean(X2016))[,2]))
fix(tdata)
#시군구별 평균 학교당 임원수
head(t)
colnames(t)
#시군구별 평균 학급당 학생수
out0<-ddply(t,~V11,summarise,학급당학생수=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT))
#시군구별 평균 학교당 교원수
out1<-ddply(t,~V11,summarise,학교당교원수=sum(TOT_TCHR_CNT)/sum(count))
#시군구별 평균 학교당 학급수
out2<-ddply(t,~V11,summarise,학교당학급수=sum(TOT_CLASS_CNT)/sum(count))
#시군구별 평균 교원당 학생수
out3<-ddply(t,~V11,summarise,교원당학생수=sum(TOT_STDT_CNT)/sum(TOT_TCHR_CNT))
#시군구별 면적당 학생수
out4<-ddply(t,~V11,summarise,km제곱당학생수=sum(TOT_STDT_CNT)/sum(X2016))
#시군구별 학교당 평균 면적
out5<-ddply(t,~V11,summarise,학교당평균면적=sum(X2016)/sum(count))

output<-merge(merge(merge(merge(merge(out0,out1,by='V11',all=T),out2,by='V11',all=T),out3,by='V11',all=T),out4,by='V11',all=T),out5,by='V11',all=T)
output

summary(output)
i=7
#par(mfrow=c(2,3))
#par(mfrow=c(1,1))

col<-as.numeric(cut(output[,i],seq(floor(min(output[,i],na.rm=T)),ceiling(max(output[,i],na.rm=T)),length=9)))
plot(q[[1]],col=colors[col[qqq]],main=colnames(output)[i],cex.main=2.5)


library('RColorBrewer')
colors =rev(brewer.pal(9,"YlOrRd"))
qqq<-match(q[[1]]@data$SIG_KOR_NM,output$V11)
plot(q[[1]],col=colors[col])
#사립 국립 공립 별로 차이가 있는지
