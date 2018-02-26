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
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(q[[1]])<-CRS(from.crs)
q[[1]]<-spTransform(q[[1]],CRS(to.crs))



#q<-readShapePoints('G:/새 폴더/병아리/SIG_201602/TL_SCCO_SIG.shp'),
                 #proj4string=CRS('+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs'))

#q@data
#edit(shp_all)
q[[1]]@data$SIG_KOR_NM<-as.character(q[[1]]@data$SIG_KOR_NM)
q[[1]]@data$SIG_KOR_NM[regexpr("부천시",q[[1]]@data$SIG_KOR_NM)!=-1]<-"부천시"
q[[1]]@data$SIG_KOR_NM<-gsub(" ","",q[[1]]@data$SIG_KOR_NM)
for(i in 1:length(q[[1]]@data$SIG_KOR_NM)){
if(((regexpr('구$',q[[1]]@data$SIG_KOR_NM)!=-1)|(regexpr('군$',q[[1]]@data$SIG_KOR_NM)!=-1))[i]){
  if(regexpr('시$',q[[1]]@data$SIG_KOR_NM)[i]) q[[1]]@data$SIG_KOR_NM[i]<-gsub('시','시 ',q[[1]]@data$SIG_KOR_NM[i])
}}


tdata[,ncol(tdata)+1]<-tdata$구분
aa<-setdiff(q[[1]]@data$SIG_KOR_NM,area[,1])
aa<-aa[order(aa)]
setdiff(area[,1],tdata[,11])
setdiff(tdata[,11],area[,1])
tdata[tdata[,2]%in%unique(substr(setdiff(q[[1]]@data$SIG_KOR_NM,tdata$구분),1,3)),ncol(tdata)]<-paste(
                    tdata[tdata[,2]%in%unique(substr(setdiff(q[[1]]@data$SIG_KOR_NM,tdata$구분),1,3)),2],
                    tdata[tdata[,2]%in%unique(substr(setdiff(q[[1]]@data$SIG_KOR_NM,tdata$구분),1,3)),ncol(tdata)-1])


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
#####
setdiff(unique(tdata$구분),unique(area[,1]))


area<-area[!area[,1]%in%setdiff(area[,1],tdata$V11),]
colnames(area)<-c('V11','X2016')

t<-merge(tdata,area,by='V11',all=T)
dim(t)
library(ineq)
library(plyr)
#########
ww<-ddply(t,~V11,summarise,학교당면적=sum(count)/mean(X2016))
aa<-ww[is.na(ww[,2]),1]
a<-c('고성군_(남)','고성군_(경상남도)','덕양구','일산동구','일산서구','남구_(광주광역시)','동구_(광주광역시)',
     '북구_(광주광역시)','서구_(광주광역시)','남구_(대구광역시)','동구_(대구광역시)','북구_(대구광역시)','서구_(대구광역시)',
     '중구_(대구광역시)','동구_(대전광역시)','서구_(대전광역시)','중구_(대전광역시)','강서구_(부산광역시)','남구_(부산광역시)',
     '동구_(부산광역시)','북구_(부산광역시)','서구_(부산광역시)','중구_(부산광역시)','서귀포시','강서구_(서울특별시)','중구_(서울특별시)',
     '분당구','수정구','중원구','권선구','영통구','장안구','팔달구','단원구','상록구','동안구','만안구','기흥구','수지구','처인구',
     '남구_(울산광역시)','동구_(울산광역시)','북구_(울산광역시)','중구_(울산광역시)','남구_(인천광역시)','동구_(인천광역시)','서구_(인천광역시)',
     '중구_(인천광역시)','덕진구','완산구','제주시','마산합포구','마산회원구','성산구','의창구','진해구','동남구','서북구','상당구',
     '서원구','청원구','흥덕구','남구_(포항시)','북구_(포항시)')

library(DUcj)
package(XML)
package(stringr)
package(RCurl)
package(rvest)

x<-NULL
#a<-setdiff(unique(tdata$구분),area[,1])
url<-paste0('https://ko.wikipedia.org/wiki/',a)
for(i in 1:length(url)){
  test<-read_html(url[i])
  test2<-html_nodes(test,css='.infobox')%>%html_text()
  test3<-strsplit(test2,split='\n')
  w<-test3[[1]][regexpr('km2',test3[[1]])!=-1]
  w<-gsub('[^0-9.]','',w)
  if(a[i]%in%'마산회원구')w=90.58
  x=c(x,w)
}
x
x<-cbind(aa,as.numeric(x))
colnames(x)<-colnames(area)

area<-rbind(area,x)
area[,2]<-as.integer(area[,2])

t<-merge(tdata,area,by='V11',all=T)

names(t)
sum(is.na(t[,6])==T)
sum(is.na(t[,7])==T)
sum(is.na(t[,8])==T)
sum(is.na(t[,9])==T)
length(unique(t[is.na(t[,9]),3]))
#############
summary(t)

#결측치 제거하고 볼 때 사용
t<-(t[(is.na(t$TOT_OFWR_CNT)|is.na(t$TOT_TCHR_CNT))!=T,])
#시군구별 평균 학교당 임원수
out0<-ddply(t,~V11,summarise,학교당임원수=round(sum(TOT_OFWR_CNT)/sum(count)))
#시군구별 평균 학급당 학생수
out1<-ddply(t,~V11,summarise,학급당학생수=round(sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT)))
#시군구별 평균 학교당 교원수
out2<-ddply(t,~V11,summarise,학교당교원수=round(sum(TOT_TCHR_CNT)/sum(count)))
#시군구별 평균 학교당 학급수
out3<-ddply(t,~V11,summarise,학교당학급수=round(sum(TOT_CLASS_CNT)/sum(count)))
#시군구별 평균 교원당 학생수
out4<-ddply(t,~V11,summarise,교원당학생수=round(sum(TOT_STDT_CNT)/sum(TOT_TCHR_CNT)))
#시군구별 면적당 학생수
out5<-ddply(t,~V11,summarise,km제곱당학생수=round(sum(TOT_STDT_CNT)/sum(X2016)))
#시군구별 평균 학급당 임원수
out6<-ddply(t,~V11,summarise,학급당임원수=round(sum(TOT_OFWR_CNT)/sum(TOT_CLASS_CNT)))
#시군구별 학교당 평균 면적
out7<-ddply(t,~V11,summarise,학교당평균면적=round(sum(X2016)/sum(count)))

output<-merge(merge(merge(merge(merge(merge(merge(out0,out1,by='V11',all=T),out2,by='V11',all=T),out3,by='V11',all=T),out4,by='V11',all=T),out5,by='V11',all=T),out6,by='V11',all=T),out7,by='V11',all=T)

summary(output)


#par(mfrow=c(2,3))
#par(mfrow=c(1,1))

library('RColorBrewer')
#plot(q[[1]],col=colors[col])
pp<-function(i,legend=F,nolocator=F){
  colors =rev(brewer.pal(9,"YlOrRd"))
  qqq<-match(q[[1]]@data$SIG_KOR_NM,output$V11)
  col<-as.numeric(cut(output[,i],seq(floor(min(output[,i],na.rm=T)),ceiling(max(output[,i],na.rm=T)),length=10)))
  par(bg='transparent')
  plot(q[[1]],col=colors[col[qqq]],main=colnames(output)[i],cex.main=2.5)
tt<-seq(floor(min(output[,i],na.rm=T)),ceiling(max(output[,i],na.rm=T)),length=9)
tttt<-NULL
for(i in 2:length(tt)-1)
{ttt<-paste0(tt[i],'~',tt[i+1])
tttt<-c(tttt,ttt)}
tttt
if(legend){
  if(nolocator){p<-c(37.25,129.62)
  legend(p[2],p[1],legend=tttt,col=colors,pch=16,cex=1,bty='n')
  }else {p<-locator(1)
legend(p,legend=tttt,col=colors,pch=16,cex=1,bty='n')}}
}
par(mfrow=c(1,1))
#pp(2,legend=T,nolocator=T)

#############지니 엣킨슨
#sum(t[t$V11%in%'강남구',12])
#ineq(t[t$V11%in%'강남구',9]/t[t$V11%in%'강남구',12])
#dim(t)
#시군구별 학교당 임원수의 불평등지수

out0<-ddply(t,~V11,summarise,학교당임원수_지니=round(1000*ineq((TOT_OFWR_CNT)/(count))))#,type='Atkinson'))
#시군구별 평균 학급당 학생수의 불평등지수
out1<-ddply(t,~V11,summarise,학급당학생수_지니=round(1000*ineq((TOT_STDT_CNT)/(TOT_CLASS_CNT))))#,type='Atkinson'))
#시군구별 평균 학교당 교원수의 불평등지수
out2<-ddply(t,~V11,summarise,학교당교원수_지니=round(1000*ineq((TOT_TCHR_CNT)/(count))))#,type='Atkinson'))
#시군구별 학교당 학급수의 불평등지수
out3<-ddply(t,~V11,summarise,학교당학급수_지니=round(1000*ineq((TOT_CLASS_CNT)/(count))))#,type='Atkinson'))
#시군구별 교원당 학생수의 불평등지수
out4<-ddply(t,~V11,summarise,교원당학생수_지니=round(1000*ineq((TOT_STDT_CNT)/(TOT_TCHR_CNT))))#,type='Atkinson'))
#시군구별 면적당 학생수의 불평등지수
out5<-ddply(t,~V11,summarise,km제곱당학생수_지니=round(1000*ineq((TOT_STDT_CNT)/(X2016))))#,type='Atkinson'))
#시군구별 학급당 임원수의 불평등지수
out6<-ddply(t,~V11,summarise,학급당임원수_지니=round(1000*ineq((TOT_OFWR_CNT)/(TOT_CLASS_CNT))))#,type='Atkinson'))
#시군구별 학교당 평균 면적
out7<-ddply(t,~V11,summarise,학교당평균면적=round(sum(X2016)/sum(count)))

output2<-merge(merge(merge(merge(merge(merge(merge(out0,out1,by='V11',all=T),out2,by='V11',all=T),out3,by='V11',all=T),out4,by='V11',all=T),out5,by='V11',all=T),out6,by='V11',all=T),out7,by='V11',all=T)
summary(output2)
pp2<-function(i,legend=F,nolocator=F){
  colors =(brewer.pal(9,"YlOrRd"))
  qqq<-match(q[[1]]@data$SIG_KOR_NM,output2$V11)
  col<-as.numeric(cut(output2[,i],seq(floor(min(output2[,i],na.rm=T)),ceiling(max(output2[,i],na.rm=T)),length=10)))
  par(bg='transparent')
  plot(q[[1]],col=colors[col[qqq]],main=colnames(output2)[i],cex.main=2.5)
  tt<-seq(floor(min(output2[,i],na.rm=T)),ceiling(max(output2[,i],na.rm=T)),length=9)
  tttt<-NULL
  for(i in 2:length(tt)-1)
  {ttt<-paste0(tt[i],'~',tt[i+1])
  tttt<-c(tttt,ttt)}
  tttt
  if(legend){
    if(nolocator){p<-c(37.25,129.62)
    legend(p[2],p[1],legend=tttt,col=colors,pch=16,cex=1,bty='n')
    }else {p<-locator(1)
    legend(p,legend=tttt,col=colors,pch=16,cex=1,bty='n')}}
}
summary(output2)


############
#엣킨슨

out0<-ddply(t,~V11,summarise,학교당임원수_앳킨슨=round(1000*ineq((TOT_OFWR_CNT)/(count),type='Atkinson')))
#시군구별 평균 학급당 학생수의 불평등지수
out1<-ddply(t,~V11,summarise,학급당학생수_앳킨슨=round(1000*ineq((TOT_STDT_CNT)/(TOT_CLASS_CNT),type='Atkinson')))
#시군구별 평균 학교당 교원수의 불평등지수
out2<-ddply(t,~V11,summarise,학교당교원수_앳킨슨=round(1000*ineq((TOT_TCHR_CNT)/(count),type='Atkinson')))
#시군구별 학교당 학급수의 불평등지수
out3<-ddply(t,~V11,summarise,학교당학급수_앳킨슨=round(1000*ineq((TOT_CLASS_CNT)/(count),type='Atkinson')))
#시군구별 교원당 학생수의 불평등지수
out4<-ddply(t,~V11,summarise,교원당학생수_앳킨슨=round(1000*ineq((TOT_STDT_CNT)/(TOT_TCHR_CNT),type='Atkinson')))
#시군구별 면적당 학생수의 불평등지수
out5<-ddply(t,~V11,summarise,km제곱당학생수_앳킨슨=round(1000*ineq((TOT_STDT_CNT)/(X2016),type='Atkinson')))
#시군구별 학급당 임원수의 불평등지수
out6<-ddply(t,~V11,summarise,학급당임원수_앳킨슨=round(1000*ineq((TOT_OFWR_CNT)/(TOT_CLASS_CNT),type='Atkinson')))
#시군구별 학교당 평균 면적
out7<-ddply(t,~V11,summarise,학교당평균면적=round(sum(X2016)/sum(count)))

output3<-merge(merge(merge(merge(merge(merge(merge(out0,out1,by='V11',all=T),out2,by='V11',all=T),out3,by='V11',all=T),out4,by='V11',all=T),out5,by='V11',all=T),out6,by='V11',all=T),out7,by='V11',all=T)
summary(output3)
pp3<-function(i,legend=F,nolocator=F){
  colors =(brewer.pal(9,"YlOrRd"))
  qqq<-match(q[[1]]@data$SIG_KOR_NM,output3$V11)
  col<-as.numeric(cut(output3[,i],seq(floor(min(output3[,i],na.rm=T)),ceiling(max(output3[,i],na.rm=T)),length=10)))
  par(bg='transparent')
  plot(q[[1]],col=colors[col[qqq]],main=colnames(output3)[i],cex.main=2.5)
  tt<-seq(floor(min(output3[,i],na.rm=T)),ceiling(max(output3[,i],na.rm=T)),length=9)
  tttt<-NULL
  for(i in 2:length(tt)-1)
  {ttt<-paste0(tt[i],'~',tt[i+1])
  tttt<-c(tttt,ttt)}
  tttt
  if(legend){
    if(nolocator){p<-c(37.25,129.62)
    legend(p[2],p[1],legend=tttt,col=colors,pch=16,cex=1,bty='n')
    }else {p<-locator(1)
    legend(p,legend=tttt,col=colors,pch=16,cex=1,bty='n')}}
}
summary(output3)


#####
png('1.png')
par(mfrow=c(1,3),bg='transparent')
##plot
pp(2,legend=T,nolocator=T)
pp2(2,legend=T,nolocator=T)
pp3(2,legend=T,nolocator=T)
dev.off()
png('2.png')
par(mfrow=c(1,3),bg='transparent')
pp(3,legend=T,nolocator=T)
pp2(3,legend=T,nolocator=T)
pp3(3,legend=T,nolocator=T)
dev.off()
png('3.png')
par(mfrow=c(1,3),bg='transparent')
pp(4,legend=T,nolocator=T)
pp2(4,legend=T,nolocator=T)
pp3(4,legend=T,nolocator=T)
dev.off()
png('4.png')
par(mfrow=c(1,3),bg='transparent')
pp(5,legend=T,nolocator=T)
pp2(5,legend=T,nolocator=T)
pp3(5,legend=T,nolocator=T)
dev.off()
png('5.png')
par(mfrow=c(1,3),bg='transparent')
pp(6,legend=T,nolocator=T)
pp2(6,legend=T,nolocator=T)
pp3(6,legend=T,nolocator=T)
dev.off()
png('6.png')
par(mfrow=c(1,3),bg='transparent')
pp(7,legend=T,nolocator=T)
pp2(7,legend=T,nolocator=T)
pp3(7,legend=T,nolocator=T)
dev.off()
png('7.png')
par(mfrow=c(1,3),bg='transparent')
pp(8,legend=T,nolocator=T)
pp2(8,legend=T,nolocator=T)
pp3(8,legend=T,nolocator=T)
dev.off()
png('8.png',bg='transparent')
par(mfrow=c(1,1),bg='transparent')
pp(9,legend=T,nolocator=T)
dev.off()

legend(locator(1),legend=paste0('불평등지수\n\n',round(ineq(out7[,2]),5),'\n',round(ineq(out7[,2],type='Atkinson'),5)),bty='n')
fix(rowdata)
head(rowdata2,3)
dim(rowdata2)
colnames(output)
i=2
png('10.png')
par(mfrow=c(2,4),bg='transparent',pty='s',mar=c(4,2,2,3))
for(i in 2:9)
plot(Lc(output[,i]),col='darkred',cex.main=1,main=paste(colnames(output)[i],'로렌츠곡선'),bty='l')
dev.off()
output
output2
setwd('D://')
write.csv(output,'output1.csv')
write.csv(output2,'output2.csv')
write.csv(output3,'output3.csv')
################
del<-coordinates(q[[1]])
colnames(del)<-c('lon','lat')
del2<-data.frame(del,q[[1]]@data$SIG_KOR_NM)
colnames(del2)<-c(colnames(del),'SIG_KOR_NM')

#q[[1]]@data$SIG_KOR_NM<-as.factor(q[[1]]@data$SIG_KOR_NM)
setwd('C:\\Users\\qkdrk\\Desktop\\satscan')
getwd()

#q[[1]]@data<-q[[1]]@data[,-c(5:ncol(q[[1]]@data))]
q[[1]]@data<-data.frame(q[[1]]@data,output[match(q[[1]]@data$SIG_KOR_NM,output$V11),])
#q[[1]]@data<-data.frame(q[[1]]@data,del2[match(q[[1]]@data$SIG_KOR_NM,del2$SIG_KOR_NM),])
q[[1]]@data<-cbind(q[[1]]@data,pop=1000)
for(i in 6:14)
  q[[1]]@data[,i]<-as.integer(q[[1]]@data[,i])
writeSpatialShape(q[[1]],'out1.shp')
str(q[[1]]@data)
del3<-q[[1]]@data
q[[1]]@data<-q[[1]]@data[,-c(5:ncol(q[[1]]@data))]
q[[1]]@data<-data.frame(q[[1]]@data,output2[match(q[[1]]@data$SIG_KOR_NM,output2$V11),])
q[[1]]@data<-data.frame(q[[1]]@data,del2[match(q[[1]]@data$SIG_KOR_NM,del2$SIG_KOR_NM),])
q[[1]]@data<-cbind(q[[1]]@data,pop=1000)
for(i in 6:13)
  q[[1]]@data[,i]<-as.integer(q[[1]]@data[,i])
writeSpatialShape(q[[1]],'out2.shp')

q[[1]]@data<-q[[1]]@data[,-c(5:ncol(q[[1]]@data))]
q[[1]]@data<-data.frame(q[[1]]@data,output3[match(q[[1]]@data$SIG_KOR_NM,output3$V11),])
q[[1]]@data<-data.frame(q[[1]]@data,del2[match(q[[1]]@data$SIG_KOR_NM,del2$SIG_KOR_NM),])
q[[1]]@data<-cbind(q[[1]]@data,pop=1000)
for(i in 6:13)
  q[[1]]@data[,i]<-as.integer(q[[1]]@data[,i])
writeSpatialShape(q[[1]],'out3.shp')
