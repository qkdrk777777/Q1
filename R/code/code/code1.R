devtools::install_github("qkdrk777777/DUcj",force=T)
library(DUcj)
package(XML)

t.school<-t.school[(t.school[,1]!="지역구분"),]
# 초등학교데이터 생성
e.school<-t.school[t.school$SCHL_GRAD_NM=="초",]
#학생수가 결측인 자료 제거
e.school<-e.school[e.school$TOT_CLASS_CNT!="",]
row.names(e.school)<-NULL

#데이터 구조 변경
e.school$OPEN_DATE<-as.Date(e.school$OPEN_DATE)
e.school$year<-format(e.school$OPEN_DATE, format="%Y")
for(i in 11:15)e.school[,i]<-as.numeric(e.school[,i])

t<-addreturn(e.school,addvar=5,matrix=T,del=F)
colnames(t)<-c('시','군','구')

rowdata<-cbind(e.school,t)


#devtools::use_data(rowdata, internal = F)

#시,군,구로 추출
city<-rowdata[is.na(rowdata$시)!=T,]
county<-rowdata[is.na(rowdata$군)!=T,]
district<-rowdata[is.na(rowdata$구)!=T,]
dim(city);dim(county);dim(district)




