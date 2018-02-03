#폐교 년도 없는 자료 제거
data2<-data1[!is.na(as.numeric(data1[,8])),]
e.data<-data2[data2$학교급==" 초",]

colnames(e.data)

#시군구 출력
t<-addreturn(e.data,addvar=5,matrix=T)
colnames(t)<-c('시','군','구')
package(stringr)
rowdata2<-cbind(e.data,t)

#devtools::use_data(rowdata2, internal = F)
