devtools::install_github("qkdrk777777/DUcj",force=T)
library(DUcj)
package(XML)

t.school<-t.school[(t.school[,1]!="��������"),]
# �ʵ��б������� ����
e.school<-t.school[t.school$SCHL_GRAD_NM=="��",]
#�л����� ������ �ڷ� ����
e.school<-e.school[e.school$TOT_CLASS_CNT!="",]
row.names(e.school)<-NULL

#������ ���� ����
e.school$OPEN_DATE<-as.Date(e.school$OPEN_DATE)
e.school$year<-format(e.school$OPEN_DATE, format="%Y")
for(i in 11:15)e.school[,i]<-as.numeric(e.school[,i])

t<-addreturn(e.school,addvar=5,matrix=T,del=F)
colnames(t)<-c('��','��','��')

rowdata<-cbind(e.school,t)


#devtools::use_data(rowdata, internal = F)

#��,��,���� ����
city<-rowdata[is.na(rowdata$��)!=T,]
county<-rowdata[is.na(rowdata$��)!=T,]
district<-rowdata[is.na(rowdata$��)!=T,]
dim(city);dim(county);dim(district)



