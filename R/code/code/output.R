library(plyr)
view2<-ddply(rowdata,~SD_EDU_OFFC_NM+시+군+구,summarise,pop=sum(TOT_STDT_CNT),class=sum(TOT_CLASS_CNT),teacher=sum(TOT_TCHR_CNT),office=sum(TOT_OFWR_CNT),pop.of.class=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT))
view3<-ddply(rowdata,~ESTB_DIV_NM+year,summarise,pop=sum(TOT_STDT_CNT),class=sum(TOT_CLASS_CNT),teacher=sum(TOT_TCHR_CNT),office=sum(TOT_OFWR_CNT),pop.of.class=sum(TOT_STDT_CNT)/sum(TOT_CLASS_CNT))
library(rgl)
plot3d(as.factor(view3$ESTB_DIV_NM),view3$year,view3$pop.of.class,col=as.numeric(as.factor(view3$ESTB_DIV_NM)))

#시,군, 구별 평균반 학생수
output1<-view2[is.na(view2[,2])==F,c(1,2,5,9)][order(view2[is.na(view2[,2])==F,c(1,2,5,9)][,3],decreasing=T),]
colnames(output1)<-c( "SD_EDU_OFFC_NM", "구분" ,"pop","pop.of.class" )
output2<-view2[is.na(view2[,3])==F,c(1,3,5,9)][order(view2[is.na(view2[,3])==F,c(1,3,5,9)][,3],decreasing=T),]
colnames(output2)<-c( "SD_EDU_OFFC_NM", "구분" ,"pop","pop.of.class" )
output3<-view2[is.na(view2[,4])==F,c(1,4,5,9)][order(view2[is.na(view2[,4])==F,c(1,4,5,9)][,3],decreasing=T),]
colnames(output3)<-c( "SD_EDU_OFFC_NM", "구분" ,"pop","pop.of.class" )
#정렬
for(i in 3:4){
output1<-output1[order(output1[,i],decreasing = T),]
output2<-output2[order(output2[,i],decreasing = T),]
output3<-output3[order(output3[,i],decreasing = T),]}

output4<-rbind(output1,output2,output3)
output4[,4]<-round(output4[,4],2)
output4
dim(output4)
#write.csv(output4,'행정구역별 평균 반 학생수.csv')
##########################
#지역별 연도별 폐교현황
rowdata2[,ncol(rowdata2)+1]<-1
colnames(rowdata2)
#연도별 폐교현황
plot(ddply(rowdata2,~폐교년도,summarise,count=sum(V21)),type='l')
#지역별 연도별 폐교현황
#시
  del1<-ddply(rowdata2,~시+폐교년도,summarise,count=sum(V21))
  del1<-del1[is.na(del1[,1])!=T,]
  del_city<-ddply(del1,~시,summarise,count=sum(count))[order(ddply(del1,~시,summarise,count=sum(count))[,2],decreasing=T),]
  colnames(del1)<-c('구분','폐교년도','count')
#군
  del2<-ddply(rowdata2,~군+폐교년도,summarise,count=sum(V21))
  del2<-del2[is.na(del2[,1])!=T,]
  del_cont<-ddply(del2,~군,summarise,count=sum(count))[order(ddply(del2,~군,summarise,count=sum(count))[,2],decreasing=T),]
  del2[order(del2[,3],decreasing=T),]
  colnames(del2)<-c('구분','폐교년도','count')
#구
  del3<-ddply(rowdata2,~구+폐교년도,summarise,count=sum(V21))
  del3<-del3[is.na(del3[,1])!=T,]
  del_dist<-ddply(del3,~구,summarise,count=sum(count))[order(ddply(del3,~구,summarise,count=sum(count))[,2],decreasing=T),]
  del3[order(del3[,3],decreasing =T),]
  colnames(del3)<-c('구분','폐교년도','count')

#시군구 2010~2016 폐교년도
  del4<-ddply(rowdata2,~시+군+구+폐교년도,summarise,count=sum(V21))
  del5<-del4[order(del4[,4],decreasing = T),][del4[order(del4[,4],decreasing = T),][,4]%in%seq(2010,2022,3),]

  for(i in c(4,5,3:1)){
    if(i==5)desc=T else desc=F
del5<-del5[order(del5[,i],decreasing=T),]}

#write.csv(del5,'년도별 시군구 폐교년도.csv')
##########################

#전국, 시, 군, 구 초등학생 수

#pdf('output.pdf')
plot(ddply(rowdata2,~폐교년도,summarise,count=sum(V21)),type='l')
plot(seq(2010,2022,3),total[1,2:6],type='l',main='Number of elementary students')
plot(seq(2010,2022,3),city[1,2:6],type='l',main='시별 평균 초등학생수')
plot(seq(2010,2022,3),cont[1,2:6],type='l',main='평균 군의 초등학생수')
plot(seq(2010,2022,3),dist[1,2:6],type='l',main='평균 구의 초등학생수')
#dev.off()

package(plotly)
del5$폐교년도<-as.factor(del5$폐교년도)

p1<-plot_ly(del4,x=~시, y=~폐교년도,z=~count)
p2<-plot_ly(del5,x=~시,y=~폐교년도,z=~count,color=~폐교년도,colors=c("#0000FF",  "#6478FF"))
p2

#all2<-all[-1,]
#plot_ly(all2,x=~region,y=~1:nrow(all2),z=~r_2010)#+r_2013+r_2016+pred2019+pred2022)
x<-rep(all$region,each=5)
y<-rep(seq(2010,2022,3),nrow(all))
temp<-NULL
for(i in 1:nrow(all))
temp<-c(temp,all$r_2010[i],all$r_2013[i],all$r_2016[i],all$pred2019[i],all$pred2022[i])

plotdata<-data.frame(x,y,temp)
plotdata$y<-as.factor(plotdata$y)

#plotdata<-plotdata[-c(1:5),]
plot_ly(plotdata, x=~x, y=~y, z=~temp,alpha=0.5,color=~y)#,colors=c("#0000FF",  "#6478FF"))
###################
#쓰래기코드

#install.packages('rgl')
library(rgl)
?writeWebGL
browseURL(paste("d://3dplot.html", writeWebGL(dir=file.path(tempdir(), "webGL"), width=500), sep=""))
browseURL("d:/index.html"          )
browseURL ( "https://www.r-project.org")

plot3d(rnorm(100), rnorm(100), rnorm(100), type = "s", col = "red")
plot3d(1)
# This writes a copy into temporary directory 'webGL', and then displays it

setwd("C:/Users/Public/Documents/ESTsoft/CreatorTemp/RtmpiE6DIE")
filename<-writeWebGL(file.path(tempdir(),list.files(pattern='view')))

filename <- writeWebGL(dir = file.path(tempdir(), "webGL"),
                       width = 500, reuse = TRUE)
# Display the "reuse" attribute
attr(filename, "reuse")

# Display the scene in a browser
if (interactive())  browseURL(paste0(filename))


open3d (x <- sort (rnorm (1000)),y <- rnorm (1000) ,z <- rnorm )


?rgl.postscript ( "persp3dd.pdf", "pdf")
x <- y <- seq(-10, 10, length = 20)
z <- outer(x, y, function(x, y) x^2 + y^2)
persp3d(x, y, z, col = 'lightblue')

title3d("Using LaTeX text", col = 'red', line = 3)
#rgl.postscript("persp3da.ps", "ps", drawText = FALSE)
rgl.postscript("persp3da.pdf", "pdf", drawText = FALSE)
rgl.postscript("persp3da.tex", "tex")
rgl.pop()
title3d("Using ps/pdf text", col = 'red', line = 3)
rgl.postscript("persp3db.ps", "ps")
rgl.postscript("persp3db.pdf", "pdf")
rgl.postscript("persp3db.tex", "tex", drawText = FALSE)
rgl.snapshot('3dplot.png', fmt='png')
