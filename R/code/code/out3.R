library(DUcj)
package(spdep)

q[[1]]@data
coords<-coordinates(q[[1]])
par(mfrow=c(1,1),pty='s')
plot(coords)
plot(nb,coords,add=T)
nb<-knn2nb(knearneigh(coords,1))
dist<-unlist(nbdists(nb,coords))
(nb<-dnearneigh(coords,d1=0,d2=0.3*max(dist)))

nb_w<-nb2listw(neighbours=nb,zero.policy =T)
print(nb_w,zero.policy=T)
coords<-coordinates(q[[1]])
coordinates(q[[1]])<-~lon+lat

moran.test(q[[1]]@data$학교당임원수,listw=nb_w,zero.policy=T,alternative='two.sided')
del<-localmoran(q[[1]]@data[,5],listw=nb_w,zero.policy=T)
del[,4]

localG(q[[1]]@data$학급당학생수,listw=nb_w,zero.policy=T)

library(RColorBrewer)
colors =brewer.pal(5,"RdBu")

for(i in 1:nrow(del)){
  if(del[i,5]=='NaN')col[i]<-3
  else if(del[i,5]<0.01)col[i]<-1
else if(del[i,5]<0.05)col[i]<-2
#else if(del[i,5]<0.1)col[i]<-1
else col[i]<-1}
colors =brewer.pal(5,"RdBu")


#par (bg = 'transparent')
plot(q[[1]],col=colors[col])

as.numeric(rownames(del[del[,4]=='NaN',]))+1
q[[1]]@data[as.numeric(rownames(del[del[,4]=='NaN',]))+1,3]
str(del)



nb2listw(q[[1]])

resI <- localmoran(afcon$totcon, q[[1]])
printCoefmat(data.frame(resI[oid,], row.names=afcon$name[oid]),
             check.names=FALSE)
moran.test(afcon$totcon, nb2listw(paper.nb))
