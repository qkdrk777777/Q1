#' Address is City, County, Distinction.
#'
#' @param addvar = address variable
#' @return Address is City, County, Distinction.
#' @examples addreturn(e.data,addvar=5,matrix=T,del=F)
#' @export
addreturn<-function(data,addvar,add2=list(),matrix=F,a=NULL,del=F){
library(stringr)
  add<-strsplit(gsub('[,-_]','',gsub('\\(.+\\)','',gsub('\\d+',"",data[,addvar])))," ","")
  if(sum(regexpr('영통구월드컵로',add)!=-1)!=0)  add[regexpr('영통구월드컵로',add)!=-1][[1]][3]<-'영통구'
    for(i in 1:length(add)){
    add[[i]]<-add[[i]][nchar(add[[i]])!=0]
    if(sum(add[[i]]%in%'대구')!=0)add[[i]][add[[i]]%in%'대구']<-'대구 '
      if(length(unique(add[[i]][regexpr("구$",add[[i]])!=-1]))!=0)
      add2[[i]]<-unique(add[[i]][regexpr(".구$",add[[i]])!=-1]) else if(length(unique(add[[i]][regexpr("군$",add[[i]])!=-1]))!=0)
      add2[[i]]<-unique(add[[i]][regexpr('.군$',add[[i]])!=-1]) else if(length(unique(add[[i]][regexpr("시$",add[[i]])!=-1]))!=0)
      add2[[i]]<-unique(add[[i]][regexpr('.시$',add[[i]])!=-1])
  }

  if(matrix==T){
    a<-rep(NA,i)
    b<-a
    c<-a
    a1<-regexpr('구$',add2)!=-1
    a2<-regexpr('군$',add2)!=-1
    a3<-regexpr('시$',add2)!=-1
    for(i in 1:length(a))
      {if(a1[i]==T)a[i]<-add2[[i]]
      if(a2[i]==T)b[i]<-add2[[i]]
      if(a3[i]==T)c[i]<-add2[[i]]
    }
    add2<-cbind(c,b,a)
    }else add2<-unlist(add2)
  add2
}
#addreturn(e.school,addvar=5,matrix=F,del=F)


