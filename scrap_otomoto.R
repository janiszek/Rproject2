# run in terminal: java -jar selenium-server-standalone-3.0.1.jar -port 4444

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
#library(rvest)

remDr<- remoteDr(remoteServerAddr = "http://localhost",
                 port=4444,
                 browserName = "chrome",
                 newSession = TRUE)

#load all the pages and get the links on them
linkVector<-c()
for (i in 1:10){
  newUrl<-paste0("https://www.otomoto.pl/osobowe/ford/mondeo/od-2016/?search%5Bfilter_float_year%3Ato%5D=2016&search%5Bfilter_enum_fuel_type%5D=diesel&search%5Border%5D=created_at%3Adesc&page=",i)
  remDr%>%go(newUrl)
  elems<-remDr %>% findElements(using="tag name", "h2")
  for (j in 1: length(elems)){
    e<-findElementsFromElement(elems[[j]],using="tag name","a")
    if(length(e)>0){
      link<-e[[1]]%>%getElementAttribute("href")
      linkVector<-c(linkVector,link)
    }
  }
}

#remove duplicated links
length(linkVector)
linkVectorU <- linkVector%>%unique()
length(linkVectorU)


#read one record with price and details
processRecord<-function(w,vecL,remDr){
  remDr%>%go(vecL[w])
  price<-NA
  price<-remDr%>%findElement("class name","offer-price")%>%getElementAttribute("data-price")
  
  details<-remDr%>%findElements("class name","offer-params__item")
  detailListDesc<-c()
  detailListValues<-c()
  for (i in 1: length(details)){
    detailListDesc<- c(detailListDesc, details[[i]]%>%findElementsFromElement("class name", "offer-params__label"))
    detailListValues<- c(detailListValues, details[[i]]%>%findElementsFromElement("class name", "offer-params__value"))
  }
  columnNames<-lapply(detailListDesc,getElementText)%>%unlist()
  values<-lapply(detailListValues,getElementText)%>%unlist()
  
  tempDf<- data.frame(matrix(values,nrow=1,ncol=length(values)))
  names(tempDf)<-columnNames
  
  tempDf<-cbind(price,tempDf)
}


print("Total number of links:")
print(length(linkVectorU))
cars<-NULL
for(w in 1:length(linkVectorU)){
  skip<-FALSE
  print(w)
  tryCatch(
    df1<-processRecord(w,linkVectorU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(cars)){
    cars<-df1
  }else{
    cars<-smartbind(cars,df1)
  }
}
View(cars)
