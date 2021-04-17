library(rvest)

vectorLinks<-NULL
#load links using Rvest
for( i in 1:10){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/ford/mondeo/od-2016/?search%5Bfilter_float_year%3Ato%5D=2016&search%5Bfilter_enum_fuel_type%5D=diesel&search%5Border%5D=created_at%3Adesc&page=",i)
  page <-read_html(newUrl)
  # watchout for article[*]
  result <- page %>% html_nodes(xpath = '/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]/article[*]/div[2]/div[1]/div[1]/h2/a')
  vectorLinks<-c(vectorLinks,xml_attr(result, "href"))
}

length(vectorLinks)
vectorLinksU <- vectorLinks%>%unique()
length(vectorLinksU)

#loading invidual add record using Rvest
processRecordRvest<-function(w,vecL,remDr){
  newUrl<-vecL[w]
  page<-read_html(newUrl)
  price<-html_node(page,".offer-price")%>%html_attr("data-price")
  #swapping [*]/*/*
  v<-page %>% xml_find_all('/html/body/div[4]/main/div[2]/div[1]/div[2]/div[1]/div[1]/div[3]/div[1]/ul[*]/*/*')%>%html_text()%>%na.omit()%>%trimws()
  indexes<-seq(1,length(v),1)
  # column names are odd 
  columns<-v[indexes%%2==1]
  # values are even
  values<-v[indexes%%2==0]
  tempDf<- data.frame(matrix(values,nrow=1,ncol=length(values)))
  names(tempDf)<-columns
  tempDf<-cbind(price,tempDf)
  tempDf
}


print("Total number of links:")
print(length(vectorLinksU))
carsRvest<-NULL
for(w in 1:length(vectorLinksU)){
  skip<-FALSE
  print(w)
  tryCatch(
    df1<-processRecordRvest(w,vectorLinksU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(carsRvest)){
    carsRvest<-df1
  }else{
    carsRvest<-smartbind(carsRvest,df1)
  }
}
View(carsRvest)
