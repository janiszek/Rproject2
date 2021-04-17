# run in terminal: java -jar selenium-server-standalone-3.0.1.jar -port 4444
#install.packages(c("RSelenium","seleniumPipes","dplyr","gtools","stringr","xml2","rvest"))

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)

remDr<- remoteDr(remoteServerAddr = "http://localhost",
                 port=4444,
                 browserName = "chrome",
                 newSession = TRUE)
#sprawdzenie czy działa
remDr %>% go("https://otodom.pl/sprzedaz/mieszkanie/?page=1")

#uwaga - 1) nie da sie sciagnac strony >500, wiec trzeba filtrowac!!!
#2) dodatkowo jest problem swiezo dodawanych ogloszen - 
# wiec trzeba najpierw pobrac wszystkie linki
# dodatkowy problem - ogloszenie moze zostac wycofane, wtedy dostajemy, 
# ze ogloszenie nieaktualne (i bedzie blad, bo nie ma elementu HTML, 
# a po 3 probach aplikacja sie wywali) - trzeba zrobić obsługę błędów

wektorLinkow<-c()
for (i in 1:2){
  newUrl<-paste0("https://otodom.pl/sprzedaz/mieszkanie/?page=",i)
  #print(newUrl)
  remDr%>%go(newUrl)
  #mozna uzyc xPath,a tutaj stosujemy szukanie po tagName i po linku:
  #to siedzi w h3 i a (zbadaj link w browser)
  # - to jest oparte na Selenium Pipes
  elems<-remDr %>% findElements(using="tag name", "h3")
  #uwaga - moze byc wiecej h3 bez linku, wiec trzeba sprawdzac
  for (j in 1: length(elems)){
    # lista, wiec podwojny nawias kwadratowy
    e<-findElementsFromElement(elems[[j]],using="tag name","a")
    if(length(e)>0){
      #jesli jest link to jest ogloszeniem - specyficzne dla otodom.pl
      link<-e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}

#sprawdzamy czy dziala:
#newUrl<-paste0("https://otodom.pl/sprzedaz/mieszkanie/?page=",1)
#remDr%>%go(newUrl)
#elems<-remDr %>% findElements(using="tag name","h3")
#obiekt z Selnium Pipes
#e<-findElementsFromElement(elems[[1]],using="tag name","a")

#sprawdzamy czy sa jakies powtarzajace sie linki z uzyciem dplyr
length(wektorLinkow)
wektorlinkowU <- wektorLinkow%>%unique()
length(wektorlinkowU)


zrobWiersz<-function(w,wektorLinkowU,remDr){
#szukamy szczegolow danej strony z ogloszeniem otodom.pl
  remDr%>%go(wektorlinkowU[w])
  
  #pobieramy cene z innej klasy - tu mozemy uzyc getElementText()
  cena<-NA
  cena<-remDr%>%findElement("class name","css-srd1q3")%>%getElementText()
  
  #sciagnij pojedynczy element nazwa:wartosc, np. Powierzchnia: 25.90m2
  szczegoly<-remDr%>%findElements("class name","css-18h1kfv")
  listaSzczegolowOpis<-c()
  listaSzczegolowWartosci<-c()
  for (i in 1: length(szczegoly)){
    listaSzczegolowOpis<- c(listaSzczegolowOpis, szczegoly[[i]]%>%findElementsFromElement("class name", "css-o4i8bk"))
    listaSzczegolowWartosci<- c(listaSzczegolowWartosci, szczegoly[[i]]%>%findElementsFromElement("class name", "css-1ytkscc"))
  }
  #pobieramy teksty z elementow z funkcja lapply zamiast petli, pozbywamy sie : z nazwy i zamieniamy na wektor
  nazwyKolumn<-lapply(listaSzczegolowOpis,getElementText)%>%str_replace_all(":","")%>%unlist()
  wartosci<-lapply(listaSzczegolowWartosci,getElementText)%>%unlist()
  
  #tworzymy 1-wierszowy DataFrame, za kazdym razem innym, bo moze byc rozna ilosc kolumn dla roznych ogloszen
  df1<- data.frame(matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  
  #dodajemy cene ogloszenia
  df1<-cbind(cena,df1)
}

#sprawdzenie pobierania ze szczegolowej strony
#remDr%>%go(wektorlinkowU[1])
#pobieramy cene z innej klasy - tu mozemy uzyc getElementText()
#cena<-NA
#cenaEl<-remDr%>%findElement("class name","css-srd1q3")
#cena<-remDr%>%findElement("class name","css-srd1q3")%>%getElementText()



mieszkania<-NULL
for(w in 1:length(wektorlinkowU)){
  skip<-FALSE
  tryCatch(
    #odwolujemy sie do skip powyzej funkcji bledu <<-. 
    #Zwykly <- pracowalby na zmiennej lokalnej wewnatrz funkcji
    df1<-zrobWiersz(w,wektorlinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  #jesli sie nie udalo, to kolejny obrot petli
  if(skip){next}
#nie mozna bindowac z NULL, wiec trzeba sprawdzic i po prostu podstawic
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    #w innym przypadku laczymy - smartbind zalatwia rozne wystapienia kolumn
    mieszkania<-smartbind(mieszkania,df1)
    View(mieszkania)
  }
}


#ladowanie szczegolow druga metoda
zrobWierszRvest<-function(w,wektorLinkow,remDr){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".css-srd1q3")%>%html_text()
  #bierzemy wszystkie /div[1]/div[1] oraz /div[1]/div[2] oraz /div[2]/div[1] oraz /div[2]/div[2]
  #zamieniamy na /*/*
  v<-page %>% xml_find_all('/html/body/div[1]/main/div/div[3]/div[1]/*/*')%>%html_attr("title")%>%na.omit()
  print(v)
  indexy<-seq(1,length(v),1)
  # nazwy kolumn to indeksy niepodzielne przez 2 
  nazwyKolumn<-v[indexy%%2==1]
  # wartosci kolumn podzielne przez 2
  wartosci<-v[indexy%%2==0]
  df1<- data.frame(matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
  df1
}

#sprawdzenie drugiej metody
mieszkania<-NULL
for(w in 1:length(wektorlinkowU)){
  skip<-FALSE
  tryCatch(
    #odwolujemy sie do skip powyzej funkcji bledu <<-. 
    #Zwykly <- pracowalby na zmiennej lokalnej wewnatrz funkcji
    df1<-zrobWierszRvest(w,wektorlinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  #jesli sie nie udalo, to kolejny obrot petli
  if(skip){next}
  #nie mozna bindowac z NULL, wiec trzeba sprawdzic i po prostu podstawic
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    #w innym przypadku laczymy - smartbind zalatwia rozne wystapienia kolumn
    mieszkania<-smartbind(mieszkania,df1)
  }
}

#pobieranie linkow z uzyciem Rvest
for( i in 1:10){
  newUrl<- paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  print(newUrl)
  page <-read_html(newUrl)
  # uwaga na article[*]
  result <- page %>% html_nodes(xpath = '/html/body/div[3]/main/section[2]/div/div/div[1]/div/article[*]/div[1]/header/h3/a')
  wektorLinkow<-c(wektorLinkow,xml_attr(result, "href"))
}
