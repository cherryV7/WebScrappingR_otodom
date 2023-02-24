# Zebranie danych dotyczących ofert sprzedaży  miast, za pomocą web scrappingu.
# Umieszczenie zebranych danych w bazie MySQL



install.packages("RSelenium")
install.packages("rvest")
install.packages("dplyr")
install.packages("gtools")

library(gtools)
library(dplyr)
library(RSelenium)
library(rvest)

#usuwa obiekty (rozwiazanie problemu z zajetym portem):
rm(rd)
rm(remDr)

remDr$open()
rd <- RSelenium::rsDriver(browser = "chrome",chromever = "108.0.5359.71" )
remDr <- rd[['client']]
remDr$navigate(url)
Sys.sleep(1)
pageFromSelenium <- remDr$getPageSource()[[1]] %>% rvest::read_html()
przyciski <- pageFromSelenium%>%html_elements(".eoupkm71.css-190hi89.e11e36i3")
ileStron <-as.numeric(przyciski[ (length(przyciski))-1 ]%>% html_text())
wektorLinkow<-c()

for ( i in 1:ileStron){
  urll<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/bydgoszcz?areaMax=38&page=",i)
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  
  linkiElements<-(pageFromSeleniumL%>%html_elements(".css-14cy79a.e3x1uf06")) %>%
    html_nodes("a")%>%html_attr("href")
  wektorLinkow<-c(wektorLinkow,linkiElements)
}
wektorLinkow<-unique(wektorLinkow)
w<-1
miasto<-"bydgoszcz"
data<-"19.12.2022"
zrobWiersz<- function(w,wektorLinkow,miasto,data,remDr){
  urll<- paste0("https://www.otodom.pl",wektorLinkow[w])
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  cena<-pageFromSeleniumL%>%html_element(".css-8qi9av.eu6swcv19")%>%html_text()
  
  v<-pageFromSeleniumL%>%html_elements(".css-1qzszy5.estckra8")%>%html_text()
  indexy<- seq(1,length(v))
  nazwyKolumn <- v[indexy%%2==1]
  wartosci <-v[indexy%%2==0]
  
  df1<-data.frame( t(wartosci) )
  names(df1)<-nazwyKolumn
  if( !any(is.na(names(df1) )) ) {
    df1<- cbind(df1,miasto)
    df1<- cbind(df1,data=data)
    df1<-cbind(cena=cena,df1)
  }
  df1
}

BydgoszczDF<-NULL
liczbaLinkow<-length(wektorLinkow)
for( l in 1:liczbaLinkow ){
  skip<-FALSE
  tryCatch(
    temp<-zrobWiersz(l,wektorLinkow,"Bydgoszcz",data,remDr=remDr),
    error=function(e){
      print(e)
      skip<<-TRUE
    }
  )
  if(skip){next}
  print(names(temp))
  if ( !any(is.na(names(temp))) ){
    if( is.null(BydgoszczDF) )
      BydgoszczDF<-temp
    else{
      BydgoszczDF<-smartbind(BydgoszczDF,temp )
    }
  }
}
# "password"
install.packages(c("DBI","RMySQL","rstudioapi"))
library(DBI)
library(RMySQL)
library(rstudioapi)
View(BydgoszczDF)
con <- DBI::dbConnect(RMySQL::MySQL(),
                      encoding ="UTF-8",
                      host = "127.0.0.1",
                      user = "szymw",
                      dbname = "byddf",
                      password ="password123"#rstudioapi::askForPassword("Database password")
)

dbGetQuery(con,'SET NAMES utf8')
dbGetQuery(con,'set character set "utf8"')
dbWriteTable(con, "BydgoszczDF", BydgoszczDF, append = FALSE,overwrite=TRUE)

dbListTables(con)
polec<- tbl(con,"BydgoszczDF")
polec%>%select(cena)
dbDisconnect(con)
