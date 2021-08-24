library(rvest)
library(RSelenium)

#https://www.newtral.es/zona-verificacion/fact-check/ 

pagina_wbpg <- read_html(
  "https://www.newtral.es/alberto-nunez-feijoo-galicia-paso-a-ser-la-comunidad-autonoma-que-menos-se-endeudo-durante-los-ultimos-11-anos/20200304/"
)

pagina_wbpg %>%
html_node("title") %>% #Codigo fuente html de la etiqueta correspondiente
  html_text()
pagina_wbpg %>%
  html_nodes("p") %>% 
  html_text()
pagina_wbpg %>%
  html_nodes("time.entry-date.c-article__date.published.updated") %>%  
  html_attr("datetime")
pagina_wbpg %>%
  html_nodes("a.c-pills__item") %>%  
  html_text()
pagina_wbpg %>%
  html_nodes("source") %>%  
  html_attr("srcset")
pagina_wbpg %>%
  html_nodes("source.uk-width-1-1") %>%  
  html_attr("srcset")
pagina_wbpg %>%
  html_nodes("img") %>%  
  html_attr("src")
pagina_wbpg %>%
  html_nodes("blockquote") %>%  
  html_text()


#download.file(url = link,destfile = "test.jpg")
# pagina source
web_articulos <- read_html(
  "https://www.newtral.es/zona-verificacion/fact-check/"
)
# Tomo las URL
urls <- web_articulos %>%
  html_nodes("div.c-card__inner a") %>% #Codigo fuente html de la etiqueta
  html_attr("href")

###########################################
#########################################################################
#########################################
library(RSelenium)
library(tidyverse)

driver <- RSelenium::rsDriver(browser = "chrome",
                              chromever =
                                system2(command = "wmic",
                                        args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                        stdout = TRUE,
                                        stderr = TRUE) %>%
                                stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                magrittr::extract(!is.na(.)) %>%
                                stringr::str_replace_all(pattern = "\\.",
                                                         replacement = "\\\\.") %>%
                                paste0("^",  .) %>%
                                stringr::str_subset(string =
                                                      binman::list_versions(appname = "chromedriver") %>%
                                                      dplyr::last()) %>%
                                as.numeric_version() %>%
                                max() %>%
                                as.character())

remote_driver <- driver[["client"]] 
remote_driver$navigate("https://www.newtral.es/zona-verificacion/fact-check/")
replicate(50,
          {
            # scroll down
            webElem <- remote_driver$findElement("css", "body")
            webElem$sendKeysToElement(list(key = "end"))
            # find button
            Vermas <- remote_driver$findElement(using = 'css selector', value="div.uk-button.c-button.c-button--muted.uk-width-medium.js-gallery-trigger.o-section__row")
            # click button
            Vermas$clickElement()
            # wait
            Sys.sleep(2)
          })
web_articulos <-read_html(remote_driver$getPageSource()[[1]])
UrlsTotal <- web_articulos %>%
  html_nodes("div.c-card__inner a") %>% #Codigo fuente html de la etiqueta
  html_attr("href")


web_data_reciente <- data.frame(WebPg=UrlsTotal)


Titulo <- c()
Cuerpo <- c()
Etiquetas<- c()
Etiquetas1<- c()
Etiquetas2<- c()
Categorias<- c()
ImagenPrincipal<- c()
Imagenes<- c()
Tuits<- c()
Fecha<- c()
Fecha2<- c()
Verificador <-c()
#[2163:2364]
for(i in web_data_reciente$WebPg){ 
  #i="https://maldita.es/malditobulo/2020/03/12/el-titular-desinformador-de-diario-gol-que-sugiere-que-la-infanta-cristina-tiene-coronavirus/"
  web_articulos_reciente <- read_html(i)
  
 title <- web_articulos_reciente %>%
    html_node("title") %>%
    html_text()
  if(identical(title, character(0))){  title <-"empty"}
  Titulo <- append(Titulo, title)
  
  body <- web_articulos_reciente %>%
    html_nodes("p") %>%
    html_text()
  if(identical(body, character(0))){  body <-"empty"}
  Cuerpo <- append(Cuerpo, paste(body, collapse = " "))
  
  main_badge <- web_articulos_reciente %>%
    html_nodes("a.c-pills__item") %>%
    html_text()
  if(identical(main_badge, character(0))){  main_badge <-"empty"}
  Etiquetas1 <- append(Etiquetas1, paste(main_badge, collapse = " , "))
  
  secondary_badge <- web_articulos_reciente %>%
    html_nodes("a.c-pills__item") %>%
    html_text()
  if(identical(secondary_badge, character(0))){  secondary_badge <-"empty"}
  Etiquetas2 <- append(Etiquetas2, paste(secondary_badge, collapse = " , "))
  
  all_badges <- web_articulos_reciente %>%
    html_nodes("a.c-pills__item") %>%
    html_text()
  if(identical(all_badges, character(0))){  all_badges <-"empty"}
  Etiquetas <- append(Etiquetas, paste(all_badges, collapse = " , "))
  
  categories <- web_articulos_reciente %>%
    html_nodes("a.c-pills__item") %>%
    html_text()
  if(identical(categories , character(0))){  categories  <-"empty"}
  Categorias <- append(Categorias, paste(categories, collapse = " , "))
  
  featuredimg <- web_articulos_reciente %>%
    html_nodes("source.uk-width-1-1") %>%
    html_attr("srcset")
  if(identical(featuredimg, character(0))){  featuredimg <-"empty"}
  ImagenPrincipal <- append(ImagenPrincipal, paste(featuredimg, collapse = " , "))
  
  images <- web_articulos_reciente %>%
    html_nodes("img") %>%
    html_attr("src")
  if(identical(images, character(0))){  images <-"empty"}
  Imagenes <- append(Imagenes,paste(images, collapse = " , "))
  
  twittercite <- web_articulos_reciente %>%
    html_nodes("blockquote") %>%
    html_text()
  if(identical(twittercite, character(0))){  twittercite <-"empty"} 
  Tuits <- append(Tuits, paste(twittercite, collapse = " --- "))
  
  tiempo <- web_articulos_reciente %>%
  html_nodes("time.entry-date.c-article__date.published.updated") %>%  #Codigo fuente html de la etiqueta
    html_attr("datetime")
  if(identical(tiempo, character(0))){  tiempo <-"empty"} 
  Fecha <- append(Fecha, paste(tiempo, collapse = " , "))
  
  tiempo2 <- web_articulos_reciente %>%
    html_nodes("span.posted-on") %>%  #Codigo fuente html de la etiqueta
    html_text()
  if(identical(tiempo2, character(0))){  tiempo2 <-"empty"} 
  Fecha2 <- append(Fecha2, paste(tiempo2, collapse = " , "))
  
  Verificador <- append(Verificador,i)
  
  Sys.sleep(0.2)
}
closeAllConnections()
web_data_reciente$Titulo <- Titulo
web_data_reciente$Cuerpo <- Cuerpo
web_data_reciente$Etiquetas1 <- Etiquetas1
web_data_reciente$Etiquetas2 <- Etiquetas2
web_data_reciente$Etiquetas <- Etiquetas
web_data_reciente$Categorias <- Categorias
web_data_reciente$ImagenPrincipal <- ImagenPrincipal
web_data_reciente$Imagenes <- Imagenes
web_data_reciente$Tuits <- Tuits
web_data_reciente$Verificador <- Verificador
web_data_reciente$Fecha <- Fecha
web_data_reciente$Fecha2 <- Fecha2

web_data_reciente
#Salvar archivos en formato RDS
saveRDS(web_data_reciente, file="DataRetos_Newtral368.rds")
#Salvar en xlsx
library(openxlsx)
write.table(web_data_reciente,"ProyectoRetos_Malditaes2364.csv", sep = "\t",qmethod = "double",fileEncoding = "UTF-8",col.names = TRUE,row.names = FALSE)



######## Limpieza
#(?:\\d{2}[/.-]\\d{2}[/.-](?:\\d{4}|\\d{2})

library(stringr) 
library(dplyr)
web_data_reciente=mutate(web_data_reciente,Fecha_articulo=as.character(str_extract(web_data_reciente$Fecha ,"\\d{2,}.+,")))
web_data_reciente=mutate(web_data_reciente,Fecha_articulo=as.character(str_replace_all(web_data_reciente$Fecha_articulo ," ,","")))
#web_data_reciente=web_data_reciente[,-12]
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_squish(Cuerpo))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=tolower(TextoLimpio))#base
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_replace_all(TextoLimpio,"\\s*<u\\+\\w+>\\s*",""))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_replace_all(TextoLimpio,"\\s*<U\\+\\w+>\\s*",""))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_replace_all(TextoLimpio,"<U\\+\\w+>\\s*",""))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_replace_all(TextoLimpio,"<u\\+\\w+>\\s*",""))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_replace_all(TextoLimpio,"http\\S*|pic.twitter\\S*",""))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_replace_all(TextoLimpio,"[[:punct:]]",""))
web_data_reciente=mutate(web_data_reciente,TextoLimpio=str_squish(TextoLimpio))
