#setwd("E:/Investigacion/UNIR")

library(rvest) 

webs=c("https://maldita.es/malditobulo/","https://maldita.es/malditobulo/page/2/")
for(i in 3:99){webs=c(webs,paste0("https://maldita.es/malditobulo/page/",i,"/")) 
}
#url <- "https://maldita.es/malditobulo/page/";webs <- paste0(url, 1:86, "/") 
#Busco todos los enlaces de cada página web
Final<-c()
for(i in 1:99){
web_articulos <- read_html(webs[i])
urls <- web_articulos %>%
    html_nodes("div.card-body a") %>% #See HTML source code for data within this tag
    html_attr("href")
Final<-c(Final,urls)
}
UrlsTotal<-unique(Final)
UrlsTotal<-UrlsTotal[-c(25,26,27,28)]#quito las url que no son noticias, lo hice manual por las pocas que había, pero cambian de posición (son las url de imágenes en la web principal)
#div.post-categories.d-flex
rm(i);rm(Final);rm(webs);rm(urls)

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
Verificador <-c()
#[2163:2364]
for(i in web_data_reciente$WebPg){ 
  #i="https://maldita.es/malditobulo/2020/03/12/el-titular-desinformador-de-diario-gol-que-sugiere-que-la-infanta-cristina-tiene-coronavirus/"
  web_articulos_reciente <- read_html(i)
  
  title <- web_articulos_reciente %>%
    html_node("h1.content-title") %>%
    html_text()
  if(identical(title, character(0))){  title <-"empty"}
  Titulo <- append(Titulo, title)

body <- web_articulos_reciente %>%
  html_nodes("div.article-body") %>%
  html_text()
if(identical(body, character(0))){  body <-"empty"}
Cuerpo <- append(Cuerpo, body)

main_badge <- web_articulos_reciente %>%
  html_nodes("span.badge.badge-dark") %>%
  html_text()
if(identical(main_badge, character(0))){  main_badge <-"empty"}
Etiquetas1 <- append(Etiquetas1, paste(main_badge, collapse = " , "))

secondary_badge <- web_articulos_reciente %>%
  html_nodes("span.badge.badge-light") %>%
  html_text()
if(identical(secondary_badge, character(0))){  secondary_badge <-"empty"}
Etiquetas2 <- append(Etiquetas2, paste(secondary_badge, collapse = " , "))

all_badges <- web_articulos_reciente %>%
  html_nodes("span.badge") %>%
  html_text()
if(identical(all_badges, character(0))){  all_badges <-"empty"}
Etiquetas <- append(Etiquetas, paste(all_badges, collapse = " , "))

categories <- web_articulos_reciente %>%
  html_nodes("div.post-categories") %>%
  html_text()
if(identical(categories , character(0))){  categories  <-"empty"}
Categorias <- append(Categorias, paste(categories, collapse = " , "))

featuredimg <- web_articulos_reciente %>%
  html_nodes("img.featured-image.img-fluid.w-100.h-auto") %>%
  html_attr("src")
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

#Salvar archivos en formato RDS
saveRDS(web_data_reciente, file="DataRetos_Malditaes2364.rds")
#Salvar en xlsx
library(openxlsx)
write.table(web_data_reciente,"ProyectoRetos_Malditaes2364.csv", sep = "\t",qmethod = "double",fileEncoding = "UTF-8",col.names = TRUE,row.names = FALSE)



######## Limpieza simple
#(?:\\d{2}[/.-]\\d{2}[/.-](?:\\d{4}|\\d{2})

library(stringr) 
library(dplyr)
web_data_reciente=mutate(web_data_reciente,Fecha_articulo=as.character(str_extract(web_data_reciente$Etiquetas2,"\\d{2,}.+")))
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



################################################################################################################################
################################################################Buscar caracteristicas de la web
####################################################################################################################

web_data_reciente2 <- data.frame(WebPg=UrlsTotal)
for(i in web_data_reciente2$WebPg){
  i="https://maldita.es/malditobulo/2020/03/11/el-pais-profesionales-sanidad-publica-menos-2010-cifras-desactualizadas-descontextualizadas/"
  #i="https://maldita.es/malditobulo/2020/03/12/murcia-clases-coronavirus-region/"
  web_articulos_reciente <- read_html(i)
  #an.error.occured <- FALSE
  #tryCatch( {  web_articulos_reciente <- read_html(i) },error = )
  #print(an.error.occured)
  
  title <- web_articulos_reciente %>%
    html_node("title") %>%
    html_text()
  if(identical(title, character(0))){  title <-"empty"}
  Titulo <- append(Titulo, title)
Sys.sleep(1)
closeAllConnections()
}
head(web_data_reciente2)
closeAllConnections()


library(stringr)
clean_text_bodies <- str_squish(
  web_data_reciente$Cuerpo
)



#####################
library(BBmisc)
library(dplyr)


Prueba<-setdiff(UrlsTotal,Eliminar)
Eliminar=c("https://maldita.es/malditobulo/2020/03/09/bulo-lysol-desinfectante-coronavirus/",
           "https://maldita.es/malditobulo/no-la-guardia-civil-no-ha-detenido-en-rivas-a-un-hombre-acusado-de-violacion-que-se-habia-escondido-en-el-maletero-de-una-mujer/",
           "https://maldita.es/malditobulo/no-vox-puenteareas-no-es-un-perfil-oficial-de-vox/",
           "https://maldita.es/malditobulo/no-hay-pruebas-de-que-este-tuit-que-habla-sobre-no-acatar-leyes-o-machacar-con-ideas-sea-de-podemos-andalucia/",
           "https://maldita.es/malditobulo/que-sabemos-sobre-el-video-de-un-patrullero-de-la-armada-espanola-pasando-frente-a-gibraltar-con-el-himno-de-espana/",
           "https://maldita.es/malditobulo/no-este-video-de-votantes-de-vox-cantando-soldadito-espanol-en-sevilla-no-es-real/",
           "https://maldita.es/malditobulo/2020/03/11/el-pais-profesionales-sanidad-publica-menos-2010-cifras-desactualizadas-descontextualizadas/")
