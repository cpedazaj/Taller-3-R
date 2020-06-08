
# Limpiar consola, limpiar memoria, fijar directorio y cargar paquetes
cat("\f")
rm(list = ls())
directorio <- "C:/Users/E5-471/Documents/Andes/9° semestre/R/Taller 3"
setwd(directorio)
paquetes <- c('dplyr','data.table','plyr','tidyverse','XML','rvest','xml2', 'plm','outreg','margins','ggplot2','forecast','foreign')
sapply(paquetes, require, character.only=T)

###Web scraping
# Se establece URL como objeto y se lee como objeto tipo xml_document
myurl <- "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
myhtml <- read_html(myurl)
class(myhtml)

# Usando el xpath para extraer el título de la página
myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]')

#Vemos qué tipo de objeto es y lo volvemos texto
myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% class()

texto <- myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% html_text()
texto

#Ahora se usa expath para obtener la tabla de los departamentos
myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]')

#Vemos clase y lo volvemos tabla
myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>% class()

tabla <- myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>% html_table()
View(tabla[[1]])

#Corregimos los valores con notas
tabla[[1]][34,1] = "Colombia"

tabla[[1]][34,2] = 1103

tabla[[1]][which(tabla[[1]][,8] == "1856[nota 3]???"),8] = 1856
tabla[[1]][which(tabla[[1]][,8] == "1857[nota 3]???"),8] = 1857
tabla[[1]][which(tabla[[1]][,8] == "1861[nota 3]???"),8] = 1861

tabla[[1]][which(tabla[[1]][,9] == "1886[nota 4]???"),9] = 1886
tabla[[1]][which(tabla[[1]][,9] == "1991[nota 2]???"),9] = 1991

colnames(tabla[[1]])[c(5, 7, 8)] <- c("Población (hab)", "IDH", "Fecha de creación")
View(tabla[[1]])

#Punto 2 - Regresiones

load("./datos.Rdata")
#Probabilidad lineal
linreg <- lm (formula = ocupado ~ edad + sexo + area + lee + niveleduc_s + mes, data = datos)
tablalin <- outreg(linreg)
colnames(tablalin)[3] <- "Modelo de probabilidad lineal"
tablalin

#Probit y logit
probit <- glm(formula = ocupado ~ edad + sexo + area + lee + niveleduc_s + mes, data = datos, family = binomial("probit")) 
logit <- glm(formula = ocupado ~ edad + sexo + area + lee + niveleduc_s + mes, data = datos, family = binomial("logit")) 
fitlist <- list(probit, logit)
outreg(setNames(fitlist, c("Probit", "Logit")))

#Logit por meses
meses <- list()
for (i in 1:12) {
        meses[[i]] <- glm(formula = ocupado ~ edad + sexo + area + lee + niveleduc_s, data = datos, family = binomial("logit"), subset = (mes == i))
}

outreg(setNames(meses, c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Novimbre", "Diciembre")))