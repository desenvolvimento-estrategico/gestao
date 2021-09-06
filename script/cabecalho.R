library(shiny)
library(shinydashboard)
#library(shinyBS)
#library(shinycssloaders)
#library(shinyjs)
#library(shinyWidgets)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
#library(circlepackeR)
library(data.tree)
library(ggplot2)
#library(shiny.i18n)
library(DT)
library(reactable)
library(leaflet)
library(httr)
library(viridis)
library(dashboardthemes)
library(shinycustomloader)

#BAIXANDO INFORMAÇÕES ----

url1<-'https://github.com/desenvolvimento-estrategico/gestao/raw/main/2015_-_Desenvolvimento_Estrate55.xlsx' #baixando a planilha
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx"))) #criando arquivo temporário
#separando as colunas com varias informações
chamados_fechados <- read_excel(tf)

url2<-'https://github.com/desenvolvimento-estrategico/gestao/raw/main/Chamados_de_1%C2%BA_nivel32_abertos.xlsx' #baixando a planilha
GET(url2, write_disk(tf <- tempfile(fileext = ".xlsx"))) #criando arquivo temporário
#separando as colunas com varias informações
chamados_abertos <- read_excel(tf)

chamados = merge(chamados_fechados, chamados_abertos, all = T)


chamados$MONTH = format(as.Date(chamados$`Data do registro`), "%m")
chamados$YEAR = format(as.Date(chamados$`Data do registro`), "%Y")

chamados$MONTH_F = format(as.Date(chamados$`Data de fechamento`), "%m")
chamados$YEAR_F = format(as.Date(chamados$`Data de fechamento`), "%Y")


chamados = chamados %>% mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Marketing - Ascom", "2015 - Marketing")) %>% 
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Marketing - Institucional", "2015 - Marketing")) %>% 
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Marketing - Mídias Digitais", "2015 - Marketing")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Marketing - Produtos", "2015 - Marketing")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Marketing - Arquitetura", "2015 - Marketing")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Vida", "2015 - Uni Corretora")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Administrativo", "2015 - Uni Corretora")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Acompanhamentos", "2015 - Uni Corretora")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Sinistro", "2015 - Uni Corretora")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Atendimento", "2015 - Uni Corretora")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Operacional", "2015 - Uni Corretora")) %>%
  mutate(`Grupo de operadores`=replace(`Grupo de operadores`, `Grupo de operadores`=="2015 - Uni Corretora - Renovações", "2015 - Uni Corretora"))

chamados = chamados %>% filter(`Grupo de operadores` %in% c('2015 - Uni Corretora', '2015 - Marketing', '2015 - Produtos e Negócios', '2015 - Desenvolvimento Estratégico'))

#FINAL DO CAEÇALHO----

Sys.setlocale("LC_ALL", "portuguese") #identificação do idioma