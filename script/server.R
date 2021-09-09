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

  # DASHBOARD SIDE BAR ----
ui = dashboardPage(skin = "green",
                   dashboardHeader(title = span(img(src='https://www.sicoobcoopvale.com.br/image/pinlogo.png', width = 40), "Desenvolvimento")),
                   dashboardSidebar(
                     sidebarMenu(
                       selectInput(inputId = "anoref", label = "Ano de referência", selected = as.vector(year(today())), 2020:year(today())),
                       selectInput(inputId = "mesref", label = "Mês de referência", selected = as.vector(month(today())), c("Janeiro" = '01',
                                                                                                             "Fevereiro" = '02',
                                                                                                             "Março" = '03',
                                                                                                             "Abril" = '04',
                                                                                                             "Maio" = '05',
                                                                                                             "Junho" = '06',
                                                                                                             "Julho" = '07',
                                                                                                             "Agosto" = '08',
                                                                                                             "Setembro" = '09',
                                                                                                             "Outubro" = '10',
                                                                                                             "Novembro" = '11',
                                                                                                             "Dezembro" = '12')
                                                                                                              ),
                       selectInput(inputId = "grupo_operador", label = "Área Responsável", c('2015 - Desenvolvimento Estratégico', '2015 - Marketing', '2015 - Produtos e Negócios', '2015 - Uni Corretora')),
                                        menuItem("Overview", tabName = 'overview', icon = icon("list")),
                                        menuItem("Chamados abertos", tabName = "chamados_abertos", icon = icon("comment")),
                                        menuItem("Avaliação de Chamados", tabName = 'avaliacao_chamados', icon = icon("list-alt"))
                       
                     )
                   ),
  #DASHBOARD BODY ------------------------------------------------
                   
                   dashboardBody(fluidPage(
                     tabItems(
                       tabItem(tabName = 'overview', h2("Detalhamento dos chamados atendidos pelo Desenvolvimento Estratégico"),
                               fluidRow(valueBoxOutput("chamados_abertos_desenvolvimento"), valueBoxOutput('chamados_fechados_mes'), valueBoxOutput('chamados_fechados'), valueBoxOutput("avaliacao_media"), valueBoxOutput("tempo_medio_chamados"), valueBoxOutput("%_chamados_fechados"), valueBoxOutput("%_chamados_3_4_5")),
                               fluidRow(wellPanel(h3("Quantidade de chamados abertos por mês"), plotlyOutput("grafico_chamados_abertos"))),
                               box(title = "Cooperativas que mais solicitaram no ano", solidHeader = TRUE, collapsible = TRUE, status = "success", plotlyOutput("grafico_soliticacao_cooperativas_ano")),
                               box(title = "Cooperativas que mais solicitaram no mês", solidHeader = TRUE, collapsible = TRUE, status = "success", plotlyOutput("grafico_soliticacao_cooperativas_mes")),
                               box(title = "10 serviços mais acionados no ano", solidHeader = TRUE, collapsible = TRUE, status = "success", plotlyOutput("grafico_categoria_chamados_ano")),
                               box(title = "10 serviços mais selecionados no mês", solidHeader = TRUE, collapsible = TRUE, status = "success", plotlyOutput("grafico_categoria_chamados_mes"))
                               ),
                       tabItem(tabName = 'chamados_abertos', h2("Detalhamento geral dos chamados em desenvolvimento"),
                               fluidRow(wellPanel(h3("Chamados Abertos"), dataTableOutput("chamados_abertos"))),
                               fluidRow(wellPanel(h3("Chamados abertos a mais de 1 mês"), dataTableOutput("chamados_atrasados")))
                         
                       ),
                       tabItem(tabName = 'avaliacao_chamados', h2("Detalhamento geral dos chamados atendidos pelo Desenvolvimento Estratégico"),
                               fluidRow(wellPanel(h3("Quantidade de chamados notas 1, 2 e 3"), dataTableOutput("chamados_nota_1_2_3"))),
                               fluidRow(wellPanel(h3("Quantidade de chamados notas 4 e 5"), dataTableOutput("chamados_nota_4_5")))
                               
                       )
                       
                     )
                     
                   )
                   )
)#fechando dashboard page


server = function(input, output){
# GESTÃO - CHAMADOS ----
  # INFO-BOXES ----
  output$chamados_abertos_desenvolvimento = renderValueBox({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(`Fechado(a)s` == F)
    value=nrow(ref)
    valueBox(value, "Chamados não fechados (em atendimento)", icon = icon("list"), color = "olive")
  })
  
  output$chamados_fechados = renderValueBox({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(`Fechado(a)s` == T)
    value=nrow(ref)
    valueBox(value, "Chamados Fechados (no ano)", icon = icon("list"), color = "olive")
  })
  
  output$chamados_fechados_mes = renderValueBox({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F==input$mesref) %>% filter(`Fechado(a)s` == T)
    value=nrow(ref)
    valueBox(value, "Chamados Fechados (no mês)", icon = icon("list"), color = "olive")
  })
  
  output$avaliacao_media = renderValueBox({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F==input$mesref) %>% drop_na(Avaliação); ref$Avaliação = as.numeric(ref$Avaliação)
    value=round(mean(ref$Avaliação),2)
    valueBox(value, "Nota Média (no mês)", icon = icon("balance-scale-right"), color = "green")
  })
  
  output$tempo_medio_chamados = renderValueBox({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F == input$mesref) %>% filter(`Fechado(a)s`==T)
    vector_days = day(ref$`Tempo de processamento efetivo`)
    vector_days = vector_days %>% replace(vector_days==31,0)
    vector_hours = hour(ref$`Tempo de processamento efetivo`)
    value = round(mean(vector_days)*(24/9)+mean(vector_hours)/9,1)
    valueBox(value, "Tempo médio até finalização do chamado (dias úteis)", icon = icon("clock"), color = "green")
  })
  
  output$`%_chamados_fechados` = renderValueBox({
    chamados_fechados = chamados %>% filter(YEAR_F==input$anoref) %>% filter(MONTH_F==input$mesref) %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(`Fechado(a)s` == T) %>% nrow()
    chamados_avaliados = chamados %>% filter(YEAR_F==input$anoref) %>% filter(MONTH_F==input$mesref) %>% filter(`Grupo de operadores`==input$grupo_operador) %>% drop_na(Avaliação) %>% nrow()
    value=paste(round((chamados_avaliados/chamados_fechados)*100,2), '%', sep = '')
    valueBox(value, "% de Chamados Avaliados (no mês)", icon = icon("percent"), color = "green")
  })
  
  output$`%_chamados_3_4_5` = renderValueBox({
    chamados_3_4_5 = chamados %>% filter(YEAR_F==input$anoref) %>% filter(MONTH_F==input$mesref) %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(`Fechado(a)s` == T) %>% filter(Avaliação>=3) %>% nrow()
    chamados_avaliados = chamados %>% filter(YEAR_F==input$anoref) %>% filter(MONTH_F==input$mesref) %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(Avaliação>=1) %>% nrow()
    value=paste(round((chamados_3_4_5/chamados_avaliados)*100,2), '%', sep '')
    valueBox(value, "% de Chamados Avaliados (no mês)", icon = icon("percent"), color = "navy")
  })
  
  
  
  # GRÁFICO DE LINHAS - CHAMADOS ABERTOS
  output$grafico_chamados_abertos = renderPlotly({ 
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR == input$anoref) %>% filter(MONTH <= as.vector(month(today()))) %>% select(MONTH)
    
    data = data.frame(table(ref))
    data$MONTH = as.vector(data$ref)
    
    data = data %>% mutate(MONTH=replace(MONTH, MONTH=="01", "Janeiro")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="02", "Fevereiro")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="03", "Março")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="04", "Abril")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="05", "Maio")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="06", "Junho")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="07", "Julho")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="08", "Agosto")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="09", "Setembro")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="10", "Outubro")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="11", "Novembro")) %>% 
      mutate(MONTH=replace(MONTH, MONTH=="12", "Dezembro"))
    
    
    data$COUNT_2 = 1:nrow(data)
    
    plot_ly(data, x = ~reorder(MONTH, COUNT_2), y = ~Freq, type = 'scatter', mode = 'lines', name = 'Chamados Abertos') %>% 
      layout(xaxis = list(title = "Meses"),
             yaxis = list (title = "Chamados")) %>% 
      add_trace(
        y=mean(data$Freq),
        name = 'Média de Chamados Abertos',
        type="scatter",
        mode="lines",
        line=list(color="red", width=3, dash="dot"),
        inherit=TRUE,
        showlegend=T)
    
    })
  
  # GRÁFICO DE BARRAS - COOPERATIVAS QUE MAIS ACIONARAM NO ANO ----
  
  output$grafico_soliticacao_cooperativas_ano = renderPlotly({
    
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F <= input$mesref)
    
    ref = aggregate(`Número do chamado`~`'Cooperativa ou Diretoria' (Solicitante)`, ref, length)
    names(ref) = c('coop', 'Quantidade')
    
    data <- data.frame(
      group=ref$coop ,
      value=ref$Quantidade
    ) %>% drop_na()
    
    data = aggregate(value~group, data, sum)
    
    
    plot_ly(data, x = ~reorder(group, -value), y = ~value, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Cooperativas"), yaxis = list(title="Quantidade"))    
    
  })
  
  # GRÁFICO DE BARRAS - COOPERATIVAS QUE MAIS ACIONARAM NO MÊS ----
  
  output$grafico_soliticacao_cooperativas_mes = renderPlotly({
    
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F == input$mesref)
    
    ref = aggregate(`Número do chamado`~`'Cooperativa ou Diretoria' (Solicitante)`, ref, length)
    names(ref) = c('coop', 'Quantidade')
    
    data <- data.frame(
      group=ref$coop ,
      value=ref$Quantidade
    ) %>% drop_na()
    
    data = aggregate(value~group, data, sum)
    
    
    plot_ly(data, x = ~reorder(group, -value), y = ~value, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Cooperativas"), yaxis = list(title="Quantidade"))    
    
  })
  
  # GRÁFICO DE SETORES (alterado para barras) - CATEGORIAS MAIS ACIONADAS NO DEPARTAMENTOS ----
  
  output$grafico_categoria_chamados_ano = renderPlotly({
    
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR == input$anoref) %>% filter(MONTH <= input$mesref)
    
    final_table=data.frame(table(ref$Subcategoria))
    
    data <- data.frame(
      group=final_table$Var1,
      value=final_table$Freq
    )
    
    
    plot_ly(data, x = ~reorder(group, -value), y = ~value, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Cooperativas", range = c(-0.5,9.5)), yaxis = list(title="Quantidade"))  
    
    
    #plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
    #  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  # GRÁFICO DE SETORES - CATEGORIAS MAIS ACIONADAS NO MÊS ----
  
  output$grafico_categoria_chamados_mes = renderPlotly({
    
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR == input$anoref) %>% filter(MONTH == input$mesref)
    
    final_table=data.frame(table(ref$Serviço))
    
    data <- data.frame(
      group=final_table$Var1,
      value=final_table$Freq
    )
    
    plot_ly(data, x = ~reorder(group, -value), y = ~value, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Cooperativas", range = c(-0.5,9.5)), yaxis = list(title="Quantidade"))  
    
    
    #plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
    #  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  #DATA TABLE - CHAMADOS NOTA 1, 2 E 3 ----
  output$chamados_nota_1_2_3 = renderDT({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F == input$mesref) %>% filter(`Fechado(a)s`==T) %>% filter(Avaliação<=3)
    final_table = data.frame(ref$`Número do chamado`, ref$`Nome do solicitante`, ref$`'Cooperativa ou Diretoria' (Solicitante)`, ref$Operador, ref$Avaliação, as.Date(ref$`Data de fechamento`))
    
    if(nrow(final_table)==0){
      final_table=data.frame(c("Não houve chamados nota 1, 2 ou 3 registrados"))
      names(final_table)=c('Projetos')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }else{
      names(final_table)=c('Chamado', 'Solicitante', 'Cliente', 'Técnico', 'Avaliação', 'Data de conclusão')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }
  })
  
  #DATA TABLE - CHAMADOS NOTA 4 e 5 ----
  output$chamados_nota_4_5 = renderDT({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR_F == input$anoref) %>% filter(MONTH_F == input$mesref) %>% filter(`Fechado(a)s`==T) %>% filter(Avaliação>=4)
    final_table = data.frame(ref$`Número do chamado`, ref$`Nome do solicitante`, ref$`'Cooperativa ou Diretoria' (Solicitante)`, ref$Operador, ref$Avaliação, as.Date(ref$`Data de fechamento`))
    
    if(nrow(final_table)==0){
      final_table=data.frame(c("Não houve chamados nota 4 ou 5 registrados"))
      names(final_table)=c('Projetos')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }else{
      names(final_table)=c('Chamado', 'Solicitante', 'Cliente', 'Técnico', 'Avaliação', 'Data de conclusão')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }
  })
  
  # DATA TABLE - CHAMADOS ABERTOS ----
  
  output$chamados_abertos = renderDT({
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR == input$anoref) %>% filter(MONTH <= input$mesref) %>% filter(`Fechado(a)s`==F)
    final_table = data.frame(ref$`Número do chamado`, ref$`Nome do solicitante`, ref$`'Cooperativa ou Diretoria' (Solicitante)`, ref$Operador, ref$Serviço, as.Date(ref$`Data do registro`))
    
    if(nrow(final_table)==0){
      final_table=data.frame(c("Não houve projetos finalizados no mês"))
      names(final_table)=c('Projetos')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                              )
              )
    }else{
      names(final_table)=c('Chamado', 'Solicitante', 'Cliente', 'Técnico', 'Serviço', 'Data de abertura')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }
  })
  
  # DATA TABLE - CHAMADOS ABERTOS A MAIS DE 1 MÊS ----
  output$chamados_atrasados = renderDT({
    chamados$ATRASADO = (today()-as.Date(chamados$`Data do registro`))
    ref = chamados %>% filter(`Grupo de operadores`==input$grupo_operador) %>% filter(YEAR == input$anoref) %>% filter(MONTH <= input$mesref) %>% filter(`Fechado(a)s`==F) %>% filter(ATRASADO>=30)
    final_table = data.frame(ref$`Número do chamado`, ref$`Nome do solicitante`, ref$`'Cooperativa ou Diretoria' (Solicitante)`, ref$Operador, ref$Serviço, as.Date(ref$`Data do registro`))
    
    if(nrow(final_table)==0){
      final_table=data.frame(c("Não houve projetos finalizados no mês"))
      names(final_table)=c('Projetos')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }else{
      names(final_table)=c('Chamado', 'Solicitante', 'Cliente', 'Técnico', 'Serviço', 'Data de abertura')
      datatable(final_table, extensions = 'Buttons',
                options = list(dom = "Blfrtip",
                               buttons = list("copy", list(extend = "collection",
                                                           buttons = c("csv", "excel", "pdf", "print"),
                                                           text = "Download")) # end of buttons customization
                               
                               # customize the length menu
                               , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                    , c(5, 10, 20, "All") # declare titles
                               ) # end of lengthMenu customization
                               , pageLength = 5
                               
                               
                )
      )
    }
  })
  
  

  
  
}#fecha chaves do server (servidor)

# Run the application 
shinyApp(ui = ui, server = server)
