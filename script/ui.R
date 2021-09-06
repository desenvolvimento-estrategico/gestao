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
dashboardPage(skin = "green",
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
                               fluidRow(valueBoxOutput("chamados_abertos_desenvolvimento"), valueBoxOutput('chamados_fechados'), valueBoxOutput("avaliacao_media"), valueBoxOutput("tempo_medio_chamados"), valueBoxOutput("%_chamados_fechados")),
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
)
  
