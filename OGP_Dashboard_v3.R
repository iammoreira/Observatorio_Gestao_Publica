#########################################################################################
# DASHBOARD 
#########################################################################################

# ---------------------------------------------------------------- #
# Preâmbulo
# ---------------------------------------------------------------- #

# Atualizado em 29/09/2025.

# Diretório de trabalho
#setwd("C:\\Users\\Ingrid M\\OneDrive\\Documentos\\UFPA\\Projetos de Extensão\\Observatório da Gestão Pública de Benevides-PA\\Compartilhado")


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)

library(dplyr)
library(tidyr)
library(tidyverse)

library(DT)

library(ggplot2)
library(plotly)
library(geobr)
library(wesanderson)

library(htmltools)
library(leaflet)
library(sf)
library(readODS)
library(fresh)

# Leitura de dados
#dados_inep = read_ods("base_completa_educ_2011_2024.ods")

# ---------------------------------------------------------------- #
# Interface do Usuário (User Interface)
# ---------------------------------------------------------------- #
ui <- dashboardPage(
  title = "Relatório de Gestão - Benevides",
  
  # -------------------------------------------------------------- #
  # Cabeçalho
  # -------------------------------------------------------------- #
  header = dashboardHeader(
    titleWidth = 220,
    title = dashboardBrand(
      title = strong("Benevides"),
      #image = "www\\ods.png", 
      color = "teal"
    ),
    skin = "teal"
  ),
  
  # -------------------------------------------------------------- #
  # Menu lateral
  # -------------------------------------------------------------- #
  sidebar = dashboardSidebar(
    width = 220, style = "info",
    sidebarMenu(
      menuItem("Sobre o projeto", tabName = "project", icon = icon("diagram-project")),
      menuItem("Educação", tabName = "education", icon = icon("book")),
      menuItem("Saúde", tabName = "medical", icon = icon("notes-medical")),
      menuItem("Segurança", tabName = "security", icon = icon("shield-halved")),
      menuItem("Emprego e Renda", tabName = "income", icon = icon("suitcase"))
    )
  ), 
  
  # -------------------------------------------------------------- #
  # Corpo do Dash
  # -------------------------------------------------------------- #
  body = dashboardBody(
    tags$head(tags$style(HTML("
      .main-header .logo {
        font-family: Georgia, Times, Times New Roman, serif;
        font-weight: bold;
        font-size: 24px;
      }
    "))),
    titlePanel(
      div(class = "header",
        img(src = "ods.png", height = 50, width = 50),
        div(class = "title", "Observatório de Gestão Pública") 
      )
    ),
    tabItems(
      # ---------------------------------------------------------- #
      # Identificação da aba: PROJETO ("project")
      # ---------------------------------------------------------- #
      tabItem(
        tabName = "project",
        tabBox(
          id = "tabs", width = 12, height = "600px", status = "success",
          tabPanel(
            title = strong("Apresentação"),
            icon = icon("quote-left"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            fluidRow(
              column(6,
                HTML("
                  <div style='text-align: justify'>
                    <p>O <strong>Observatório de Gestão Pública de Benevides</strong> é uma iniciativa desenvolvida como parte de um projeto de extensão da Faculdade de Estatística da Universidade Federal do Pará. A iniciativa foi inspirada em outros projetos desenvolvidos nacional e regionalmente, como o <strong>Observatório Social do Brasil</strong> e <strong>Observatório de Desenvolvimento Sustentável em Barcarena</strong>.</p>
                    <p>O objetivo principal do projeto é tornar acessível ferramentas de acompanhamento do desenvolvimento socioeconômico do município às partes interessadas da gestão (secretarias, entidades sociais e representantes comunitários). </p> 
                    <p>Sed consectetur reiciendis ut velit autem non unde rerum sit atque dolores ut delectus voluptatem a tenetur debitis. Et eveniet quia id odit omnis ut ipsam omnis ut cumque molestiae ut deleniti atque qui possimus dolores vel dignissimos dolores. Aut repellendus excepturi aut similique praesentium et sunt eligendi sed dolor molestiae ex optio consequatur. In voluptatem deserunt et reiciendis accusantium At voluptas laboriosam sit velit temporibus et repellendus nulla cum harum sequi et maiores galisum. </p>
                    <p>Quo minus voluptates eos enim dolor ut officia eaque non modi possimus! Ut nobis distinctio non nihil consectetur sed doloribus iusto id incidunt saepe est repellendus nesciunt vel culpa Quis qui quisquam consequuntur. Et excepturi dolor vel quisquam nihil qui dolorum quas qui enim labore ut voluptatem blanditiis. Ab voluptatem adipisci non nemo voluptas nam error consequatur eos maxime suscipit eum neque velit? </p>
                  </div>
                ")
              ),
              column(6,
                HTML("
                  <div style='text-align: justify'>
                    <p>Ea alias odio ut dolorem repellendus et quia ullam aut reiciendis commodi qui reiciendis voluptatum eum Quis possimus vel optio reprehenderit. Ab quia sint qui minus consequatur aut illum sint sit inventore rerum a rerum ipsam et animi fuga. Ea fuga incidunt et quia minus quo fuga architecto. Et alias itaque At provident autem quo cupiditate deserunt. </p><p> Quo veniam harum et animi magni non eveniet omnis. Eos quae amet ut voluptatem rerum quo asperiores magni. Sed aperiam quis nam esse velit quo rerum sunt est vitae provident aut omnis itaque. </p>
                    <p>A plataforma integra dados do INEP de 2011 a 2024, oferecendo visualizações interativas e análises comparativas que auxiliam na tomada de decisão e no planejamento de políticas públicas.</p>
                  </div>
                ")
              )
            )
          ),
          tabPanel(
            title = strong("Material e Métodos"),
            icon = icon("chart-simple"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            fluidRow(
              column(6,
                HTML("
                  <div style='text-align: justify'>
                    <p> <strong>Lorem ipsum</strong> dolor sit amet. Quo ratione esse ab nemo voluptatum aut reprehenderit accusamus qui adipisci perferendis est dolorem obcaecati et sunt pariatur. Ad odit officia ut tempore accusamus hic voluptates vero. Qui velit iure sit soluta dolorum ut quasi voluptate. Eum praesentium eligendi qui laudantium sunt qui vitae itaque est dolor esse. </p>
                    <p>Sed consectetur reiciendis ut velit autem non unde rerum sit atque dolores ut delectus voluptatem a tenetur debitis. Et eveniet quia id odit omnis ut ipsam omnis ut cumque molestiae ut deleniti atque qui possimus dolores vel dignissimos dolores. Aut repellendus excepturi aut similique praesentium et sunt eligendi sed dolor molestiae ex optio consequatur. In voluptatem deserunt et reiciendis accusantium At voluptas laboriosam sit velit temporibus et repellendus nulla cum harum sequi et maiores galisum. </p>
                    <p>Quo minus voluptates eos enim dolor ut officia eaque non modi possimus! Ut nobis distinctio non nihil consectetur sed doloribus iusto id incidunt saepe est repellendus nesciunt vel culpa Quis qui quisquam consequuntur. Et excepturi dolor vel quisquam nihil qui dolorum quas qui enim labore ut voluptatem blanditiis. Ab voluptatem adipisci non nemo voluptas nam error consequatur eos maxime suscipit eum neque velit? </p>
                  </div>
                ")
              ),
              column(6,
                HTML("
                  <div style='text-align: justify'>
                    <p>Ea alias odio ut dolorem repellendus et quia ullam aut reiciendis commodi qui reiciendis voluptatum eum Quis possimus vel optio reprehenderit. Ab quia sint qui minus consequatur aut illum sint sit inventore rerum a rerum ipsam et animi fuga. Ea fuga incidunt et quia minus quo fuga architecto. Et alias itaque At provident autem quo cupiditate deserunt. </p><p> Quo veniam harum et animi magni non eveniet omnis. Eos quae amet ut voluptatem rerum quo asperiores magni. Sed aperiam quis nam esse velit quo rerum sunt est vitae provident aut omnis itaque. </p>
                    <p>A plataforma integra dados do INEP de 2011 a 2024, oferecendo visualizações interativas e análises comparativas que auxiliam na tomada de decisão e no planejamento de políticas públicas.</p>
                  </div>
                ")
              )
            )
          ),
          tabPanel(
            title = strong("Fontes"),
            icon = icon("book-bookmark"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            fluidRow(column(12,
              HTML("
                <div style='text-align: justify'>
                  <p>Barcarena Sustentável. <strong>Inspirações para um Observatório de Barcarena</strong>. Disponível em: <a, href='https://www.barcarenasustentavel.org/wp-content/uploads/2022/04/IBS_PesquisaObservatorios.pdf', target='_blank'>https://www.barcarenasustentavel.org/wp-content/uploads/2022/04/IBS_PesquisaObservatorios.pdf</a>. Último acesso em: 28 set. 2025.</p>
                  <p>Hydro. Hydro e UFPA criam observatório de desenvolvimento sustentável em Barcarena. <strong>Revista Alumínio</strong>. Disponível em: <a, href='https://revistaaluminio.com.br/hydro-e-ufpa-criam-observatorio-de-desenvolvimento-sustentavel-em-barcarena/', target='_blank'>https://revistaaluminio.com.br/hydro-e-ufpa-criam-observatorio-de-desenvolvimento-sustentavel-em-barcarena/</a>. Último acesso em: 28 set. 2025.</p>
                </div>
              ")
            ))
          ),
          tabPanel(
            title = strong("Equipe"),
            icon = icon("users"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            HTML("
              <p align='center'>
              #<img src='PERFIL_ANALISTA.jpg' alt='PERFIL' style='width: 150px; height: 150px; border-radius: 50%; bject-fit: cover; border: 3px solid #1772D1; transition: transform 0.3s; cursor: pointer;'/>
              </p> <p align='left'> <strong>TÉCNICO</strong> </p>
              <p>🏛 Universidade Federal do Pará - Faculdade de Estatística </p>
              <p>🔗 <a href='https://www.linkedin.com' target='_blank'>LinkedIn</a> | <a href='https://github.com/csilv7/EXTENSION_PROJECT_FOR_BENEVIDES' target='_blank'>GitHub</a></p>
              <p>Sobre Nós</p>
              <h4>Contato</h4>
              <p>✉ <b>E-mail:</b> pessoa@gov.br </p>
            ")
          )
        )
      ),
      
      # ---------------------------------------------------------- #
      # Identificação da aba: EDUCAÇÃO ("education")
      # ---------------------------------------------------------- #
      tabItem(
        tabName = "education",
        # -------------------------------------------------------- #
        # Gráfico 01 - Evolução dos Indicadores Educacionais
        # -------------------------------------------------------- #
        fluidRow(
          box(width = 3,
            title = strong("Seleção de dados"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            sliderInput("anos", "Período:",
                        min = min(dados_inep$Ano), 
                        max = max(dados_inep$Ano),
                        value = c(2011, 2024),
                        step = 1),
            selectInput("municipio", "Município:",
                        choices = sort(unique(dados_inep$Nom.Mun)),
                        selected = "Benevides"),
            selectInput("nivel_educ", "Nível Educacional:",
                        choices = c("Ensino Infantil" = "EI",
                                    "Ensino Fundamental" = "EF", 
                                    "Ensino Médio" = "EM"),
                        selected = "EF"),
            selectInput("serie", "Ano/Série:",
                        choices = NULL),  # será preenchido dinamicamente
            selectInput("local", "Localidade:",
                        choices = unique(dados_inep$Local.),
                        selected = "Total"),
            selectInput("dep_adm", "Dependência Administrativa:",
                        choices = unique(dados_inep$Dep.Adm),
                        selected = "Total")
          ),
          box(height = "575px",
            width = 9,
            title = strong("Evolução dos Indicadores Educacionais"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            plotOutput("grafico_linha")
            #plotlyOutput("grafico_linha")
          )
        ),
        # -------------------------------------------------------- #
        # Gráfico 02 - Região Norte x Município
        # -------------------------------------------------------- #
        fluidRow(
          box(width = 3,
            title = strong(" "),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("ano", "Ano de referência:", 
                        choices = sort(unique(dados_inep$Ano), decreasing = TRUE),
                        selected = 2024),
            selectInput("municipio", "Município:", 
                        choices = sort(unique(dados_inep$Nom.Mun)), 
                        selected = "Benevides"),
            selectInput("variavel", "Indicador:",
                        choices = c("Taxa de Aprovação", "Taxa de Reprovação", 
                                    "Taxa de Abandono", "Taxa Distorção-Idade",
                                    "Média de Alunos por Turma"),
                        selected = "Taxa de Aprovação"),
            selectInput("nivel_educ02", "Nível Educacional:",
                        choices = c("Ensino Infantil" = "EI",
                                    "Ensino Fundamental" = "EF", 
                                    "Ensino Médio" = "EM"),
                        selected = "EF"),
            selectInput("serie02", "Ano/Série:",
                        choices = NULL),  
            selectInput("local", "Localidade:",
                        choices = unique(dados_inep$Local.),
                        selected = "Total"),
            selectInput("dep_adm", "Dependência Administrativa:",
                        choices = unique(dados_inep$Dep.Adm),
                        selected = "Total")
          ),
          box(height = "625px",
            width = 9,
            title = strong("Comparativo Regional: Região Norte"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("mapa_norte")
          )
        ),
        # -------------------------------------------------------- #
        # Gráfico 03 - Estado do Pará x Município
        # -------------------------------------------------------- #
        fluidRow(
          box(width = 3,
            title = strong(" "),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("ano", "Selecionar ano:", 
                        choices = sort(unique(dados_inep$Ano), decreasing = TRUE),
                        selected = 2024),
            selectInput("variavel", "Indicador:",
                        choices = c("Taxa de Aprovação", "Taxa de Reprovação", 
                                    "Taxa de Abandono", "Taxa Distorção-Idade",
                                    "Média de Alunos por Turma"),
                        selected = "Taxa de Aprovação"),
            selectInput("municipio", "Município:", 
                        choices = sort(unique(dados_inep$Nom.Mun)), 
                        selected = "Benevides"),
            selectInput("nivel_educ03", "Nível Educacional:",
                        choices = c("Ensino Infantil" = "EI",
                                    "Ensino Fundamental" = "EF", 
                                    "Ensino Médio" = "EM"),
                        selected = "EF"),
            selectInput("serie03", "Ano/Série:",
                        choices = NULL),
            selectInput("local", "Localidade:",
                        choices = unique(dados_inep$Local.),
                        selected = "Total"),
            selectInput("dep_adm", "Dependência Administrativa:",
                        choices = unique(dados_inep$Dep.Adm),
                        selected = "Total")
          ),
          box(height = "625px",
            width = 9,
            title = strong("Comparativo Regional: Estado do Pará"),
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("mapa_para")
          )
        ),        
        # -------------------------------------------------------- #
        # Tabela 01 - Dados para Exportação
        # -------------------------------------------------------- #
        fluidRow(
          # Tabela de Dados
          box(
            width = 12,
            title = strong("Resumo dos Dados"),
            status = "info",
            solidHeader = TRUE,
            DTOutput("tabela_dados")
          )
        )
      ),
      
      # ---------------------------------------------------------- #
      # Identificação da aba: SAÚDE ("medical")
      # ---------------------------------------------------------- #
      tabItem(
        tabName = "medical",
        h2("Análise de Saúde - Em Desenvolvimento"),
        p("Esta seção está em construção.")
      ),

      # ---------------------------------------------------------- #
      # Identificação da aba: SEGURANÇA ("security")
      # ---------------------------------------------------------- #      
      tabItem(
        tabName = "security",
        h2("Análise de Segurança - Em Desenvolvimento"),
        p("Esta seção está em construção.")
      ),

      # ---------------------------------------------------------- #
      # Identificação da aba: EMPREGO E RENDA ("income")
      # ---------------------------------------------------------- #      
      tabItem(
        tabName = "income",
        h2("Análise de Emprego e Renda - Em Desenvolvimento"),
        p("Esta seção está em construção.")
      )
    )
  ),

  # -------------------------------------------------------------- #
  # Rodapé da página
  # -------------------------------------------------------------- #
  footer = dashboardFooter(
    left = strong(paste0("Atualizado em ", format(Sys.Date(), "%d/%m/%Y"))),
    right = strong("Faculdade de Estatística - UFPA")
  )
)

# ---------------------------------------------------------------- #
# Camada Servidor
# ---------------------------------------------------------------- #
server = function(input, output, session) {
  
  # -------------------------------------------------------------- #
  # Funções para geração dos gráficos
  # -------------------------------------------------------------- #
  # Função para o gráfico 01 - Evolução dos Indicadores
  # -------------------------------------------------------------- #
  grafico_linha_reactive <- reactive({
    req(input$anos, input$municipio, input$nivel_educ, input$serie, input$local, input$dep_adm)

    # Renomeação das variáveis (facilita o callback)
    nivel = input$nivel_educ
    serie = input$serie
    mun = input$municipio
    ano_inicial = input$anos[1]
    ano_final = input$anos[2]
    local = input$local
    depadm = input$dep_adm

    # Condicional para uso dos termos originais
    niv = case_when(nivel == "EI" ~ "Ensino Infantil",
                    nivel == "EF" ~ "Ensino Fundamental",
                    nivel == "EM" ~ "Ensino Médio",
                    TRUE ~ nivel)

    # Identificação do título, subtítulo e variáveis
    mtitle = paste0("Evolução Geral dos Indicadores do ", niv, " - ", serie); mtitle = as.character(mtitle)
    stitle = paste0("Taxas de Aprovação, Reprovação, Abandono e Distorção-Idade em ", mun, ", de ", ano_inicial, " a ", ano_final)

    # colunas selecionadas
    colunas = c(paste0(serie," ",nivel,"_Tx_Aprov"),  # "Aprovação" 
                paste0(serie," ",nivel,"_Tx_Reprov"), # "Reprovação"
                paste0(serie," ",nivel,"_Tx_Aband"),  # "Abandono" 
                paste0(serie," ",nivel,"_Tx_TDI"))    # "Distorção-Idade"
    
    # "Derretimento" dos dados
    df_melt = dados_inep %>% filter(Nom.Mun == mun & Dep.Adm == depadm  & Local. == local & Ano >= ano_inicial & Ano <= ano_final) %>%
      select("Ano", "Nom.Mun", all_of(colunas)) %>%
	pivot_longer(., cols = colunas, names_to = "Taxa", values_to = "Perc") %>% arrange(Taxa)

    
    # Labels / Gráfico
    labs = c("Aprovação", "Reprovação", "Abandono", "Distorção-Idade")

    graf_01 = ggplot(df_melt, aes(x = Ano, y = Perc, color = Taxa, group = Taxa)) +
      geom_line(linewidth = .90) +
      geom_point(size = 2) +
      labs(title =  bquote(bold(.(mtitle))), subtitle = stitle, x = "Ano", y = "Percentual (%)") +
      scale_x_continuous(breaks = df_melt$Ano) + 
      scale_color_manual(values = c('#0A9396', '#FC4E68', '#CA6702', '#005F73'), labels = labs, name='') +
      theme_minimal() +
      theme(legend.position = "bottom")

    return(graf_01)
    
    #ggplotly(graf_01) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  # -------------------------------------------------------------- #
  # Função para o gráfico 02 - Região Norte x Munícipio
  # -------------------------------------------------------------- #
  mapa_norte_reactive <- reactive({
    req(input$nivel_educ02, input$serie02, input$municipio, input$variavel, input$ano, input$local, input$dep_adm)

    nivel = input$nivel_educ02
    serie = input$serie02
    mun = input$municipio
    ano = input$ano
    local = input$local
    depadm = input$dep_adm
    var = input$variavel

    niv = case_when(nivel == "EI" ~ "Ensino Infantil",
                    nivel == "EF" ~ "Ensino Fundamental",
                    nivel == "EM" ~ "Ensino Médio",
                    TRUE ~ nivel)

    if (nivel == "EI") {
      mtitle = paste0("Média de Alunos por Turma do ", niv)
    } else {
      mtitle = paste0(var, " do ", niv, " - ", serie)
    }

    stitle = paste0("Comparativo da Região Norte com o Município de ", mun, " (", ano,")")

    tryCatch({
      # Definir coluna baseada na variável selecionada
      coluna = case_when(
        var == "Taxa de Aprovação" ~ paste0(serie," ",nivel,"_Tx_Aprov"),
        var == "Taxa de Reprovação" ~ paste0(serie," ",nivel,"_Tx_Reprov"),
        var == "Taxa de Abandono" ~ paste0(serie," ",nivel,"_Tx_Aband"),
        var == "Taxa Distorção-Idade" ~ paste0(serie," ",nivel,"_Tx_TDI"),
        var == "Média de Alunos por Turma" ~ paste0(serie, " ", nivel,"_MAT"),
        TRUE ~ var
      )

      # Verificar se a coluna existe nos dados
      if(!coluna %in% names(dados_inep)) {
        stop(paste("Coluna", coluna, "não encontrada nos dados"))
      }

      # Filtrar dados
      dados_join = dados_inep %>% 
        filter(Local. == local, Dep.Adm == depadm, Ano == ano) %>% 
        select(Ano, Cod.Mun, Nom.Mun, Local., Dep.Adm, all_of(coluna)) %>% 
        rename(code_muni = Cod.Mun)
      
      # DEBUG: Verificar dados filtrados
      print(paste("Número de municípios com dados:", nrow(dados_join)))
      print(paste("Valor para", mun, ":", dados_join %>% filter(Nom.Mun == mun) %>% pull(coluna)))

      # Carregar mapa da região Norte
      mun_norte = geobr::read_municipality(year = 2020) %>%  # Usando 2020 para maior compatibilidade
        filter(abbrev_state %in% c("AC","AM","AP","PA","RO","RR","TO"))
      
      # Juntar dados
      mun_norte_com_dados = mun_norte %>% 
        left_join(dados_join, by = "code_muni")
      
      # Verificar se há dados após o join
      if(all(is.na(mun_norte_com_dados[[coluna]]))) {
        stop("Nenhum dado encontrado após união com o mapa")
      }

      # Obter valor do município selecionado
      valor_mun = dados_join %>% 
        filter(Nom.Mun == mun) %>% 
        pull(coluna)
      
      if(length(valor_mun) == 0) {
        valor_mun_text = "N/A"
      } else {
        valor_mun_text = paste0(round(valor_mun, 2), "%")
      }

      # Criar mapa
      graf_02 = ggplot(mun_norte_com_dados) +
        geom_sf(aes(fill = !!sym(coluna)), color = "white", size = 0.1) +
        geom_sf(data = filter(mun_norte_com_dados, Nom.Mun == mun), 
                color = "red", size = 0.8, fill = NA) +
        labs(title = mtitle, subtitle = stitle, fill = '') +
        scale_fill_viridis_c(option = "C", na.value = "grey90") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      # Adicionar anotação com valor do município
      bbox <- st_bbox(mun_norte_com_dados)
      graf_02 <- graf_02 + 
        annotate("text", 
                 x = bbox$xmin + (bbox$xmax - bbox$xmin) * 0.1,
                 y = bbox$ymin + (bbox$ymax - bbox$ymin) * 0.9,
                 label = paste(mun, ":", valor_mun_text),
                 color = "red", fontface = "bold", size = 4)

      ggplotly(graf_02) %>% layout(legend = list(orientation = "h", y = -0.1))
      
    }, error = function(e) {
      message("Erro no mapa da Região Norte: ", e$message)
      plot_ly() %>% 
        add_annotations(text = paste("Erro:", e$message),
                        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                        showarrow = FALSE,
                        font = list(size = 16)) %>%
        layout(title = "Mapa não disponível",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })

  # -------------------------------------------------------------- #
  # Função para o gráfico 03 - Estado do Pará x Município
  # -------------------------------------------------------------- #
  mapa_para_reactive <- reactive({
    req(input$nivel_educ03, input$serie03, input$municipio, input$variavel, input$ano, input$local, input$dep_adm)

    nivel = input$nivel_educ03  # CORREÇÃO: usar input$nivel_educ03
    serie = input$serie03       # CORREÇÃO: usar input$serie03
    mun = input$municipio
    ano = input$ano
    local = input$local
    depadm = input$dep_adm
    var = input$variavel

    niv = case_when(nivel == "EI" ~ "Ensino Infantil",
                    nivel == "EF" ~ "Ensino Fundamental",
                    nivel == "EM" ~ "Ensino Médio",
                    TRUE ~ nivel)

    if (nivel == "EI") {
      mtitle = paste0("Média de Alunos por Turma do ", niv)
    } else {
      mtitle = paste0(var, " do ", niv, " - ", serie)
    }

    stitle = paste0("Comparativo do Estado do Pará com o Município de ", mun, " (", ano,")")

    tryCatch({
      coluna = case_when(
        var == "Taxa de Aprovação" ~ paste0(serie," ",nivel,"_Tx_Aprov"),
        var == "Taxa de Reprovação" ~ paste0(serie," ",nivel,"_Tx_Reprov"),
        var == "Taxa de Abandono" ~ paste0(serie," ",nivel,"_Tx_Aband"),
        var == "Taxa Distorção-Idade" ~ paste0(serie," ",nivel,"_Tx_TDI"),
        var == "Média de Alunos por Turma" ~ paste0(serie, " ", nivel,"_MAT"),
        TRUE ~ var
      )

      # Verificar se a coluna existe
      if(!coluna %in% names(dados_inep)) {
        stop(paste("Coluna", coluna, "não encontrada nos dados"))
      }

      # Filtrar dados
      dados_join = dados_inep %>% 
        filter(Local. == local, Dep.Adm == depadm, Ano == ano) %>% 
        select(Ano, Cod.Mun, Nom.Mun, Local., Dep.Adm, all_of(coluna)) %>% 
        rename(code_muni = Cod.Mun)

      # Carregar mapa do Pará
      mun_pa = geobr::read_municipality(year = 2020) %>%  # Usando 2020 para maior compatibilidade
        filter(abbrev_state == "PA")
      
      # Juntar dados
      mun_pa_com_dados = mun_pa %>% 
        left_join(dados_join, by = "code_muni")

      # Obter valor do município selecionado
      valor_mun = dados_join %>% 
        filter(Nom.Mun == mun) %>% 
        pull(coluna)
      
      if(length(valor_mun) == 0) {
        valor_mun_text = "N/A"
      } else {
        valor_mun_text = paste0(round(valor_mun, 2), "%")
      }

      # Criar mapa
      graf_03 = ggplot(mun_pa_com_dados) +
        geom_sf(aes(fill = !!sym(coluna)), color = "white", size = 0.1) +
        geom_sf(data = filter(mun_pa_com_dados, Nom.Mun == mun), 
                color = "red", size = 0.8, fill = NA) +
        labs(title = mtitle, subtitle = stitle, fill = '') +
        scale_fill_viridis_c(option = "C", na.value = "grey90") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      # Adicionar anotação
      bbox <- st_bbox(mun_pa_com_dados)
      graf_03 <- graf_03 + 
        annotate("text", 
                 x = bbox$xmin + (bbox$xmax - bbox$xmin) * 0.1,
                 y = bbox$ymin + (bbox$ymax - bbox$ymin) * 0.9,
                 label = paste(mun, ":", valor_mun_text),
                 color = "red", fontface = "bold", size = 4)

      ggplotly(graf_03) %>% layout(legend = list(orientation = "h", y = -0.1))
      
    }, error = function(e) {
      message("Erro no mapa do Pará: ", e$message)
      plot_ly() %>% 
        add_annotations(text = paste("Erro:", e$message),
                        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                        showarrow = FALSE,
                        font = list(size = 16)) %>%
        layout(title = "Mapa não disponível",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })

  # -------------------------------------------------------------- #
  # Função para a Tabela 01 - Resumo dos dados
  # -------------------------------------------------------------- #
  tabela_dados_reactive <- reactive({
    req(input$nivel_educ, input$municipio, input$anos, input$nivel_educ, input$variavel)

    # Identificação da variável (troca da identificação)
    variavel = case_when(input$variavel == "Taxa de Aprovação" ~ paste0("Total ",nivel,"_Tx_Aprov"),
				 input$variavel == "Taxa de Reprovação" ~ paste0("Total ",nivel,"_Tx_Reprov"),
				 input$variavel == "Taxa de Abandono" ~ paste0("Total ",nivel,"_Tx_Aband"),
				 input$variavel == "Taxa Distorção-Idade" ~ paste0("Total ",nivel,"_Tx_TDI"),
				 TRUE ~ input$variavel)

    
    # Simulação de tabela (substitua pelos seus dados reais)
    anos_seq = seq(input$anos[1], input$anos[2])
    df_tabela = data.frame(
      Ano = anos_seq,
      Município = input$municipio,
      `Taxa Aprovação` = dados_inep %>% filter(Ano >= min(anos_seq) & Ano <=max(anos_seq)  & Local. == "Total" & Dep.Adm == "Total" & Nom.Mun == input$municipio) %>% select(variavel),
      `Taxa Reprovação` = dados_inep %>% filter(Ano >= min(anos_seq) & Ano <=max(anos_seq)  & Local. == "Total" & Dep.Adm == "Total" & Nom.Mun == input$municipio) %>% select(variavel),
      `Taxa Abandono` = dados_inep %>% filter(Ano >= min(anos_seq) & Ano <=max(anos_seq)  & Local. == "Total" & Dep.Adm == "Total" & Nom.Mun == input$municipio) %>% select(variavel),
      `Distorção Idade` = dados_inep %>% filter(Ano >= min(anos_seq) & Ano <=max(anos_seq)  & Local. == "Total" & Dep.Adm == "Total" & Nom.Mun == input$municipio) %>% select(variavel)
    )
    
    return(df_tabela)
  })
  
  # -------------------------------------------------------------- #  
  # Outputs
  # -------------------------------------------------------------- #
  #output$grafico_linha <- renderPlotly({
  output$grafico_linha = renderPlot({
    grafico_linha_reactive()
  })
  
  output$mapa_norte <- renderPlotly({
    mapa_norte_reactive()
  })
 
  output$mapa_para <- renderPlotly({
    mapa_para_reactive()
  })

  output$tabela_dados <- renderDT({
    datatable(
      tabela_dados_reactive(),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      extensions = 'Buttons'
    )
  })
  
  # Escolha dinâmica do ano ou série a partir do nível educacional 
  observeEvent(input$nivel_educ, {
    series_opcoes = switch(input$nivel_educ,
      "EI" = c("Creche", "Pré-Escola", "Total"),
      "EF" = c("Anos Iniciais", "Anos Finais", "1° Ano", "2° Ano", "3° Ano", "4° Ano", "5° Ano", "6° Ano", "7° Ano", "8° Ano", "9° Ano", "Total"),
      "EM" = c("1ª Série", "2ª Série", "3ª Série", "4ª Série", "Não Seriado", "Total"))
    updateSelectInput(session, "serie",
                      choices = series_opcoes,
                      selected = "Total")
  })

  observeEvent(input$nivel_educ02, {
    series_opcoes = switch(input$nivel_educ02,
      "EI" = c("Creche", "Pré-Escola", "Total"),
      "EF" = c("Anos Iniciais", "Anos Finais", "1° Ano", "2° Ano", "3° Ano", "4° Ano", "5° Ano", "6° Ano", "7° Ano", "8° Ano", "9° Ano", "Total"),
      "EM" = c("1ª Série", "2ª Série", "3ª Série", "4ª Série", "Não Seriado", "Total"))
    updateSelectInput(session, "serie02",
                      choices = series_opcoes,
                      selected = "Total")
  })

  observeEvent(input$nivel_educ03, {
    series_opcoes = switch(input$nivel_educ03,
      "EI" = c("Creche", "Pré-Escola", "Total"),
      "EF" = c("Anos Iniciais", "Anos Finais", "1° Ano", "2° Ano", "3° Ano", "4° Ano", "5° Ano", "6° Ano", "7° Ano", "8° Ano", "9° Ano", "Total"),
      "EM" = c("1ª Série", "2ª Série", "3ª Série", "4ª Série", "Não Seriado", "Total"))
    updateSelectInput(session, "serie03",
                      choices = series_opcoes,
                      selected = "Total")
  })
}

# ----------------------------
# Produto Final: Dashboard
# ----------------------------
shinyApp(ui, server)