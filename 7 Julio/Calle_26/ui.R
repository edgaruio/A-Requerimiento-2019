# Dashboard
library(plotly)

dashboardPage(skin = "blue",
              dashboardHeader(title = "Calle 26"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE, br(),
                            tags$img(src = "Logo.png", height=40, width=200, align="center"),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            menuItem("EMPRESAS", tabName = "empresas", icon = icon("dashboard"),
                                     menuSubItem("Check", tabName = "empresas", icon = icon("check-circle")),
                                     h5("Filtros por empresas"),
                                     selectInput("xpoligono", label = "Cuadrante:",
                                                 choices = c("TOTAL","CAN"="can","CONECTA"="conecta","DIAN"="dian","DORADO"="dorado","EL TIEMPO"="el_tiempo","GRAN ESTACION"="gran_estacion","PATIOS TRASN"="patios_trans","SALITRE"="salitre"),
                                                 selected = "TOTAL", multiple = F),
                                     selectInput("xpiramide1", label = "Piramide 1:",choices = name_piramide1,
                                                 selected = "Total", multiple = F),
                                     selectInput("xpiramide2", label = "Piramide 2:",choices = name_piramide2,
                                                 selected = "TOTAL", multiple = F),
                                     actionButton("go", label = "Aplicar Filtros"),
                                     br(),
                                     tags$hr()
                                     ),
                            menuItem("PERSONAS", tabName = "personas", icon = icon("dashboard"),
                                     menuSubItem("Check", tabName = "personas", icon = icon("check-circle")),
                                     h5("Filtros por personas"),
                                     selectInput("xpoligono2", label = "Cuadrante:",
                                                 choices = c("TOTAL","CAN"="can","CONECTA"="conecta","DIAN"="dian","DORADO"="dorado","EL TIEMPO"="el_tiempo","GRAN ESTACION"="gran_estacion","PATIOS TRASN"="patios_trans","SALITRE"="salitre"),
                                                 selected = "TOTAL", multiple = F),
                                     selectInput("xcategoria", label = "Categoria:",choices = name_categoria,
                                                 selected = "TOTAL", multiple = F),
                                     selectInput("xsegmento", label = "Segmento Poblacional",choices = name_segmento,
                                                 selected = "TOTAL", multiple = F),
                                     actionButton("go2", label = "Aplicar Filtros"),
                                     br(),
                                     tags$hr()
                            )
                            )),
              dashboardBody(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Aplicacion Geolocalizacion Cudrantes"),
                                   br(),br(),
                                   h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta de información de empresas y afiliados 
                                      por cuadrantes"),
                                   br(),
                                   h3("En la primera pestaña muestra informacion global de Empresas por Piramide 1 y Piramide 2 y en la segunda pestañas
                                      muestra informacion de Personas por Categoria y Segmento."),
                                   br(),
                                   h3("Nota: Debe dar click en el boton 'Check' para cambiar el panel principal. 
                                      Al cambiar los filtros debe dar click en 'Go' para visualizar la información"),
                                   h4("Fecha actualización: 12/03/2020 (Corte Febrero)")
                            ),
                            column(1)
                          )
                  ),
                  tabItem(tabName = "empresas",
                          h3("Resumen Empresas"),
                          p(class = "text_small", "En esta seccion puede encontrar geolocalizacion de empresas por cuadrantes y resumen descriptivo de empresas"),
                          fluidRow(
                            column(width = 6,
                                 fluidRow(
                                   box(title = "Mapa Empresas",status = "primary", solidHeader = TRUE,collapsible = TRUE, width=12,
                                       withLoader(leafletOutput("Mapaempresas", height = 750), type = "html", loader = "loader1"))
                                   ),
                                 fluidRow(
                                   box(title = "Piramide 1",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       plotlyOutput("plot_piramide1"), width=12)
                                   ),
                                 fluidRow(
                                   box(title = "Piramide 2",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       plotlyOutput("plot_piramide2"), width=12)
                                 ),
                                 fluidRow(
                                   box(title = "Listado Empresas",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("listado_empresas"), width=12)
                                 )),
                            column(width = 6,
                                   fluidRow(
                                     box(title = "Resumen Empresas",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12,
                                         valueBoxOutput("info_emp1",width = 3),
                                         valueBoxOutput("info_emp2",width = 3),
                                         valueBoxOutput("info_emp3",width = 3),
                                         valueBoxOutput("info_emp4",width = 3))
                                   ),
                                   fluidRow(
                                   box(title = "Piramide Poblacional",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       plotlyOutput("plot_pira_glob", height = 500), width = 12)
                                   ),
                                   fluidRow(
                                     box(title = "Resumen Afiliados por Segmento",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                         plotlyOutput("plot1_glob"), width = 12)
                                     ),
                                   fluidRow(
                                   box(title = "Resumen Empresas Categoria",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       plotlyOutput("plot2_glob"), width = 12)
                                   ),
                                   fluidRow(
                                   box(title = "Resumen Empresas por Actividad Económica",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       plotlyOutput("plot_actividades"), width = 12)
                                   )
                                   )
                            )
                          ),
                  tabItem(tabName = "personas",
                          h3("Resumen Personas"),
                          p(class = "text_small", "En esta seccion puede encontrar geolocalizacion de personas por cuadrantes y resumen descriptivo de personas"),
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     box(title = "Mapa Personas (Viven y/o Trabajan)",status = "primary", solidHeader = TRUE,collapsible = TRUE, width=12,
                                         withLoader(leafletOutput("Mapaafiliados", height = 750), type = "html", loader = "loader1"))
                                     )
                            ),
                            column(width = 6,
                                   fluidRow(
                                     box(title = "Resumen Afiliados",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12,
                                         valueBoxOutput("info_afil1",width = 3),
                                         valueBoxOutput("info_afil2",width = 3),
                                         valueBoxOutput("info_afil3",width = 3),
                                         valueBoxOutput("info_afil4",width = 3))
                                   ),
                                   fluidRow(
                                     box(title = "Piramide Poblacional",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                         plotlyOutput("resumen1"), width = 12)
                                   ),
                                   fluidRow(
                                     box(title = "Resumen Afiliados Estrato",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                         plotlyOutput("resumen2"), width = 12)
                                   )
                            )
                          )
                  )
                )
              )
)

