# Dashboard

dashboardPage(skin = "blue",
              dashboardHeader(title = "Sist. Recomendacion TMS"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE, br(),
                            tags$img(src = "Logo.png", height=40, width=200, align="center"),
                            shinyjs::hidden(menuItem("MANUAL", tabName = "dummy")),
                            menuItem("GLOBAL", tabName = "global", icon = icon("dashboard"),
                                     menuSubItem("Check",tabName = "global",icon = icon("check-circle")),
                                     tags$hr(),
                                     h5("Filtros"),
                                     selectInput("xsegmento1", label = "Segmento Poblacion:",
                                                 choices = as.character(name_segmento1), selected = "Total", multiple = F),
                                     selectInput("xgenero1", label = "Genero:",
                                                 choices = as.character(name_genero), selected = "Total", multiple = F),
                                     selectInput("xproducto", label = "Producto",
                                                 choices = as.character(name_producto), selected = "Total", multiple = T),
                                     sliderInput("xsalario", label = "Salario:", 
                                                 min = min(data$salario),max = max(data$salario), step = 1000000, 
                                                 value=c(min(data$salario),max(data$salario))),
                                     sliderInput("xedad", label = "Edad:",
                                                 min = min(data_rec$edad),max = max(data_rec$edad), step = 1,
                                                 value=c(min(data_rec$edad),max(data_rec$edad))),
                                     actionButton("go1", label = "Go")),
                            menuItem("POR PERSONAS", tabName = "por_persona", icon = icon("dashboard"),
                                     menuSubItem("Check",tabName = "por_persona",icon = icon("check-circle")),
                                     tags$hr(),
                                     h5("Filtros"),
                                     textInput("numerocedula","Numero de Cedula",value = "1000002480"),
                                     radioButtons("tipodoc","Tipo de documento",
                                                  choices = c("Cedula" = "CC",
                                                              "Carnet Diplomatico" = "CD",
                                                              "Cedula de Extranjería" = "CE",
                                                              "Registro Civil" = "CL",
                                                              "Pasaporte" = "CP",
                                                              "Tarjeta de Identidad" = "CT",
                                                              "Tarjeta de Extranjería" = "CD"),
                                                  selected = "CC"),
                                     sliderInput("Distancia", label = h3("Distancia (km)"), min = 0.1,max = 5, step = 0.1, value=1),
                                     actionButton("go2", label = "Go"))
                            )),
              dashboardBody(
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Sistema de Recomendacion TMS"),
                                   br(),br(),
                                  h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta del Sistema de Recomemdacion de Productos TMS,
                                    la cual permite, de una forma interactiva, conocer la recomendacion de productos."),
                                  br(),
                                  h3("El documento se divide en dos secciones:"),
                                  br(),
                                  h3("La primera de ellas muestra recomendaciones de manera global de acuerdo a los filtros seleccionados, ademas muestra graficamente
                                     top 3 de recomendaciones."),
                                  br(),
                                  h3("La segunda seccion corresponde a la consulta indivual. De acuerdo a la recomendacion dada por Tarjeta TMS, se muestra 
                                    listado de convenios mas cercanos al lugar donde vive y trabaja."),
                                  br(),
                                  h3("Nota: En cada cambio de pestaña debe dar click en 'Check' para cambiar el panel principal")
                                  ),
                            column(1)
                            )
                          ),
                  tabItem(tabName = "global",
                          fluidPage(
                          h3("Recomendaciones"),
                          p(class = "text_small", "En esta seccion puede visualizar recomendaciones de productos para personas que usan TMS"),
                          fluidRow(
                            box(title = "Estadisticas Afiliado",width=12,status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                valueBoxOutput("conteo_tms",width = 3),
                                valueBoxOutput("conteo_genero_m",width = 3),
                                valueBoxOutput("conteo_genero_f",width = 3),
                                valueBoxOutput("conteo_activo",width = 3)
                            )
                          ),
                          fluidRow(
                            box(title = "Tabla Resumen",width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                dataTableOutput("resumen_recomendaciones")
                            )
                          ),
                          fluidRow(
                            box(title = "Ranking Recomendaciones",width=12,status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                column(width = 4,
                                       plotlyOutput("plot_reco1", height = 500)),
                                column(width = 4,
                                       plotlyOutput("plot_reco2", height = 500)),
                                column(width = 4,
                                       plotlyOutput("plot_reco3", height = 500))
                          )),
                          downloadButton("downloadData1", "Descargar archivo")
                          )),
                  tabItem(tabName = "por_persona",
                          fluidPage(
                            fluidRow(
                              box(title = h3("Resumen"), width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                  dataTableOutput("resumen_TMS_AFIL"))
                            ),
                            fluidRow(
                              box(title = "Punto Afiliado",status = "primary", solidHeader = FALSE,collapsible = TRUE,
                                  leafletOutput("MapaPunto", height = 600), width=6),
                              box(title = "Punto Empresa",status = "primary", solidHeader = FALSE,collapsible = TRUE,
                                  leafletOutput("MapaPunto2", height = 600), width=6)
                            ),
                            fluidRow(
                              box(title = "Resumen Convenios Afiliado",status = "primary", solidHeader = FALSE,collapsible = TRUE,
                                  dataTableOutput("resumen_convenios"), width = 6),
                              box(title = "Resumen Convenios Empresa",status = "primary", solidHeader = FALSE,collapsible = TRUE,
                                  dataTableOutput("resumen_convenios2"), width = 6)
                            )#,
                            # fluidRow(
                            #   box(title = "Listado Consumos", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                            #       dataTableOutput("resumen_consumos"), width = 6)
                            # )
                            )
                ))
              )
)

