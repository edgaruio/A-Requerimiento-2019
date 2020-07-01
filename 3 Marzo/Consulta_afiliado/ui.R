
dashboardPage(skin = "blue",
              dashboardHeader(title = "Presentacion Afiliados"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            tags$hr(),
                            menuItem("GLOBAL", tabName = "global", icon = icon("area-chart"),
                                     menuSubItem("Check",tabName = "global",icon = icon("check-circle")),
                                     fileInput("file1", h5("Seleccione el archivo (.csv)"),
                                               buttonLabel = "Cargar",
                                               placeholder = "Sin archivo seleccionado",
                                               multiple = F,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),
                                     checkboxInput("header", "Encabezado", TRUE),
                                     radioButtons("sep", "Separador de Columnas",
                                                  choices = c("Coma" = ",",
                                                              "Punto y Coma"= ";",
                                                              "Tabulacion" = "\t"),
                                                  selected = ","),
                                     actionButton("go2", label = "Go")),
                            tags$hr(),
                            menuItem("INDIVIDUAL", tabName = "individual", icon = icon("area-chart"),
                                     menuSubItem("Check",tabName = "individual",icon = icon("check-circle")),
                                     radioButtons("tipodoc","Tipo de documento",
                                                  choices = c("Cédula de Ciudadania" = "CC",
                                                              "Carnet Diplomatico" = "CD",
                                                              "Cedula de Extranjería" = "CE",
                                                              "Permiso Especial"="CV",
                                                              "Tarjeta de Identidad" = "TI",
                                                              "Registro Civil" = "RC",
                                                              "Pasaporte" = "PAS"),
                                                  selected = "CC"),
                                     textInput("identificacion","Identificación Persona",value = ""),
                                     actionButton("go", label = "Go"),
                                     tags$hr()))),
              dashboardBody(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Consulta Empresas"),
                                   br(),br(),
                                   h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta de información por Personas. 
                                      Se divide en dos secciones:"),
                                   br(),
                                   h3("La primera seccion muestra informacion global de cedulas de clientes afiliados. Debe cargar una base de datos 
                                      con el listado de identificaciones a consultar. Una vez que carga el documento en formato .csv debe indicar 
                                      si el listado tiene encabezado y el tipo de separador de las columnas (coma, punto y coma o tabulacion)"),
                                   br(),
                                   h3("La segunda seccion corresponde a consulta individual por Idenficacion de una Persona."),
                                   br(),
                                   h3("Al finalizar, debe dar click en Go para aplicar los filtros en cada seccion"),
                                   br(),
                                   h3("Nota: En cada cambio de pestaña debe dar click en 'Check' para cambiar el panel principal")
                            ),
                            column(1)
                          )
                  ),
                  tabItem(tabName = "global",
                          fluidPage(
                            fluidRow(valueBoxOutput("Cruces",width = 3)),
                            column(width = 3,
                                   h3("Previsualizacion de Datos"),
                                   br(),
                                   dataTableOutput("Preview"),
                                   br(),
                                   textOutput("Nota")
                                   ),
                            column(width = 9,
                                   h3("Informacion Persona"),
                                   br(),
                                   dataTableOutput("Calificada"),
                                   br(),
                                   downloadButton("downloadData", "Descargar base"),
                                   br()
                                   )
                          )
                  ),
                  tabItem(tabName = "individual",
                          fluidPage(
                            fluidRow(
                            box(title = "Información Afiliado",width=12,status="primary",solidHeader=TRUE,collapsible=FALSE,collapsed = TRUE,
                              fluidRow(
                                valueBoxOutput("info_empresa",width = 6),
                                valueBoxOutput("info_pir1",width = 3),
                                valueBoxOutput("info_pir2",width = 3)
                                )
                            )
                            )
                          )
                )
              )
)
)
