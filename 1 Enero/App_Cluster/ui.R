
dashboardPage(skin = "blue",
              dashboardHeader(title = "Empresas Cluster"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            menuItem("CONSULTA", tabName = "individual", icon = icon("area-chart"),
                                     menuSubItem("Check",tabName = "individual",icon = icon("check-circle")),
                                     selectInput("xname_empresa", label = "Nombre Empresa:",
                                                           choices = c(Choose = '', as.character(name_empresa_prin)), selected = "",
                                                           multiple = F),
                                     radioButtons("tipodoc","Tipo de documento",
                                                  choices = c("NIT" = "NIT",
                                                              "Cédula de Ciudadania" = "CC",
                                                              "Tarjeta de Identidad" = "TI",
                                                              "Registro Civil" = "RC",
                                                              "Cedula de Extranjería" = "CE",
                                                              "NUIP" = "NUIP",
                                                              "Pasaporte" = "PAS",
                                                              "Carnet Diplomatico" = "CD"),
                                                  selected = "NIT"),
                                     textInput("nit_empresa","Identificación Empresa",value = ""),
                                     actionButton("go", label = "Go"),
                                     tags$hr()))),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "individual",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                            box(title = "Información General",width=12,status="primary",solidHeader=TRUE,collapsible=FALSE,collapsed = TRUE,
                              fluidRow(
                                valueBoxOutput("info_empresa",width = 6),
                                valueBoxOutput("info_pir1",width = 3),
                                valueBoxOutput("info_cluster",width = 3)
                                ),
                              fluidRow(br(),
                                       column(width = 6,
                                              loadingState(), 
                                              plotlyOutput("plot1", height = 500)),
                                       column(width = 6,
                                              loadingState(), 
                                              plotlyOutput("plot2", height = 500)),
                                       br()
                              )
                            )
                            ),
                            fluidRow(
                              box(title = "Listado Convenios", width = 6, status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                                  dataTableOutput("lista_convenios")
                              ),
                              box(title = "Listado Portafolio", width = 6, status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                                  dataTableOutput("lista_portafolio")
                              )
                            ))
                  )
                )
              )
)

