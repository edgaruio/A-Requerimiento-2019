# Aplicacion empresas

shinyServer(function(input, output, session) {
  
  # ====== primera fila ----------------------

  data_f_con <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    data_f_con <- empresas_convenios %>%
      filter(nombre_empresa_principal == input$xname_empresa | idnitprincipal == id)
    return(data_f_con)
  })
  
  data_f_porta <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    data_f_porta <- empresas_portafolio %>%
      filter(nombre_empresa_principal == input$xname_empresa | idnitprincipal == id)
    return(data_f_porta)
  })
  
  data_f1 <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    data_f1 <- info_empresas %>%
      filter(nombre == input$xname_empresa | id_empresa == id)
    return(data_f1)
  })
  
  output$info_empresa <- renderValueBox({
    data_f<-data_f_con()
    valueBox(
      value = formatC(data_f$nombre_empresa_principal[1],format="s"),
      subtitle = "Empresa",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$info_cluster <- renderValueBox({
    data_f<-data_f_con()
    valueBox(
      value = formatC(data_f$cluster[1],format="s"),
      subtitle = "Cluster",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$info_pir1 <- renderValueBox({
    data_f<-data_f1()
    valueBox(
      value = formatC(data_f$piramide_1[1],format="s"),
      subtitle = "Piramide 1",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$plot1 <- renderPlotly({
    data_plot <- data_f1() %>%
      dplyr::select(id_empresa,Basico:Alto) %>%
      group_by(id_empresa) %>%
      summarise(Basico = sum(Basico),
                Medio = sum(Medio),
                Joven = sum(Joven),
                Alto = sum(Alto)) %>%
      gather(key = "Segmento", value = "Conteo", 2:5)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

    p1 <- plot_ly(data_plot, labels = ~Segmento, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total empleados:', Conteo),showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Segmento',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.8, y = 0.5)) %>%
      config(displayModeBar = F)
    p1
  })
   
  output$plot2 <- renderPlotly({
    data_plot <- data_f1() %>%
      dplyr::select(id_empresa,A:C) %>%
      group_by(id_empresa) %>%
      summarise(A = sum(A),
                B = sum(B),
                C = sum(C)) %>%
      gather(key = "Categoria", value = "Conteo", 2:4)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')

    p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total empleados:', Conteo),showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Categoria',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.8, y = 0.5)) %>%
      config(displayModeBar = T)
    p1
  })

  ## ====== Segunda fila ----------------------
  
  output$lista_convenios <- DT::renderDataTable({
    step1_aux1 <- data_f_con() %>% 
      # arrange(CATEGORIA) %>% 
      select(CATEGORIA, 'NOMBRE COMERCIAL' = NOMBRE.COMERCIAL)
    datatable(step1_aux1,filter = 'top',
              options = list(dom="lt", pageLength = 10),
              rownames = F)
  })
  
  output$lista_portafolio <- DT::renderDataTable({
    step1_aux1 <- data_f_porta() %>% 
      # arrange(SERVICIO) %>% 
      select(SERVICIO, PRODUCTO, SUBPRODUCTO)
    datatable(step1_aux1,filter = 'top',
              options = list(dom="lt", pageLength = 10),
              rownames = F)
  })
  
})



