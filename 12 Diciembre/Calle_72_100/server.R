shinyServer(function(input, output){
  
#### ========= EMPRESA --------------
  data_empresas <- eventReactive(input$go,{
    data_f1 <- empresas %>% 
      dplyr::filter(if (input$xpiramide1 != "TOTAL") Piramide1 %in% input$xpiramide1 else TRUE,
                    if (input$xpiramide2 != "TOTAL") Piramide2 %in% input$xpiramide2 else TRUE,
                    if (input$xpoligono != "TOTAL") cuadrante_emp %in% input$xpoligono else TRUE)
    return(data_f1)
  })
  
  data_personas1 <- eventReactive(input$go,{
    data_f2 <- personas %>%
      filter(if (input$xpiramide1 != "TOTAL") Piramide1 %in% input$xpiramide1 else TRUE,
             if (input$xpiramide2 != "TOTAL") Piramide2 %in% input$xpiramide2 else TRUE,
             if (input$xpoligono != "TOTAL") cuadrante_v %in% input$xpoligono else TRUE | if (input$xpoligono != "Total") cuadrante_t %in% input$xpoligono else TRUE)
    return(data_f2)
  })
  
  paleta <- eventReactive(input$go,{
    pal <- colorFactor(palette = 'Dark2',domain = data_empresas()$Piramide1)
  })
  
  output$Mapaempresas <- renderLeaflet({
    
    map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
    map %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addCircleMarkers(data = data_empresas(), lng = ~cx_empresa, lat = ~cy_empresa, fillOpacity = 0.7, color = ~paleta()(data_empresas()$Piramide1), stroke = FALSE, 
                       radius = ~ifelse(data_empresas()$Piramide1 == "1 EMP GRANDES", 15,
                                        ifelse(data_empresas()$Piramide1 == "2 EMP MEDIO", 10,
                                               ifelse(data_empresas()$Piramide1 == "3 EMPRESAS PYMES", 6, 
                                                      ifelse(data_empresas()$Piramide1 == "4 MICRO", 2 , 10))))) %>%
      addLegend(pal=paleta(), values= data_empresas()$Piramide1, opacity=0.7, title = "Piramide 1", position = "bottomright") %>%
      addPolygons(data=poligonos, color = "teal", opacity = 0.1) 
  })
  
  output$info_emp1 <- renderValueBox({
    data_f1<-data_empresas()
    valueBox(
      value = formatC(length(unique(data_f1$id_empresa)),digits = 0, format = "d", big.mark=","),
      subtitle = "Total Empresas",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$info_emp2 <- renderValueBox({
    data_f1<-data_empresas()
    valueBox(
      value = formatC(sum(data_f1$NumEmpleados, na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Total Afiliados",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$info_emp3 <- renderValueBox({
    data_f1<-data_empresas()
    valueBox(
      value = formatC(sum(data_f1$promedio_aportes, na.rm = T)/1000000,digits = 0, format = "d", big.mark=","),
      subtitle = "Total Aportes (M)",
      icon = icon("fas fa-dollar-sign"),
      color = "blue"
    )
  })
  
  output$info_emp4 <- renderValueBox({
    data_f1<-data_empresas()
    valueBox(
      value = formatC(sum(data_f1$promedio_remaneto, na.rm = T)/1000000,digits = 0, format = "d", big.mark=","),
      subtitle = "Total Remanente (M)",
      icon = icon("fas fa-dollar-sign"),
      color = "blue"
    )
  })
  
  output$plot_pira_glob <- renderPlotly({
    aux1 <- data_personas1() %>%
      filter(!is.na(Edad)) %>%
      mutate(edad_agru =  cut(Edad, breaks = c(0,10,20,30,40,50,60,80,90,100,110,120,130))) %>%
      group_by(edad_agru, Genero) %>%
      summarise(clientes=n_distinct(id_persona)) %>%
      mutate(pop = ifelse(Genero == "M",yes = -clientes, no = clientes)) %>%
      mutate(abs_pop = abs(pop)) %>% 
      ungroup() %>% 
      mutate(freq = clientes / sum(clientes))
    
    m <- list(l = 50,r = 50,b = 50,t = 50, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    aux1 %>%
      plot_ly(x= ~pop, y=~edad_agru,color=~Genero) %>%
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~paste(comma(abs_pop), "(", round(freq*100,2), "%)"),  textposition = 'outside') %>%
      layout(margin = m,
             bargap = 0.1,
             barmode = 'overlay',
             title = 'Piramide Poblacional', 
             font = list(color = 'lightgrey'),
             xaxis = list(title='Afiliados', titlefont = f1, tickfont = f2, zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                          range = c(-round(max(abs(aux1$pop))*1.4, digits = 0), round(max(abs(aux1$pop))*1.4, digits = 0))),
             yaxis = list(title='Edad Agru', titlefont = f1, tickfont = f2),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  output$plot1_glob <- renderPlotly({
    data_plot <- data_empresas() %>%
      dplyr::select(id_empresa,Seg_Alto:Seg_Medio) %>%
      group_by(id_empresa) %>%
      summarise(Basico = sum(Seg_Básico, na.rm = T),
                Medio = sum(Seg_Medio, na.rm = T),
                Joven = sum(Seg_Joven, na.rm = T),
                Alto = sum(Seg_Alto, na.rm = T)) %>%
      gather(key = "Segmento", value = "Conteo", 2:5)
    
    m <- list(l = 0,r = 0,b = 50,t = 50, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(214,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "lightgrey")
    
    p1 <- plot_ly(data_plot, labels = ~Segmento, values = ~Conteo, type = 'pie', hole = 0, alpha = 0.9,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  # hoverinfo = 'text', 
                  # text = ~paste('Total empleados:', Conteo), 
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Segmento',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
  output$plot2_glob <- renderPlotly({
    data_plot <- data_empresas() %>%
      dplyr::select(id_empresa,Cat_A:Cat_C) %>%
      group_by(id_empresa) %>%
      summarise(A = sum(Cat_A, na.rm = T),
                B = sum(Cat_B, na.rm = T),
                C = sum(Cat_C, na.rm = T)) %>%
      gather(key = "Categoria", value = "Conteo", 2:4)
    
    m <- list(l = 0,r = 0,b = 50,t = 50, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  # hoverinfo = 'text',
                  # text = ~paste('Total empleados:', Conteo),
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Categoria',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
  output$plot_actividades <- renderPlotly({
    grap1 <- data_empresas() %>% 
      select(ActividadCIIU) %>% 
      group_by(ActividadCIIU) %>% 
      summarise(Conteo = n()) %>% 
      arrange(desc(Conteo)) %>%
      filter(row_number() <= 10)
    
    m <- list(l=300,r=0,b=0,t=50,pad=0)
    
    grap1 %>% 
      plot_ly(x = ~Conteo, y = ~reorder(ActividadCIIU,Conteo), type = 'bar', orientation = 'h') %>%
      layout(margin = m,
             title = "Top 10 Actividades Economicas",
             xaxis = list(title = ""),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
  })
  
  output$plot_piramide1 <- renderPlotly({
    grap1 <- data_empresas() %>% 
      select(Piramide1) %>% 
      group_by(Piramide1) %>% 
      summarise(Conteo = n()) %>% 
      arrange(desc(Conteo)) %>%
      filter(row_number() <= 10)
    
    m <- list(l=100,r=0,b=0,t=50,pad=0)
    
    grap1 %>% 
      plot_ly(x = ~Conteo, y = ~reorder(Piramide1,Conteo), type = 'bar', orientation = 'h') %>%
      layout(margin = m,
             title = "Empresas por Piramide 1",
             xaxis = list(title = ""),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
  })
  
  output$plot_piramide2 <- renderPlotly({
    grap1 <- data_empresas() %>% 
      select(Piramide2) %>% 
      group_by(Piramide2) %>% 
      summarise(Conteo = n()) %>% 
      arrange(desc(Conteo)) %>%
      filter(row_number() <= 10)
    
    m <- list(l=100,r=0,b=0,t=50,pad=0)
    
    grap1 %>% 
      plot_ly(x = ~Conteo, y = ~reorder(Piramide2,Conteo), type = 'bar', orientation = 'h') %>%
      layout(margin = m,
             title = "Empresas por Piramide 2",
             xaxis = list(title = ""),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
  })
  
  output$listado_empresas <- renderDataTable({
    aux <- data_empresas() %>% 
      select(RazonSocial,Piramide1,Piramide2)
    datatable(aux, options = list(dom = "lt", pageLength = 10, lengthChange = FALSE), rownames = FALSE, filter = 'top')
  })
  
#### PERSONAS ===============================
  
  data_personas2 <- eventReactive(input$go2,{
    data_f2 <- personas %>%
      filter(if (input$xcategoria != "TOTAL") Categoria %in% input$xcategoria else TRUE,
             if (input$xsegmento != "TOTAL") Segmento_poblacional %in% input$xsegmento else TRUE,
             (if (input$xpoligono2 != "TOTAL") cuadrante_v %in% input$xpoligono2 else TRUE) | (if (input$xpoligono2 != "TOTAL") cuadrante_t %in% input$xpoligono2 else TRUE))
    return(data_f2)
  })
  
  paletaafil2 <- eventReactive(input$go2,{
    aux_pal <- data_personas2()
    mypalette = colorBin(palette="viridis", domain=aux_pal$smmlv2, na.color="transparent")
  })
  
  output$Mapaafiliados <- renderLeaflet({
    aux_personas <- data_personas2()
    
    map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 14))
    map %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addCircleMarkers(data = aux_personas, lng =~ cx_persona, lat =~ cy_persona, fillColor = ~paletaafil2()(aux_personas$smmlv2),
                       fillOpacity = 0.8, radius=2, stroke=FALSE) %>%
      addLegend(pal=paletaafil2(), values= aux_personas$smmlv2, opacity=0.7, title = "Salario (SMMLV)", position = "bottomright") %>%
      addPolygons(data=poligonos, color = "teal", opacity = 0.1) %>% 
      setView(-74.052422, 4.675838, zoom = 14)
  })
  
  output$info_afil1 <- renderValueBox({
    data_f1<-data_personas2()
    valueBox(
      value = formatC(mean(data_f1$Salario, na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Promedio Salario",
      icon = icon("fas fa-dollar-sign"),
      color = "blue"
    )
  })
  
  output$info_afil2 <- renderValueBox({
    data_f1<-data_personas2()
    valueBox(
      value = formatC(mean(data_f1$Edad, na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Promedio Edad",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$info_afil3 <- renderValueBox({
    data_f1<-data_personas2()
    valueBox(
      value = formatC(sum(data_f1$Genero == "M", na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "N. Hombres",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$info_afil4 <- renderValueBox({
    data_f1<-data_personas2()
    valueBox(
      value = formatC(sum(data_f1$Genero == "F", na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "N. Mujeres",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$resumen1 <- renderPlotly({
    aux1 <- data_personas2() %>%
      filter(!is.na(Edad)) %>%
      mutate(edad_agru =  cut(Edad, breaks = c(0,10,20,30,40,50,60,80,90,100,110,120,130))) %>%
      group_by(edad_agru, Genero) %>%
      summarise(clientes=n_distinct(id_persona)) %>%
      mutate(pop = ifelse(Genero == "M",yes = -clientes, no = clientes)) %>%
      mutate(abs_pop = abs(pop)) %>% 
      ungroup() %>% 
      mutate(freq = clientes / sum(clientes))
    
    m <- list(l = 50,r = 50,b = 50,t = 50, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "grey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "grey")
    
    aux1 %>%
      plot_ly(x= ~pop, y=~edad_agru,color=~Genero) %>%
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~paste(comma(abs_pop), "(", round(freq*100,2), "%)"),  textposition = 'outside') %>%
      layout(margin = m,
             bargap = 0.1,
             barmode = 'overlay',
             title = 'Piramide Poblacional', 
             font = list(color = 'grey'),
             xaxis = list(title='Afiliados', titlefont = f1, tickfont = f2, zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                          range = c(-round(max(abs(aux1$pop))*1.4, digits = 0), round(max(abs(aux1$pop))*1.4, digits = 0))),
             yaxis = list(title='Edad Agru', titlefont = f1, tickfont = f2),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    
  })
  
  output$resumen2 <- renderPlotly({
    data_plot <- data_personas2() %>%
      dplyr::select(id_persona,EstratoPersona) %>%
      filter(!is.na(EstratoPersona)) %>%
      group_by(EstratoPersona) %>%
      summarise(Conteo = n_distinct(id_persona))
    
    m <- list(l = 0,r = 0,b = 50,t = 50, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(214,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "grey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "grey")
    
    p1 <- plot_ly(data_plot, labels = ~EstratoPersona, values = ~Conteo, type = 'pie', hole = 0, alpha = 0.9,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  # hoverinfo = 'text', 
                  # text = ~paste('Total empleados:', Conteo), 
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Estrato Socieconómico',
             font = list(color = 'grey'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
})
