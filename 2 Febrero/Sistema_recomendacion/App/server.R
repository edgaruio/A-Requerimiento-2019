shinyServer(function(input, output){
  
  ### Global ============================================================
  
  data_f1 <- eventReactive(input$go1,{
    data_f1 <- data_rec %>%
      filter(salario >= input$xsalario[1] & salario <= input$xsalario[2],
             edad >= input$xedad[1] & edad <= input$xedad[2],
             if(input$xsegmento1 != "Total") Segmento_Poblacion %in% input$xsegmento1 else TRUE,
             if(input$xgenero1 != "Total") Genero %in% input$xgenero1 else TRUE) %>% 
      left_join(data %>% select(Identificacion,Tarjeta,`Entre 0 y 5`,`Entre 10 y 14`,`Entre 15 y 18`,`Entre 6 y 9`,`Mas de 19`),
                by = c("Tarjeta"="Tarjeta")) %>% 
      filter(if(input$xproducto != "Total") 
        Opcion1 %in% c(input$xproducto) |
        Opcion2 %in% c(input$xproducto) | 
        Opcion3 %in% c(input$xproducto) | 
        Opcion4 %in% c(input$xproducto) | 
        Opcion5 %in% c(input$xproducto) | 
        Opcion6 %in% c(input$xproducto) | 
        Opcion7 %in% c(input$xproducto) | 
        Opcion8 %in% c(input$xproducto) | 
        Opcion9 %in% c(input$xproducto) | 
        Opcion10 %in% c(input$xproducto) else TRUE)
    return(data_f1)
  })
  
  output$conteo_tms <- renderValueBox({
    data_f<-data_f1() %>% 
      arrange(Nombre.completo) %>%
      select(-ciudad) %>% 
      select(Nombre.completo,Tarjeta:Opcion10,Saldo,salario,AUTORIZACION) %>%
      dplyr::rename("Nombre completo" = "Nombre.completo","Salario" = "salario")
    valueBox(
      value = formatC(n_distinct(data_f$Tarjeta), digits = 0, format = "d", big.mark=","),
      subtitle = "Recomendaciones TMS",
      icon = icon("credit-card"),
      color = "light-blue"
    )
  })
  
  output$conteo_genero_m <- renderValueBox({
    data_f<-data_f1() %>% 
      left_join(data %>% select(Tarjeta,genero),
                by = c("Tarjeta"="Tarjeta"))
    valueBox(
      value = formatC(sum(data_f$genero == "M"), digits = 0, format = "d", big.mark=","),
      subtitle = "Hombres",
      icon = icon("user"),
      color = "light-blue"
    )
  })
  
  output$conteo_genero_f <- renderValueBox({
    data_f<-data_f1() %>% 
      left_join(data %>% select(Tarjeta,genero),
                by = c("Tarjeta"="Tarjeta"))
    valueBox(
      value = formatC(sum(data_f$genero == "F"), digits = 0, format = "d", big.mark=","),
      subtitle = "Mujeres",
      icon = icon("user"),
      color = "light-blue"
    )
  })
  
  output$conteo_activo <- renderValueBox({
    data_f<-data_f1() 
    # %>% 
    #   left_join(data %>% select(Tarjeta,Estado.cupo),
    #             by = c("Tarjeta"="Tarjeta"))
    valueBox(
      value = formatC(sum(data_f$Estado.cupo == "ACTIVO"), digits = 0, format = "d", big.mark=","),
      subtitle = "Cupo Activo",
      icon = icon("check"),
      color = "light-blue"
    )
  })
  
  output$resumen_recomendaciones <- DT::renderDataTable({
    aux <- data_f1() %>% 
      arrange(Nombre.completo) %>%
      select(Nombre.completo,Identificacion,Tarjeta:Opcion10,AUTORIZACION,`Entre 0 y 5`,`Entre 10 y 14`,`Entre 15 y 18`,`Entre 6 y 9`,`Mas de 19`) %>%
      dplyr::rename("Nombre completo" = "Nombre.completo")
    datatable(aux,filter = 'top',options = list(dom="lt", pageLength = 10, scrollX = 400),rownames = F) #%>% 
      # formatCurrency("Salario", currency = "$", interval = 3, mark = ",", digits = 0, dec.mark = getOption("OutDec")) %>% 
      # formatCurrency("Saldo", currency = "$", interval = 3, mark = ",", digits = 0, dec.mark = getOption("OutDec"))
  })
  
  output$plot_reco1 <- renderPlotly({
    grap1 <- data_f1() %>% 
      select(Opcion1) %>% 
      group_by(Opcion1) %>% 
      summarise(Conteo = n()) %>% 
      arrange(desc(Conteo)) %>% 
      filter(row_number() <= 10)
    
    m <- list(l=300,r=0,b=0,t=50,pad=0)
      
    grap1 %>% 
      plot_ly(x = ~Conteo, y = ~reorder(Opcion1,Conteo), type = 'bar', orientation = 'h') %>%
      layout(margin = m,
             title = "Top Primera Recomendacion",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$plot_reco2 <- renderPlotly({
    grap1 <- data_f1() %>% 
      select(Opcion2) %>% 
      group_by(Opcion2) %>% 
      summarise(Conteo = n()) %>% 
      arrange(desc(Conteo))%>% 
      filter(row_number() <= 10)
    
    m <- list(l=300,r=0,b=0,t=50,pad=0)
    
    grap1 %>% 
      plot_ly(x = ~Conteo, y = ~reorder(Opcion2,Conteo), type = 'bar', orientation = 'h') %>%
      layout(margin = m,
             title = "Top Segunda Recomendacion",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$plot_reco3 <- renderPlotly({
    grap1 <- data_f1() %>% 
      select(Opcion3) %>% 
      group_by(Opcion3) %>% 
      summarise(Conteo = n()) %>% 
      arrange(desc(Conteo))%>% 
      filter(row_number() <= 10)
    
    m <- list(l=300,r=0,b=0,t=50,pad=0)
    
    grap1 %>% 
      plot_ly(x = ~Conteo, y = ~reorder(Opcion3,Conteo), type = 'bar', orientation = 'h') %>%
      layout(margin = m,
             title = "Top Tercera Recomendacion",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function(){
      paste("Recomendacion", Sys.time(), ".csv", sep = "")
    },
    content = function(file){
      write.csv2(data_f1() %>% 
                   select(Nombre.completo,Identificacion,Tarjeta:Opcion10,AUTORIZACION,`Entre 0 y 5`,`Entre 10 y 14`,`Entre 15 y 18`,`Entre 6 y 9`,`Mas de 19`),
                 file, row.names = F)
    }
  )
  
  ### Indiviual =========================================================
  
  
  data_f2 <- eventReactive(input$go2,{
    id = as.character(paste0(input$tipodoc,input$numerocedula))
    data_f2 <- data %>%
      filter(Identificacion == id) %>% 
      left_join(data_rec %>% 
                  select(Tarjeta,Opcion1:Opcion10), 
                by = c("Tarjeta"="Tarjeta")) %>% 
      select(-c(TipoTMS:Ues_2))
    return(data_f2)
  })
  
  data_f3 <- eventReactive(input$go2,{
    id = as.character(paste0(input$tipodoc,input$numerocedula))
    data_f3 <- data %>%
      select(Identificacion,Nombre.completo,Tarjeta,limitecupo,Saldo,CX_Persona:CY_Empresa) %>%
      filter(!is.na(CX_Persona)) %>%
      filter(Identificacion == id) %>%
      arrange(Identificacion) %>%
      group_by(Identificacion) %>%
      filter(row_number()==1)
    return(data_f3)
  })

  output$resumen_TMS_AFIL <- DT::renderDataTable({
    data_aux <- data_f2()
    datatable(data_aux,
              options = list(dom="lt", pageLength = 10, scrollX = 400),
              rownames = F) 
    # %>%
    #   formatPercentage("Prob Uso", digits = 1, interval = 3, mark = ",",
    #                    dec.mark = getOption("OutDec")) %>%
    #   formatCurrency("Limite cupo", currency = "$", interval = 3, mark = ",",
    #                  digits = 0, dec.mark = getOption("OutDec")) %>%
    #   formatCurrency("Saldo", currency = "$", interval = 3, mark = ",",
    #                  digits = 0, dec.mark = getOption("OutDec"))
  })

  bd_c <- eventReactive(input$go2,{
    aux1<- convenios
    aux1$Dist_V<-round(distHaversine(aux1[,c("CX","CY")], data_f3()[,c("CX_Persona","CY_Persona")])/1000,1)
    aux1<-aux1 %>% filter(Dist_V<=input$Distancia)
  })

  bd_c2 <- eventReactive(input$go2,{
    aux1<- convenios
    aux1$Dist_V<-round(distHaversine(aux1[,c("CX","CY")], data_f3()[,c("CX_Empresa","CY_Empresa")])/1000,1)
    aux1<-aux1 %>% filter(Dist_V<=input$Distancia)
  })

  output$MapaPunto <- renderLeaflet({
    Recreacion <- makeIcon(
      iconUrl = "Recreacion.png",
      iconWidth = 30, iconHeight = 40)

    aux_punto<-data_f3()
    aux_convenio<-bd_c()
    map <- leaflet(data=aux_punto,options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 12, maxZoom = 15))

    map %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      setView(lat=aux_punto$CY_Persona, lng = aux_punto$CX_Persona, zoom = 13) %>%
      addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "study area") %>%
      addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "study area") %>%
      addMarkers(data = aux_punto, lng = ~CX_Persona, lat = ~CY_Persona, label=~Nombre.completo, icon = Recreacion) %>%
      # addMarkers(data = aux_punto, lng = ~empresacx, lat = ~empresacy, label=~NombreEmpresa, icon = Recreacion) %>%
      addCircles(data = aux_punto, lng = ~CX_Persona, lat = ~CY_Persona, color = "steelblue", radius = input$Distancia*1000+50) %>%
      # addCircles(data = aux_punto, lng = ~empresacx, lat = ~empresacy, color = "steelblue", radius = input$Distancia*1000) %>%
      addMarkers(data=aux_convenio, lng =~CX, lat =~CY,
                 popup = ~as.character(paste(RAZON.SOCIAL,CATEGORIA,sep = ", ")),label = ~as.character(paste(RAZON.SOCIAL,CATEGORIA,sep = ", ")))
    # addMarkers(data=aux_convenio, lng =~empresacx, lat =~empresacy, popup = ~as.character(NombreEmpresa),label = ~as.character(NombreEmpresa))
  })

  output$MapaPunto2 <- renderLeaflet({
    Recreacion <- makeIcon(
      iconUrl = "Recreacion.png",
      iconWidth = 30, iconHeight = 40)

    aux_punto <- data_f3() %>%
      arrange(Identificacion) %>%
      group_by(Identificacion) %>%
      filter(row_number()==1)

    aux_convenio2<-bd_c2()

    map <- leaflet(data=aux_punto,options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 12, maxZoom = 15))

    map %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      setView(lat=aux_punto$CY_Empresa, lng = aux_punto$CX_Empresa, zoom = 13) %>%
      addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "study area") %>%
      addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "study area") %>%
      # addMarkers(data = aux_punto, lng = ~CX, lat = ~CY, label=~Nombre.completo, icon = Recreacion) %>%
      addMarkers(data = aux_punto, lng = ~CX_Empresa, lat = ~CY_Empresa, label=~"", icon = Recreacion) %>%
      # addCircles(data = aux_punto, lng = ~CX, lat = ~CY, color = "steelblue", radius = input$Distancia*1000) %>%
      addCircles(data = aux_punto, lng = ~CX_Empresa, lat = ~CY_Empresa, color = "steelblue", radius = input$Distancia*1000) %>%
      addMarkers(data=aux_convenio2, lng =~CX, lat =~CY,
                 popup = ~as.character(paste(RAZON.SOCIAL,CATEGORIA,sep = ", ")),label = ~as.character(paste(RAZON.SOCIAL,CATEGORIA,sep = ", ")))
    # addMarkers(data=aux_convenio, lng =~empresacx, lat =~empresacy, popup = ~as.character(NombreEmpresa),label = ~as.character(NombreEmpresa))
  })

  output$resumen_convenios <- DT::renderDataTable({
    aux1 <- bd_c()
    step1_aux1 <- aux1 %>%
      select(RAZON.SOCIAL,CATEGORIA,Dist_V) %>%
      arrange(Dist_V)
    datatable(step1_aux1,filter = 'top',options = list(dom="lt", pageLength = 10),rownames = F)
  })

  output$resumen_convenios2 <- DT::renderDataTable({
    aux1 <- bd_c2()
    step1_aux1 <- aux1 %>%
      select(RAZON.SOCIAL,CATEGORIA,Dist_V) %>%
      arrange(Dist_V)
    datatable(step1_aux1,filter = 'top',options = list(dom="lt", pageLength = 10),rownames = F)
  })

  # output$resumen_consumos <- DT::renderDataTable({
  #   aux_consumo <- ventas[ventas$TARJETA %in% data_f2()$Tarjeta,]
  #   datatable(aux_consumo,filter = 'top',options = list(dom="lt", pageLength = 10),rownames = F) %>%
  #     formatCurrency("VALOR", currency = "$", interval = 3, mark = ",",
  #                    digits = 0, dec.mark = getOption("OutDec"))
  # })
  
  
  
  
  
})
