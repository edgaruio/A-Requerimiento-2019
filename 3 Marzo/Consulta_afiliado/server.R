# Aplicacion empresas

shinyServer(function(input, output, session) {
  
  ######## PRIMERA VENTANA ===========================================================================
  
  # Funcion para cargar el archivo subido por el usuario.
  data<-eventReactive(input$go2,{
    req(input$file1)
    df <- fread(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                na.strings = c("", "NA", "#/NA", "na"))
    return(df)
  })

  # Validacion del archivo
  error<-reactive({

    error=character()
    nom_esp<-c('id_persona')
    x=data()

    # Verificacion de nombres de Columnas
    cond1<-sum(names(x) != nom_esp)>0
    if(cond1){
      error=rbind(error,paste0("ERROR: La(s) variable(s): ",paste(names(x)[names(x) != nom_esp],collapse = ","),
                               " no coincide(n) con el nombre esperado","\n"))
    }

    if(length(error)==0){
      error=paste0("Consulta la base de datos de ", dim(x)[1], " registros..." )
    }

    return(error)
  })

  # Calificacion Masiva
  calif<-reactive({

    #if (error()==paste0("Consulta la base de datos de ", dim(data())[1], " registros..." )){

      x=data()
      names(x)<-c("id")

      step1 <- x %>%
        left_join(persona %>% select(-tipo_id), by = c("id"="id_persona"))
      return(step1)
    #}
  })

  # Salidas
  output$Nota <- renderText({
    error()
  })

  output$Preview <- renderDataTable({
    datatable(data(), options=list(dom="lt",searching=T, scrollY = TRUE), rownames=F, 
              colnames = c("Id Persona"))
  })

  output$Calificada <- renderDataTable({
    datatable(calif(),
              options=list(dom="lt",searching=T, scrollY = TRUE), rownames=F,
              colnames = c("ID","Categoria","Segmento","Id Empresa","Razon Social","Piramide 1","Piramide 2","Id Empresa Principal","Empresa Principal")
              )
  })

  output$Cruces <- renderValueBox({
    valueBox(
      value = formatC(dim(data())[1] - sum(is.na(calif()$id_empresa)),format="d"),
      subtitle = "Total Cruce",
      icon = icon("check"),
      color = "blue"
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Descarga", ".csv", sep = "")
    },
    content = function(file) {
      fwrite(calif(), file, row.names = FALSE)
    }
  )


  # # ######## SEGUNDA VENTANA ==================================================================================================================


  data_f <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$identificacion))
    data_f <- persona %>%
      filter(id_persona == id)
    return(data_f)
  })

  output$info_empresa <- renderValueBox({
    valueBox(
      value = formatC(data_f()$razon_social[1],format="s"),
      subtitle = "Empresa",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$info_pir1 <- renderValueBox({
    valueBox(
      value = formatC(data_f()$Segmento_poblacional[1],format="s"),
      subtitle = "Segmento",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_pir2 <- renderValueBox({
    valueBox(
      value = formatC(data_f()$categoria[1],format="s"),
      subtitle = "Piramide 2",
      icon = icon("home"),
      color = "blue"
    )
  })

  # output$conteo_empleados <- renderValueBox({
  #   data_f<-data_f() %>%
  #     select(id_empresa,Afiliados) %>%
  #     group_by(id_empresa) %>%
  #     summarise(Afiliados = sum(Afiliados))
  #   valueBox(
  #     value = formatC(data_f$Afiliados,digits = 0, format = "d", big.mark=","),
  #     subtitle = "Total Empleados",
  #     icon = icon("child"),
  #     color = "blue"
  #   )
  # })
  #
 
  
})



