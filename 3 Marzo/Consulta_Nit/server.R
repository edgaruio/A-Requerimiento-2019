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
    nom_esp<-c('Nit')
    x=data()

    # Verificacion de nombres de Columnas
    cond1<-sum(names(x) != nom_esp)>0
    if(cond1){
      error=rbind(error,paste0("ERROR: La(s) variable(s): ",paste(names(x)[names(x) != nom_esp],collapse = ","),
                               " no coincide(n) con el nombre esperado","\n"))
    }

    if(length(error)==0){
      error=paste0("Calificando la base de datos de ", dim(x)[1], " registros..." )
    }

    return(error)
  })

  # Calificacion Masiva
  calif<-reactive({

    # if (error()==paste0("Calificando la base de datos de ", dim(data())[1], " registros..." )){

      x=data()
      names(x)<-c("Nit")

      step1 <- x %>%
        left_join(bd_empresas, by = c("Nit"="id_empresa"))
      return(step1)
    # }
  })

  # Salidas
  output$Nota <- renderText({
    error()
  })

  output$Preview <- renderDataTable({
    datatable(data(), options=list(dom="lt",searching=T, scrollY = TRUE), rownames=F, colnames = c("Nit"))
  })

  output$Calificada <- renderDataTable({
    datatable(calif(), options=list(dom="lt",searching=T, scrollY = TRUE), rownames=F, colnames = c("Nit","Razon Social","Piramide 1","Piramide 2"))
  })

  output$Cruces <- renderValueBox({
    valueBox(
      value = formatC(dim(data())[1] - sum(is.na(calif()$razon_social)),format="d"),
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


  # ######## SEGUNDA VENTANA ==================================================================================================================


  data_f <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    data_f <- bd_empresas %>%
      filter(id_empresa == id)
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
      value = formatC(data_f()$piramide_1[1],format="s"),
      subtitle = "Piramide 1",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_pir2 <- renderValueBox({
    valueBox(
      value = formatC(data_f()$piramide_2[1],format="s"),
      subtitle = "Piramide 2",
      icon = icon("home"),
      color = "blue"
    )
  })

})



