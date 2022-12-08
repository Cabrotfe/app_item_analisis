pacman::p_load(shiny,shinydashboard, readxl,tidyverse,ggpubr)




ui <- dashboardPage(
  dashboardHeader(title = "Aplicación para analizar ítems",titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(## Acá ponemos lo que está en la barra izquierda
      menuItem(text = "Instrucciones", tabName = "intro", icon = icon("dashboard")),
      radioButtons(inputId = "data_t", "Selecciona el tipo de datos",choices = c(excel = ".xlsx",tab="\t",csv="," )),
      fileInput(inputId = "data_f",label = "Carga de archivo",buttonLabel = "Cargar"),
      menuItem(text = "Observación datos", tabName = "observ", icon = icon("th")),
      menuItem(text = "Pantalla de análisis", tabName = "descriptivos"),
      menuItem(text = "Análisis de ítems", tabName = "analisis_items")
    )
  ),
  dashboardBody(
    tabItems( ### Eso es para crear los ítems. 
      tabItem(tabName = "intro", ## Acá va el nombre de lo que se pone en la sidebar
              fluidRow(h2("Bienvenidos a la aplicación de análisis de ítems")), ## Acá decimos que en una caja ponga el output
              fluidRow(h3("Para utilizarla debes cargar archivos. Selecciona el tipo de archivo, y cárgalo para que puedas comenzar a analizar los datos"))
      ),
      tabItem(tabName = "observ", fluidRow(box(tableOutput("mostrar_datos"),width = 12))), ## Aquí llamo al menuItem
      tabItem(tabName = "descriptivos",
              fluidRow(column(width = 4,selectInput("items_utilizar",label = "¿Qué ítems quieres considerar?",choices = c("Selecciona una base de datos"), multiple = T)),
                       column(width = 2,selectInput(inputId = "pnt",label = "¿Cómo quieres ver el puntaje",choices = c(`puntaje bruto` = "pb",`porcentaje de logro`="pl",`escala de notas`="not"))),
                       column(width = 2,conditionalPanel(condition = "input.pnt == 'not'",sliderInput("escala_notas",min = 0,max = 10,step = 0.1,value = c(1,7),label = "Escala"))),
                       column(width = 1,conditionalPanel(condition = "input.pnt == 'not'",numericInput("reprobacion",value = 4,label = "Reprobación"))),
                       column(width = 1,conditionalPanel(condition = "input.pnt == 'not'",numericInput("exigencia",value = 0.6,label = "Exigencia"))),
                       column(width = 1,conditionalPanel(condition = "input.pnt == 'not'",numericInput("máximo",value = 0,label = "Máximo")))),
              fluidRow(column(width = 5,plotOutput(outputId = "histograma", height = 200)),
                       column(width = 5,tableOutput(outputId = "descriptivos_puntos")))),
      tabItem(tabName = "analisis_items")
    )
  )
)



server <- function(input, output, session) {
  
  
  data_file = reactive({ ## 1: crear reactive y poner condiciones de acuerdo al radio buttons
    if(is.null(input$data_f)){ ## Hay que poner esto, de lo contrario genera un error todo el tiempo
      return() ## arrojar ningún output si no hay nada en el input
    } else if(input$data_t %in% c(",","\t")){
      file_path = input$data_f
      aa = read.table(file = file_path$datapath, header=T, sep = input$data_t)
      return(aa)
    } else if(input$data_t %in% c(".xlsx")){
      file_path = input$data_f
      aa = read_excel(path = file_path$datapath)
      return(aa)
    }
  })
  
  output$mostrar_datos = renderTable({
    if(is.null(input$data_f)){ ## Hay que poner esto, de lo contrario genera un error todo el tiempo
      return()
    } else{
      head(data_file(),5)
    }
  })
  
  observeEvent(is.null(data_file()==F), {
    updateSelectInput(inputId = "items_utilizar", choices = colnames(data_file()))
  })
  
  ### Esto crea un vector de puntajes
  puntaje = reactive({
    bla = rowSums(data_file()[,c(input$items_utilizar)])
    if(input$pnt=="pb"){
      bla
    } else if(input$pnt=="pl"){
      bla = (bla/length(input$items_utilizar))*100
    }
    return(bla)
  })
  
  ########3 Esto crea puntajes dentro de un data.frame
  base_puntos = reactive({
    bla = rowSums(data_file()[,c(input$items_utilizar)])
    if(input$pnt=="pb"){
      bla
    } else if(input$pnt=="pl"){
      bla = (bla/length(input$items_utilizar))*100
    } else if(input$pnt=="not"){
      bla = ifelse(bla>=input$máximo*input$exigencia,
                   ((input$escala_notas[2] - input$reprobacion)*(bla-input$exigencia*input$máximo)/(input$máximo*(1-input$exigencia))+input$reprobacion),
                   ((input$reprobacion - input$escala_notas[1])*(bla)/(input$máximo*input$exigencia)+input$escala_notas[1]))
    }
    base_nueva = data.frame(cbind(data_file(), puntaje = bla))
    return(base_nueva)
  })
  
  
  
  output$histograma = renderPlot({
    base_puntos() %>% ggdensity(x="puntaje",add = "mean",xlab = "Puntajes",rug=T)
  })
  
  output$descriptivos_puntos = renderTable({
    promedio = round(mean(base_puntos()$puntaje,na.rm=T),1)
    mediana = round(median(base_puntos()$puntaje,na.rm=T),1)
    des = round(sd(base_puntos()$puntaje,na.rm=T),1)
    maximo = round(max(base_puntos()$puntaje,na.rm=T),1)
    minimo = round(min(base_puntos()$puntaje,na.rm=T),1)
    data.frame(cbind(estadístico = c("Promedio","Mediana","Desviación estándar","máximo","mínimo"),valor =c(promedio,mediana,des,maximo,minimo)))
  })
  
}



shinyApp(ui, server)
