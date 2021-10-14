library(ggplot2)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(jsonify)
library(magrittr)
library(lubridate)
library(plotly)
library(dygraphs)
library(mapdeck)
library(sf)
library(tidyverse)
library(jsonlite)
library(DT)
library(reticulate)
library(ramify)

reticulate::use_python("py38")
py_available()

# Change project directory
getwd()
setwd("~/Documents/Time_Series_Approximation")
os <- import("os")
os$listdir(".")

`%!in%` <- Negate(`%in%`)

# py_run_file('Coord_converter.py')
coordinates = readxl::read_excel("./Coordinates/Fields_coords.xlsx")
polygon <- coordinates %>%
  group_by(Месторождение) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

coord_pipes = readxl::read_excel("./Coordinates/Pipelines_coords.xlsx")
traces <- coord_pipes %>%
  group_by(Pipeline) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

coord_companies = readxl::read_excel("./Coordinates/Companies_coords.xlsx")
coord_comp <- coord_companies %>%
  group_by(Месторождение) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("MULTIPOINT")


# Run approximation Python script
calc_funcs_py <- import("Calculation_funcs")
pert_distr <- import("pert_func")
# source_python('Main.py')
# py_run_file('Main.py')
data <- read.csv('approximation_result.csv')
# head(data)

prediction = 3 * 365
approx_error_period = 3 * 365

data <- mutate(data, Дата = as.Date(Дата))
data <- data %>% mutate(Combine_prod = if_else(is.na(Добыча), Approximation, Добыча))
data <- data %>% mutate(Месторождение = if_else(Месторождение == 'Независимые производители', 'Независимые произв.', Месторождение))
colnames(data)[9:10] <- c('approx_err_low', 'approx_err_high')

top_data <- data %>% group_by(Месторождение, Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation)) %>% mutate(Дата = as.Date(Дата))
top_data <- top_data[top_data$Дата >= max(top_data$Дата) - 6*365 & !is.na(top_data$Добыча),] %>% group_by(Месторождение) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation))
top_data = top_data[top_data$Месторождение %!in% list('Независимые произв.', 'Нефтяные компании',
                                                      'АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)', 'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)', 'ОАО Томскнефть (ГП 50%)'),]
top_data = top_data[order(-top_data$Добыча),]

df <- data %>% group_by(Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation)) %>% mutate(Дата = as.Date(Дата))
df <- mutate(df, Residuals = Добыча - Approximation, Month = month(df$Дата))
approx_err = data.frame(calc_funcs_py$variation(df, df[(nrow(df) - approx_error_period - prediction):nrow(df),], alpha=0.05, approx_variation=TRUE))
df <- mutate(df, approx_err_low = approx_err[,length(df)+1], approx_err_high = approx_err[,length(df)+2])

consumption <- read.csv('consumption_result.csv')
consumption <- mutate(consumption, Дата = as.Date(Дата))
consumption <- rename(consumption, c('Собственные_нужды'='Собственные.нужды'))
colnames(consumption)[9:10] <- c('approx_err_low', 'approx_err_high')



# !!! SHINY APPLICATION !!!
ui <- dashboardPage(
  dashboardHeader(title = "TESTING PAGE!"),
  dashboardSidebar(
    width = 300,
    tags$head(tags$style(HTML(type = "text/css", "#sidebarCollapsed {margin-left: 5px; }")),
              tags$style(HTML(type = "text/css", ".item {font-size: 16px; }")),
              tags$style(HTML(type = "text/css", ".selectize-dropdown.single.plugin-selectize-plugin-a11y {font-size: 16px; }"))
    ),
    selectInput("field",
                label ='',
                choices = unique(data$Месторождение)
    ),
    sliderInput("dateslider",
                label ='',
                value = c(min(df$Дата), max(df$Дата)),
                min = min(df$Дата),
                max = max(df$Дата),
                width = '95%'
    ),
    dateRangeInput("daterange",
                   label ='',
                   start = min(df$Дата),
                   end = max(df$Дата)
    ),
    tags$head(tags$style(HTML(type = "text/css", ".shiny-html-output .shiny-table {font-size: 14pt; margin-left: 25px; }")),
              tags$style(HTML(type = "text/css", ".table .shiny-table {padding-left: 25px; }")),
              tags$style(HTML(type = "text/css", "#table_name {font-weight: bold; font-size: 16pt; color: lightskyblue; padding-top: 35px; }")),
              tags$style(HTML(type = "text/css", "#hist_name {font-weight: bold; font-size: 14pt; color: lightskyblue; padding-top: 35px; }")),
              tags$style(HTML(type = "text/css", "#hist {font-weight: bold; font-size: 14pt; color: lightskyblue; padding-left: 13px; }"))
    ),
    
    textOutput("table_name"),
    fluidRow(tableOutput("stat_table")),
    
    textOutput("hist_name"),
    fluidRow(plotOutput("hist"))
    
  ),
  dashboardBody(
    tags$head(tags$style(HTML(type = "text/css", '.skin-blue .main-sidebar {font-size: 14pt; }')),
              tags$style(HTML(type = "text/css", '.irs--shiny .irs-grid-text { color: #fffbfb; }')),
              tags$style(HTML(type = "text/css", ".content-wrapper {background-color: #222d32 }"))
    ),
    
    fluidRow(
      column(12,
             mapdeckOutput(outputId = 'map', height = 600),
             
             fluidRow(
               column(6, plotlyOutput("plot_sum", height = 300)),
               column(6, plotlyOutput("plot_sep", height = 300))
             )
      )
    )
  )
)

server <- function(input, output, session) {
  
  reactdelay <- 1
  change_dateslider <- reactiveVal(Sys.time())
  change_daterange <- reactiveVal(Sys.time())
  
  observeEvent(input$dateslider, {
    if (difftime(Sys.time(), change_dateslider()) > reactdelay) {
      change_daterange(Sys.time())
      updateDateRangeInput(session,
                           "daterange",
                           start = input$dateslider[[1]],
                           end = input$dateslider[[2]])
    }
  })
  
  observeEvent(input$daterange, {
    if (difftime(Sys.time(), change_daterange()) > reactdelay) {
      change_dateslider(Sys.time())
      updateSliderInput(session,
                        "dateslider",
                        value = c(input$daterange[[1]], input$daterange[[2]]))
    }
  })
  
  # TABLE WITH STATISTICS
  stats_table <- reactive({data.frame(Год=integer(),
                                      Мин=double(), 
                                      Среднее=double(),
                                      Макс=double(),
                                      stringsAsFactors=FALSE) %>%
      add_row(Год = lubridate::year(Sys.Date()) - 1,
              Мин = NA,
              Среднее = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()) - 1, ]$Approximation / 1e6), digits=2),
              Макс = NA
      ) %>%
      add_row(Год = lubridate::year(Sys.Date()),
              Мин = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()), ]$Approximation / 1e6 + 
                                df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()), ]$approx_err_low / 1e6), digits=2),
              Среднее = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()), ]$Approximation / 1e6), digits=2),
              Макс = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()), ]$Approximation / 1e6 + 
                                 df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()), ]$approx_err_high / 1e6), digits=2)
      ) %>%
      add_row(Год = lubridate::year(Sys.Date()) + 1,
              Мин = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()) + 1, ]$Approximation / 1e6 + 
                                df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()) + 1, ]$approx_err_low / 1e6), digits=2),
              Среднее = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()) + 1, ]$Approximation / 1e6), digits=2),
              Макс = round(sum(df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()) + 1, ]$Approximation / 1e6 + 
                                 df[lubridate::year(df$Дата) == lubridate::year(Sys.Date()) + 1, ]$approx_err_high / 1e6), digits=2)
      )
  })

  output$table_name <- renderText({"Суммарная добыча"})
  output$stat_table <- renderTable({stats_table()}, align='c', digits=0)
  
  histdf <- reactive({
    data[as.integer(names(argmax(subset(data, Месторождение == input$field & if (month(Sys.time()) > 3) {
      Дата >= as.Date(paste(c(toString(year(Sys.time())), '06-01'), collapse='-'), format="%Y-%m-%d") & Дата <= as.Date(paste(c(toString(year(Sys.time()) + 1), '06-01'), collapse='-'), format="%Y-%m-%d")} else {
      Дата >= as.Date(paste(c(toString(year(Sys.time()) - 1), '06-01'), collapse='-'), format="%Y-%m-%d") & Дата <= as.Date(paste(c(toString(year(Sys.time())), '06-01'), collapse='-'), format="%Y-%m-%d")
    }), rows=F)$Combine_prod)), c("Дата", "Combine_prod", "approx_err_low", 'approx_err_high')]
  })
  
  # HISTOGRAMM WITH GAS PEAK BALANCE
  output$hist_name <- renderText({paste(c("Пиковый баланс", toString(histdf()$Дата)), collapse=' ')})
  output$hist <- renderPlot({ggplot(data.frame(x = pert_distr$pert_function(a=(histdf()$Combine_prod + histdf()$approx_err_low) / 1000, c=(histdf()$Combine_prod + histdf()$approx_err_high) / 1000, mu=(histdf()$Combine_prod) / 1000, amount=1000000), y = c(rep("x", 1000000))), aes(x)) + 
      geom_density(fill='lightblue', color='#1faee9', alpha=0.7) + xlab('Добыча, млн м3') +
      theme(
        plot.background = element_rect(color=NA, fill="#222d32"),
        panel.background = element_rect(fill="#3d3d3d", colour="#3d3d3d", size=0.5, linetype="solid"),
        panel.grid.major = element_line(size=0.5, linetype='solid', colour="#bdbdbd"), 
        panel.grid.minor = element_line(size=0.25, linetype='solid', colour="#bdbdbd"),
        axis.text.x = element_text(face="bold", color="white", size=10),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_blank()
      )
  }, width = 290, height = 315)

  # SINGLE CURVE PLOT (LEFT) 
  subdf <- reactive({ 
    subset(data, Месторождение == input$field & Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  })
  subdf_approx <- reactive({ 
    subset(data, Месторождение == input$field & Дата >= input$dateslider[1] & Дата <= input$dateslider[2] & Дата <= df$Дата[nrow(df)-prediction])
  })
  subdf_predict <- reactive({ 
    subset(data, Месторождение == input$field & Дата >= input$dateslider[1] & Дата <= input$dateslider[2] & Дата >= df$Дата[nrow(df)-prediction])
  })
  
  subcons <- reactive({ 
    subset(consumption, Месторождение == input$field & Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  })
  subcons_approx <- reactive({ 
    subset(consumption, Месторождение == input$field & Дата >= input$dateslider[1] & Дата <= input$dateslider[2] & Дата <= df$Дата[nrow(df)-prediction])
  })
  subcons_predict <- reactive({ 
    subset(consumption, Месторождение == input$field & Дата >= input$dateslider[1] & Дата <= input$dateslider[2] & Дата >= df$Дата[nrow(df)-prediction])
  })
  
  output$plot_sum <- renderPlotly({
    plot_ly() %>% 
      add_trace(data = subdf(), x = ~Дата, y = ~Добыча,
                name = 'Добыча', type="scatter", mode = 'lines', line = list(color = 'rgb(29, 172, 214)', width = 2)) %>% 
      add_trace(data = subcons(), x = ~Дата, y = ~Собственные_нужды, yaxis = 'y2',
                name = 'С/н', type="scatter", mode = 'lines', line = list(color = 'rgb(219, 94, 26)', width = 2)) %>% 
      
      add_trace(data = subdf_predict(), x = ~Дата, y = ~Approximation + approx_err_high,
                name = paste0('Макс. Д.'), showlegend = FALSE, type="scatter", mode = 'lines', line=list(color='#1f75fe', width = 0)) %>%
      add_trace(data = subdf_predict(), x = ~Дата, y = ~Approximation + approx_err_low,
                name = paste0('Мин. Д.'), showlegend = FALSE, type="scatter", mode = 'lines', fill='tonexty', fillcolor='rgba(77,255,193,0.2)', line=list(color='transparent')) %>% 
      
      add_trace(data = subcons_predict(), x = ~Дата, y = ~Approximation + approx_err_high, yaxis = 'y2',
                name = paste0('Макс. c/н'), showlegend = FALSE, type="scatter", mode = 'lines', line=list(color='#ad4a15', width = 0 )) %>%
      add_trace(data = subcons_predict(), x = ~Дата, y = ~Approximation + approx_err_low, yaxis = 'y2',
                name = paste0('Мин. c/н'), showlegend = FALSE, type="scatter", mode = 'lines', fill='tonexty', fillcolor='rgba(255,195,75,0.3)', line=list(color='transparent')) %>% 
      
      add_trace(data = subdf_predict(), x = ~Дата, y = ~Approximation,
                name = paste0('Добыча'), type="scatter", mode = 'lines', line = list(width = 2, color = 'rgb(29, 172, 214)')) %>% 
      add_trace(data = subcons_predict(), x = ~Дата, y = ~Approximation, yaxis = 'y2',
                name = paste0('С/н'), type="scatter", mode = 'lines', line = list(width = 2, color = 'rgb(219, 94, 26)')) %>% 
      
      layout(title = list(text = "Профиль добычи", y = 0.96, x = 0.47, xanchor = 'center', yanchor =  'top', font=list(color='white')),
             paper_bgcolor="#222d32",
             plot_bgcolor="#3d3d3d",
             xaxis = list(title=list(font=list(size=14, color='white')),
                          gridcolor = 'rgb(100,100,100)',
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          tickfont = list(color='white', size=10),
                          zeroline = FALSE),
             yaxis = list(title=list(text="Добыча, тыс. м3",
                                     font=list(size=14, color='#b8fff3')),
                          gridcolor = 'rgb(100,100,100)',
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          tickfont = list(color='#b8fff3', size=10),
                          zeroline = FALSE,
                          side = 'right',
                          range = list(0, max(subdf()$Approximation, subdf_approx()$Approximation, subdf_predict()$Approximation) * 1.1),
                          dtick = max(subdf()$Approximation, subdf_approx()$Approximation, subdf_predict()$Approximation) * 1.1 / 4),
             yaxis2 = list(title=list(text="Потребление, тыс. м3",
                                      font=list(size=14, color='#fac6be')),
                           gridcolor = 'rgb(100,100,100)',
                           showgrid = TRUE,
                           showline = TRUE,
                           showticklabels = TRUE,
                           tickcolor = 'rgb(127,127,127)',
                           ticks = 'outside',
                           tickfont = list(color='#fac6be', size=10),
                           overlaying = "y",
                           side = "left",
                           range = list(0, max(subdf()$Approximation, subdf_approx()$Approximation, subdf_predict()$Approximation) * 0.03),
                           dtick = max(subdf()$Approximation, subdf_approx()$Approximation, subdf_predict()$Approximation) * 0.03 / 4)) %>%
      layout(showlegend = F, hovermode = "x", 
             xaxis = list(title = NA,
                          rangeselector = list(buttons=list(list(count=1 + prediction/365, label="1+ год", step="year", stepmode="backward"),
                                                            list(count=5 + prediction/365, label="5+ лет", step="year", stepmode="backward"),
                                                            list(label="Весь ряд", step="all")
                          )
                          )
             )
      )
  })
  
  # SUMMARY PLOT (RIGHT)
  subdata_top <- reactive({ 
    subset(data[data$Месторождение %in% top_data[1:4,]$Месторождение,], Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  })
  
  subdata_low_prod <- reactive({ 
    subset(data[data$Месторождение %in% top_data[5:nrow(top_data),]$Месторождение,], Дата >= input$dateslider[1] & Дата <= input$dateslider[2]) %>% 
      group_by(Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation), Combine_prod = sum(Combine_prod)) %>% mutate(Дата = as.Date(Дата))
  })
  
  subdata_dependent <- reactive({
    subset(data[data$Месторождение %in% list('АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)', 'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)', 'ОАО Томскнефть (ГП 50%)'),], Дата >= input$dateslider[1] & Дата <= input$dateslider[2]) %>% 
      group_by(Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation), Combine_prod = sum(Combine_prod)) %>% mutate(Дата = as.Date(Дата))
  })
  
  subdata_independent <- reactive({
    subset(data[data$Месторождение %in% list('Независимые произв.', 'Нефтяные компании'),], Дата >= input$dateslider[1] & Дата <= input$dateslider[2]) #%>% 
    #  group_by(Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation), Combine_prod = sum(Combine_prod)) %>% mutate(Дата = as.Date(Дата))
  })
  
  output$plot_sep <- renderPlotly({
    plot_ly() %>% 
      add_trace(stackgroup = 'one', data = subdata_independent(), x = ~Дата, y = ~Combine_prod, 
                #type="scatter", mode = 'lines', fillcolor = '#666666', fill='tonexty', line = list(color = '#808080', width = 1), name = 'Независ. + нефт.') %>%
                type="scatter", mode = 'lines', color = ~Месторождение, line = list(width = 1)) %>%
      add_trace(stackgroup = 'one', data = subdata_dependent(), x = ~Дата, y = ~Combine_prod, 
                type="scatter", mode = 'lines', fillcolor = '#7c947c', fill='tonexty', line = list(color = '#98ab98', width = 1), name = 'Совместные предп.') %>%
      add_trace(stackgroup = 'one', data = subdata_low_prod(), x = ~Дата, y = ~Combine_prod, 
                type="scatter", mode = 'lines', line = list(color = "#b0976f", width = 1), name = 'Др. м/р ПАО Газпром') %>%
      add_trace(stackgroup = 'one', data = subdata_top(), x = ~Дата, y = ~Combine_prod, 
                type="scatter", mode = 'lines', color = ~Месторождение, fill='tonexty', line = list(width = 1)) %>%
      layout(title = list(text = "Суммарная добыча", y = 0.96, x = 0.47, xanchor = 'center', yanchor =  'top', font=list(color='white')),
             paper_bgcolor="#222d32",
             plot_bgcolor="#3d3d3d",
             showlegend = T, legend = list(orientation = 'h', font=list(size=10, color='white'), bgcolor='#222d3200'), hovermode = "x", 
             #shapes = list(type = "line", y0 = 0, y1 = 1, yref = "paper",
             #             x0 = df[nrow(df)-prediction,]$Дата, x1 = df[nrow(df)-prediction,]$Дата, line = list(color = 'red', dash="dot", width = 1)),
             xaxis = list(title=list(font=list(size=14, color='white')),
                          tickfont = list(color='white', size=10),
                          rangeselector=list(buttons=list(list(count=1 + prediction/365, label="1+ год", step="year", stepmode="backward"),
                                                          list(count=5 + prediction/365, label="5+ лет", step="year", stepmode="backward"),
                                                          list(label="Весь ряд", step="all")
                          )
                          )
             ),
             yaxis = list(title=list(text="Добыча, тыс. м3",
                                     font=list(size=14, color='white')),
                          tickfont = list(color='white', size=10))
      )
  })
  
  # WORLD MAP, POLYGONS AND LINES
  current_zoom <- eventReactive({input$map_view_change}, {input$map_view_change$zoom})
  
  set_token('pk.eyJ1IjoibmlreXBhcmZlbm92IiwiYSI6ImNrdGw1d3RtczA1NDYyd3A2dTd3M3M4ajYifQ.YhQLodjnRi2A2Kgch8KKgA')
  output$map <- renderMapdeck({
    mapdeck(style = mapdeck_style('dark'),
            pitch = 45,
            location = c(50, 56),
            zoom = 3.5,
    ) %>%
      add_polygon(
        data = polygon,
        layer_id = "polygon_layer",
        fill_colour = '#30d5c8',
        auto_highlight = TRUE,
        highlight_colour = '#ff5f49cc',
        update_view = F,
      ) %>%
      add_scatterplot(
        data = coord_comp,
        lat = "Latitude",
        lon = "Longitude",
        radius = 10000,
        fill_colour = "#ff8c00",
        auto_highlight = TRUE,
        layer_id = "scatter_layer",
        update_view = F,
      ) %>%
      add_path(
        data = traces,
        stroke_colour = '#57b9ff', 
        width_scale = 1,
        layer_id = "pipe_layer",
        #auto_highlight = TRUE,
        #highlight_colour = '#ff5f49cc',
        update_view = F,
      ) %>%
      add_trips(
        data = traces,
        stroke_colour = '#00bfff',
        stroke_width = 5,
        layer_id = "pipe_layer_animation",
        opacity = 0.5,
        trail_length = 60,
        start_time = 0,
        end_time = 90,
        animation_speed = 30
        #        ) %>%
        #        add_text(
        #          data = coord_comp,
        #          lat = "Latitude",
        #          lon = "Longitude",
        #          text = 'Месторождение',
        #          fill_colour = "#ffffff",
        #          size = 20,
        #          update_view = F,
        #          layer_id = 'pipe_text'
      )
  })
  
  # observeEvent({input$map_view_change},{
  # print(input$map_view_change$zoom)
  # })
  #observeEvent({input$map_view_change},{
  #print(current_zoom())
  #})
}

shinyApp(ui, server)
