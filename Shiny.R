library(ggplot2)
library(ggridges)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycustomloader)
library(jsonify)
library(magrittr)
library(lubridate)
library(plotly)
library(mapdeck)
library(sf)
library(sp)
library(jsonlite)
library(DT)
library(reticulate)
library(ramify)
library(mc2d)
library(R.oo)
require(visNetwork, quietly = TRUE)

# Add python abilities
reticulate::use_python("py38")
py_available()

# Change project directory
# getwd()
# setwd("~/Documents/Time_Series_Approximation")
# os <- import("os")
# os$listdir(".")

# MAIN PRODUCTION APPROXIMATION, FORECAST AND PERT DISTR. SCRIPT
# py_run_file('Main.py')
# pert_distr <- import("pert_func")

pert_distr <- function(a, c, b=NaN, mu=NaN, lamb=4, amount=100000) {
  if (!is.nan(mu) & is.nan(b)) {
    b = (mu * (lamb + 2) - a - c) / lamb
  }
  if (a == c) {
    a = b - 0.001 * b
    c = b + 0.001 * b
  }
  while (b > c) {
    lamb = lamb + 1
    b = (mu * (lamb + 2) - a - c) / lamb
  }
  while (b < a) {
    lamb = lamb - 1
    b = (mu * (lamb + 2) - a - c) / lamb
  }
  out <- tryCatch(
    {
      pert  <-  rpert(amount, min=a, mode=b, max=c, shape=lamb)
    },
    error=function(e) {
      pert = NaN
      print('Error in b/mu parameter')
    })
  return(pert)
}

LatLongToMerc <- function(lon, lat)
{
  rLat = lat * pi / 180;
  rLong = lon * pi / 180;
  a = 6378137.0;
  b = 6356752.3142;
  f = (a - b) / a;
  e = sqrt(2 * f - f^2);
  X = a * rLong;
  Y = a * log(tan(pi / 4 + rLat / 2) * ((1 - e * sin(rLat))/(1 + e * sin(rLat)))^(e / 2));
  return(list('x' = X, 'y' = Y));
}

# Add negative 'in' for more convenience
`%!in%` <- Negate(`%in%`)

# BLOCK OF COORDINATES PREPARING
# Regions coordinates
regions <- readRDS("./Coordinates/sf_russia_map_clean_simple.rds")

region_centroids_file <- readxl::read_excel("./Coordinates/macro_graph.xlsx", sheet = 'coords')
graph_regions_file = readxl::read_excel("./Coordinates/macro_graph.xlsx", sheet="graph") 

graph_regions <- merge(graph_regions_file, region_centroids_file, by.x=c('Start'), by.y=c('Region')) %>% 
  rename(c('lat_begin'='Latitude', 'lon_begin'='Longitude'))
graph_regions <- merge(graph_regions, region_centroids_file, by.x=c('End'), by.y=c('Region')) %>% 
  rename(c('lat_end'='Latitude', 'lon_end'='Longitude'))
graph_regions <- subset(graph_regions, lat_begin != 0 & lat_end != 0) %>%
  mutate(x_begin = LatLongToMerc(graph_regions$lon_begin, graph_regions$lat_begin)$x,
         y_begin = LatLongToMerc(graph_regions$lon_begin, graph_regions$lat_begin)$y,
         x_end = LatLongToMerc(graph_regions$lon_end, graph_regions$lat_end)$x,
         y_end = LatLongToMerc(graph_regions$lon_end, graph_regions$lat_end)$y) %>%
  mutate(Distance = sqrt((x_begin-x_end)^2 + (y_begin-y_end)^2))

region_centroids <- region_centroids_file %>%
  group_by(Region) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("MULTIPOINT") %>%
  merge(region_centroids_file, by.x=c('Region'), by.y=c('Region'))
region_centroids <- mutate(region_centroids, 
                           x = LatLongToMerc(region_centroids$Longitude,region_centroids$Latitude)$x,
                           y = LatLongToMerc(region_centroids$Longitude,region_centroids$Latitude)$y)

# Field coordinates
coordinates = readxl::read_excel("./Coordinates/Fields_coords.xlsx")
polygon <- coordinates %>%
  group_by(Месторождение) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Pipes coordinates
coord_pipes = readxl::read_excel("./Coordinates/Pipelines_coords.xlsx")
traces <- coord_pipes %>%
  group_by(Pipeline) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

# Compressors coordinates
coord_compressors = readxl::read_excel("./Coordinates/Graph.xlsx", sheet="Compressors")
compressors <- coord_compressors %>%
  group_by(Compressor, Name) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("MULTIPOINT")

# Compressors graph
normalize <- function(x, k = 4, na.rm = TRUE) {
  return ( as.numeric(unlist((k-1) * (x-min(x))/(max(x)-min(x)) + 1 )))
  }
graph_compressors = readxl::read_excel("./Coordinates/Graph.xlsx", sheet="Graph")
graph_compressors <- merge(graph_compressors, coord_compressors, by.x=c('Начало'), by.y=c('Compressor')) %>% 
  rename(c('lat_begin'='Latitude', 'lon_begin'='Longitude'))
graph_compressors <- merge(graph_compressors, coord_compressors, by.x=c('Конец'), by.y=c('Compressor')) %>%
  rename(c('lat_end'='Latitude', 'lon_end'='Longitude')) 
graph_compressors <- subset(graph_compressors, lat_begin != 0 & lat_end != 0) %>%
  mutate(graph_compressors, stroke=normalize(graph_compressors['Годовая производительность, млрд м3']))

# Pipes connected with fields
coord_pipes_to_field = readxl::read_excel("./Coordinates/Pipelines_coords_to_field.xlsx")
pipes_to_field <- coord_pipes_to_field %>%
  group_by(Pipeline, Месторождение) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

# Point coordinates (companies and some industrial systems)
coord_ugsys = readxl::read_excel("./Coordinates/UGS_coords.xlsx")
coord_ugs <- coord_ugsys %>%
  group_by(ПХГ, Name) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("MULTIPOINT")

# PREDICTION PERIOD 3 YEARS IS OPTIMAL
prediction = 3 * 365
approx_error_period = 3 * 365

# BLOCK OF PRODUCTION DATA READING AND PREPARING
# Approximation data preparing
data <- read.csv('approximation_result.csv')
data <- mutate(data, Дата = as.Date(Дата))
data <- data %>% mutate(Combine_prod = if_else(is.na(Добыча), Approximation, Добыча))
data <- data %>% mutate(Месторождение = if_else(Месторождение == 'Независимые производители', 'Независимые произв.', Месторождение))
colnames(data)[9:10] <- c('approx_err_low', 'approx_err_high')
data <- data %>% mutate(Combine_prod_low = if_else(is.na(Добыча), Approximation + approx_err_low, Добыча)) %>% 
  mutate(Combine_prod_low = if_else(Combine_prod_low < 0, 0, Combine_prod_low))
data <- data %>% mutate(Combine_prod_high = if_else(is.na(Добыча), Approximation + approx_err_high, Добыча))


# Preparing data for right bottom graph (sorting for top production fields extraction)
top_data <- data %>% group_by(Месторождение, Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation)) %>% mutate(Дата = as.Date(Дата))
top_data <- top_data[top_data$Дата >= max(top_data$Дата) - 6*365 & !is.na(top_data$Добыча),] %>% group_by(Месторождение) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation))
top_data = top_data[top_data$Месторождение %!in% list('Независимые произв.', 'Нефтяные компании',
                                                      'Суммарная добыча', 'ПАО Газпром', 'ПАО Газпром + СП', 'Совм. предпр.', 'НП + НК', 'НП + НК + СП',
                                                      'АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)', 'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)', 'ОАО Томскнефть (ГП 50%)'),]
top_data = top_data[order(-top_data$Добыча),]

# Field order for Input Choser
field_order <- data.frame(Месторождение=c('Суммарная добыча', 'ПАО Газпром', 'ПАО Газпром + СП', top_data$Месторождение[1:20],
                                          'Совм. предпр.', 'АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)', 'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)', 'ОАО Томскнефть (ГП 50%)',
                                          top_data$Месторождение[20:nrow(top_data)], 'НП + НК', 'НП + НК + СП', 'Независимые произв.', 'Нефтяные компании'))

# Preparing of consumption data
consumption <- read.csv('consumption_result.csv')
consumption <- mutate(consumption, Дата = as.Date(Дата))
consumption <- rename(consumption, c('Собственные_нужды'='Собственные.нужды'))
colnames(consumption)[9:10] <- c('approx_err_low', 'approx_err_high')


# Trends list obtained by analytical calculations
# trends <- read.csv('Production_forecast.csv')


#===============================================================================================================================
# !!! SHINY APPLICATION !!!
ui <- dashboardPage(
  dashboardHeader(title = "Добыча в ЕСГ"),
  dashboardSidebar(
    width = 300,
    tags$head(tags$style(HTML(type = "text/css", "#sidebarCollapsed {margin-left: 5px; }")),
              tags$style(HTML(type = "text/css", ".item {font-size: 16px; }")),
              tags$style(HTML(type = "text/css", ".selectize-dropdown.single.plugin-selectize-plugin-a11y {font-size: 16px; }"))
    ),  

    selectInput("field",
                label ='',
                choices = unique(field_order$Месторождение)
    ),

    sidebarMenu(
      id = "tabs",
      menuItem(strong("Детальная карта", style = 'padding-left: 10px;'), tabName = "detailed_map", icon = icon("map")),
      menuItem(strong("Макро-граф", style = 'padding-left: 10px;'), tabName = "macro_graph", icon = icon("sitemap"))
    ),
    
#    tags$head(tags$style(HTML(type = "text/css", ".irs--shiny .irs-handle {width: 16px; height: 16px; top: 20px; }"))
#    ),
#    sliderInput("dateslider",
#                label ='',
#                value = c(min(data$Дата), max(data$Дата)),
#                min = min(data$Дата),
#                max = max(data$Дата),
#                width = '95%'
#    ),
    
    checkboxGroupInput("ugss_display",
                       "Отображение элементов:",
                       c("Газопроводы"="pipe", 
                         "Графовая  структура"="graph", 
                         "Регионы РФ"="regions"),
                       selected = 'pipe',
    ),    
    
    tags$head(tags$style(HTML(type = "text/css", ".shiny-html-output .shiny-table {font-size: 14pt; margin-left: 25px; }")),
              tags$style(HTML(type = "text/css", ".table .shiny-table {padding-left: 25px; }")),
              tags$style(HTML(type = "text/css", ".shiny-loader-output-container {height: 150px; }")),
              tags$style(HTML(type = "text/css", "#ugss_display-label {font-weight: bold; font-size: 14pt; color: lightskyblue; }")),
              tags$style(HTML(type = "text/css", "#table_name {font-weight: bold; font-size: 14pt; color: lightskyblue; padding-top: 10px; }")),
              tags$style(HTML(type = "text/css", "#hist_name {font-weight: bold; font-size: 14pt; color: lightskyblue; padding-top: 25px; }")),
              tags$style(HTML(type = "text/css", "#hist {font-weight: bold; font-size: 14pt; color: lightskyblue; padding-left: 13px; }")),
    ),
    
    textOutput("table_name"),
    fluidRow(withLoader(tableOutput("stat_table"), type='html', loader='loader3')),
    
    textOutput("hist_name"),
    fluidRow(withLoader(plotOutput("hist", height = 320), type='html', loader='loader3'))
    
  ),
  dashboardBody(
    tags$head(tags$style(HTML(type = "text/css", '.skin-blue .main-sidebar {font-size: 14pt; }')),
              tags$style(HTML(type = "text/css", '.irs--shiny .irs-grid-text { color: #fffbfb; }')),
              tags$style(HTML(type = "text/css", ".content-wrapper {background-color: #222d32 }"))
    ),
    tabItems(
      tabItem(tabName = "detailed_map",
              fluidRow(
                column(12, mapdeckOutput(outputId = 'map', height = 600),
                       
                       fluidRow(
                         column(6, withLoader(plotlyOutput("plot_single_graph", height = 300), type='html', loader='loader3')),
                         column(6, withLoader(plotlyOutput("plot_total_graph", height = 300), type='html', loader='loader3'))
                       )
                )
              )
      ),
      tabItem(tabName = "macro_graph",
              h2('Here should be a macro-graph... soon...', style = 'color: white; text-align: center; vertical-align: middle;'),
              withLoader(visNetworkOutput("macro_graph_visnetwork", height = 820), type='html', loader='loader5')
              )
    )
    
  )
)
#===============================================================================================================================
server <- function(input, output, session) {
  
  reactdelay <- 1
#  change_dateslider <- reactiveVal(Sys.time())
#  change_daterange <- reactiveVal(Sys.time())
  
#  observeEvent(input$dateslider, {
#    if (difftime(Sys.time(), change_dateslider()) > reactdelay) {
#      change_daterange(Sys.time())
#      updateDateRangeInput(session,
#                           "daterange",
#                           start = input$dateslider[[1]],
#                           end = input$dateslider[[2]])
#    }
#  })
  
#  observeEvent(input$daterange, {
#    if (difftime(Sys.time(), change_daterange()) > reactdelay) {
#      change_dateslider(Sys.time())
#      updateSliderInput(session,
#                        "dateslider",
#                        value = c(input$daterange[[1]], input$daterange[[2]]))
#    }
#  })
  
  
  df_field <- reactive({ 
    subset(data, Месторождение == input$field)
  })
  
  df_field_slider <- reactive({
    subset(data, Месторождение == input$field)
    # subset(df_field()), Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  })
  
  cons_field <- reactive({ 
    subset(consumption, Месторождение == input$field)
  })
  
  cons_field_slider <- reactive({
    subset(consumption, Месторождение == input$field)
    # subset(cons_field()), Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  })
  
  # TABLE WITH STATISTICS
  stats_table <- reactive({
    req(input$field)
    current_year = year(Sys.Date())
    data.frame(Год = c(toString(current_year - 1), toString(current_year), toString(current_year + 1)), 
                  Мин = c(NA, 
                          sum(subset(df_field(), year(Дата) == current_year)$Combine_prod_low / 1e6), 
                          sum(subset(df_field(), year(Дата) == current_year + 1)$Combine_prod_low / 1e6)),
                  Средн = c(sum(subset(df_field(), year(Дата) == current_year - 1)$Combine_prod / 1e6), 
                            sum(subset(df_field(), year(Дата) == current_year)$Combine_prod / 1e6), 
                            sum(subset(df_field(), year(Дата) == current_year + 1)$Combine_prod / 1e6)), 
                  Макс = c(NA, 
                           sum(subset(df_field(), year(Дата) == current_year)$Combine_prod_high / 1e6), 
                           sum(subset(df_field(), year(Дата) == current_year + 1)$Combine_prod_high / 1e6)))
  })
  
  output$table_name <- renderText({"Суммарная добыча, млрд м3"})
  output$stat_table <- renderTable({stats_table()}, align='c', digits=1)

  # Subset with summarized quarter min/mid/max data
  temp <- reactive({
    subset(data, Месторождение == input$field & 
                   if (month(Sys.time()) > 3) {
                       Дата >= as.Date(paste(c(toString(year(Sys.time())), '10-01'), collapse='-'), format="%Y-%m-%d") &
                       Дата <= as.Date(paste(c(toString(year(Sys.time()) + 1), '03-31'), collapse='-'), format="%Y-%m-%d") } else {
                       Дата >= as.Date(paste(c(toString(year(Sys.time()) - 1), '10-01'), collapse='-'), format="%Y-%m-%d") &
                       Дата <= as.Date(paste(c(toString(year(Sys.time())), '03-31'), collapse='-'), format="%Y-%m-%d") 
         }) %>%
    mutate(Quarter=quarter(Дата), Year=year(Дата)) %>% 
    group_by(Year, Quarter) %>%
    summarize(Min=max(Combine_prod_low), Mean=max(Combine_prod), Max=max(Combine_prod_high)) %>%
    mutate(prod_period=paste(c(toString(Year), toString(Quarter)), collapse='-'))  
  })
  
  # Histogram data relying on Pert-distribution
  df_hist <- reactive({
    rbind(
      data.frame(
        x = pert_distr(a=(temp()$Min)[1] / 1000, c=(temp()$Max)[1] / 1000, mu=(temp()$Mean)[1] / 1000, amount=1000000), 
        y = c(rep(temp()$prod_period[1], 1000000))),
      data.frame(
        x = pert_distr(a=(temp()$Min)[2] / 1000, c=(temp()$Max)[2] / 1000, mu=(temp()$Mean)[2] / 1000, amount=1000000), 
        y = c(rep(temp()$prod_period[2], 1000000)))
    )
})    
  

  # HISTOGRAMM WITH GAS PEAK BALANCE
  output$hist_name <- renderText({paste(c("Пиковый баланс", 
                                          paste(c(toString(temp()$Year[1]), toString(format(as.Date(paste(c(temp()$Year[2], '01-01'), collapse='-'),
                                                                                                  format='%Y-%m-%d'), '%y'))), collapse='-')), collapse=' ')})
  output$hist <- renderPlot({
    ggplot(df_hist(), aes(x, y, fill=y)) +
      stat_density_ridges(quantile_lines = T, calc_ecdf = T, quantiles = c(0.05, 0.95)) +
      scale_fill_manual(values=c('#30d5c8B3', '#1faee9B3')) + 
#      scale_colour_manual(values = c('#00ffff', '#33ccff')) +
      xlab('Добыча, млн м3') +
      theme(
        legend.position = "none",
        plot.background = element_rect(color=NA, fill="#222d32"),
        panel.background = element_rect(fill="#3d3d3d", colour="#3d3d3d", size=0.5, linetype="solid"),
        panel.grid.major = element_line(size=0.5, linetype='solid', colour="#bdbdbd"), 
        panel.grid.minor = element_line(size=0.25, linetype='solid', colour="#bdbdbd"),
        axis.text.x = element_text(face="bold", color="white", size=10),
        axis.text.y = element_text(face="bold", color="white", size=14, angle=90, hjust = -0.5),
        axis.title.x = element_text(color="white", size=14, face="bold"),
        axis.title.y = element_blank()
      )
  }, width = 290, height = 315)
  
  # SINGLE CURVE PLOT (LEFT) 
  # Reactive production subdata
  sub_prod <- reactive({ 
    df_field_slider()
  })
  sub_prod_approx <- reactive({ 
    subset(df_field_slider(), Дата <= df_field()$Дата[nrow(df_field())-prediction])
  })
  sub_prod_predict <- reactive({ 
    subset(df_field_slider(), Дата >= df_field()$Дата[nrow(df_field())-prediction])
  })
  # Reactive consumption subdata
  sub_cons <- reactive({ 
    cons_field_slider()
  })
  sub_cons_approx <- reactive({ 
    subset(cons_field_slider(), Дата <= df_field()$Дата[nrow(df_field())-prediction])
  })
  sub_cons_predict <- reactive({ 
    subset(cons_field_slider(), Дата >= df_field()$Дата[nrow(df_field())-prediction])
  })
  # Trends subdata
  #sub_trend <- reactive({
  #  subset(trends, Месторождение == input$field & Дата >= df_field()$Дата[nrow(df_field())-prediction]) # & Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  #})
  
  output$plot_single_graph <- renderPlotly({
    plot_ly() %>% 
      add_trace(data = sub_cons(), x = ~Дата, y = ~Собственные_нужды, yaxis = 'y2',
                name = 'С/н', type="scatter", mode = 'lines', line = list(color = 'rgb(219, 94, 26)', width = 2)) %>% 
      add_trace(data = sub_prod(), x = ~Дата, y = ~Добыча,
                name = 'Добыча', type="scatter", mode = 'lines', line = list(color = 'rgb(29, 172, 214)', width = 2)) %>% 
      
      add_trace(data = sub_cons_predict(), x = ~Дата, y = ~Approximation + approx_err_high, yaxis = 'y2',
                name = paste0('Макс. c/н'), showlegend = FALSE, type="scatter", mode = 'lines', line=list(color='#ad4a15', width = 0 )) %>%
      add_trace(data = sub_cons_predict(), x = ~Дата, y = ~Approximation + approx_err_low, yaxis = 'y2',
                name = paste0('Мин. c/н'), showlegend = FALSE, type="scatter", mode = 'lines', fill='tonexty', fillcolor='rgba(255,195,75,0.3)', line=list(color='transparent')) %>% 
      
      add_trace(data = sub_prod_predict(), x = ~Дата, y = ~Approximation + approx_err_high,
                name = paste0('Макс. Д.'), showlegend = FALSE, type="scatter", mode = 'lines', line=list(color='#1f75fe', width = 0)) %>%
      add_trace(data = sub_prod_predict(), x = ~Дата, y = ~Approximation + approx_err_low,
                name = paste0('Мин. Д.'), showlegend = FALSE, type="scatter", mode = 'lines', fill='tonexty', fillcolor='rgba(77,255,193,0.2)', line=list(color='transparent')) %>% 
      
      add_trace(data = sub_cons_predict(), x = ~Дата, y = ~Approximation, yaxis = 'y2',
                name = paste0('С/н'), type="scatter", mode = 'lines', line = list(width = 2, color = 'rgb(219, 94, 26)')) %>%      
      add_trace(data = sub_prod_predict(), x = ~Дата, y = ~Approximation,
                name = paste0('Добыча'), type="scatter", mode = 'lines', line = list(width = 2, color = 'rgb(29, 172, 214)')) %>% 
      
      #add_trace(data = sub_trend(), x = ~Дата, y = ~Добыча,
      #          name = 'ЦКР', type='scatter', mode='lines', line = list(color = '#926eae', width = 2, dash='dot')) %>%
      
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
                                     font=list(size=14, color='#1fb5b8')),
                          gridcolor = 'rgb(100,100,100)',
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          tickfont = list(color='#1fb5b8', size=10),
                          zeroline = FALSE,
                          side = 'right',
                          range = list(0, max(sub_prod()$Combine_prod_high, sub_prod_approx()$Combine_prod_high, sub_prod_predict()$Combine_prod_high) * 1.1),
                          dtick = max(sub_prod()$Combine_prod_high, sub_prod_approx()$Combine_prod_high, sub_prod_predict()$Combine_prod_high) * 1.1 / 4),
             yaxis2 = list(title=list(text="Собств. нужды, тыс. м3",
                                      font=list(size=14, color='#d35339')),
                           gridcolor = 'rgb(100,100,100)',
                           showgrid = TRUE,
                           showline = TRUE,
                           showticklabels = TRUE,
                           tickcolor = 'rgb(127,127,127)',
                           ticks = 'outside',
                           tickfont = list(color='#d35339', size=10),
                           overlaying = "y",
                           side = "left",
                           range = list(0, max(sub_cons()$Approximation, sub_cons_approx()$Approximation, sub_cons_predict()$Approximation) * 2),
                           dtick = max(sub_cons()$Approximation, sub_cons_approx()$Approximation, sub_cons_predict()$Approximation) * 2 / 4)) %>%
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
    subset(data[data$Месторождение %in% top_data[1:4,]$Месторождение,]) #, Дата >= input$dateslider[1] & Дата <= input$dateslider[2])
  })
  
  subdata_low_prod <- reactive({ 
    subset(data[data$Месторождение %in% top_data[5:nrow(top_data),]$Месторождение,]) %>% #, Дата >= input$dateslider[1] & Дата <= input$dateslider[2]) %>% 
      group_by(Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation), Combine_prod = sum(Combine_prod)) %>% mutate(Дата = as.Date(Дата))
  })
  
  subdata_dependent <- reactive({
    subset(data[data$Месторождение %in% list('АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)', 'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)', 'ОАО Томскнефть (ГП 50%)'),], Дата >= input$dateslider[1] & Дата <= input$dateslider[2]) %>% 
      group_by(Дата) %>% summarise(Добыча = sum(Добыча), Approximation = sum(Approximation), Combine_prod = sum(Combine_prod)) %>% mutate(Дата = as.Date(Дата))
  })
  
  subdata_independent <- reactive({
    subset(data[data$Месторождение %in% list('Независимые произв.', 'Нефтяные компании'),]) #, Дата >= input$dateslider[1] & Дата <= input$dateslider[2]) #%>% 
  })
  
  output$plot_total_graph <- renderPlotly({
    plot_ly() %>% 
      add_trace(stackgroup = 'one', data = subdata_independent(), x = ~Дата, y = ~Combine_prod, 
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
  chosen_field <- reactive({
    if (all(is.na(subset(polygon, Месторождение == input$field)$geometry))) {
      data.frame(Месторождение=input$field, Longitude=c(0, 0, 0, 0), Latitude=c(0, 0, 0, 0)) %>%
        group_by(Месторождение) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
    } else {
      subset(polygon, Месторождение == input$field)
    }
  })
  
  chosen_pipes_to_field <- reactive({
    if (all(is.na(subset(pipes_to_field, Месторождение == input$field)$geometry))) {
      data.frame(Месторождение=input$field, Longitude=0, Latitude=0) %>%
        group_by(Месторождение) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("LINESTRING")
    } else {
      subset(pipes_to_field, Месторождение == input$field)
    }
  })

  # current_zoom <- eventReactive({input$map_view_change}, {input$map_view_change$zoom})
  # observeEvent({input$map_view_change}, {message(current_zoom())})
  
  set_token('pk.eyJ1IjoibmlreXBhcmZlbm92IiwiYSI6ImNrdGw1d3RtczA1NDYyd3A2dTd3M3M4ajYifQ.YhQLodjnRi2A2Kgch8KKgA')
  output$map <- renderMapdeck({
    mapdeck(style = mapdeck_style('dark'),
            pitch = 45,
            location = c(50, 55),
            zoom = 3.5,
    ) %>%
      add_polygon(
        data = polygon,
        layer_id = "polygon_layer",
        fill_colour = '#30d5c8',
        auto_highlight = TRUE,
        highlight_colour = '#ff5f49cc',
        update_view = F,
        tooltip = 'Месторождение',
      ) %>%
      add_scatterplot(
        data = coord_ugs,
        lat = "Latitude",
        lon = "Longitude",
        radius = 15000 ,
        fill_colour = '#e0b3ff',# "#c9a0dc", # "#ff8c00",
        auto_highlight = TRUE,
        layer_id = "scatter_layer",
        update_view = F,
        tooltip = 'Name',
      )  %>%
      add_path(
        data = traces,
        stroke_colour = '#57b9ff', 
        stroke_width = 1,
        layer_id = "pipe_layer",
        update_view = F,
      ) 
  })

  # Event connected with chosen UGSS display option and chosen field (transportation path color)
  observeEvent({
    input$ugss_display
    input$field}, {
      
    # Condition for displaying of different options
    if ('pipe' %in% input$ugss_display) {
      pipe_width = 1
      mapdeck_update(map_id = 'map') %>%
        add_path(
          data = traces,
          stroke_colour = '#57b9ff', 
          stroke_width = 1,
          layer_id = "pipe_layer",
          update_view = F,
        ) %>%
        add_path(
          data = chosen_pipes_to_field(),
          stroke_colour = '#ffa947', #'#ff5f49', 
          width_scale = 2,
          layer_id = "chosen_pipeline",
          update_view = F,
        )
    } else {  
      pipe_width = 0
      mapdeck_update(map_id = 'map') %>%
        clear_path("pipe_layer") %>%    
        clear_path("chosen_pipeline")
    } 
    
    if ('graph' %in% input$ugss_display) {
      graph_width = 1
      mapdeck_update(map_id = 'map') %>%
        add_scatterplot(
          data = compressors,
          lat = "Latitude",
          lon = "Longitude",
          radius = 10000,
          stroke_opacity = 0,
          fill_opacity = 0,
          fill_colour = "#ffff00",
          auto_highlight = TRUE,
          layer_id = 'graph_layer',
          update_view = F,
          tooltip = 'Compressor',
          )
    } else {
      graph_width = 0
      mapdeck_update(map_id = 'map') %>%
        clear_scatterplot('graph_layer') 
    }
      
    if ('regions' %in% input$ugss_display) {
      mapdeck_update(map_id = 'map') %>%
        add_polygon(
          data = regions,
          layer_id = "regions",
          stroke_width = 1000,
          stroke_colour = "#ff77ff",
          fill_opacity = 0,
          auto_highlight = F,
          update_view = F,
        ) %>%
        add_scatterplot(
          data = region_centroids,
          lat = "Latitude",
          lon = "Longitude",
          radius = 20000,
          stroke_width = 7000,
          stroke_colour = "#ff77ff",
          fill_opacity = 0,
          auto_highlight = F,
          layer_id = "regions_centroids",
          update_view = F,
          tooltip = 'Region',
        ) 
    } else {
      mapdeck_update(map_id = 'map') %>%
        clear_polygon('regions') %>%
        clear_scatterplot('regions_centroids') %>%
        clear_line('graph_regions')
    }
      
    graph_compressors$stroke <- graph_compressors$stroke * 2 * graph_width
    
    # Due-to mapdeck can't clear animated layers (assume bug), animations are hiding by multiplication width and opacity to zero values
    mapdeck_update(map_id = 'map') %>%
      add_trips(
        data = traces,
        stroke_colour = '#00bfff',
        stroke_width = 5 * pipe_width,
        layer_id = "pipe_layer_animation",
        opacity = 0.5 * pipe_width,
        trail_length = 60,
        start_time = 0,
        end_time = 90,
        animation_speed = 30
      ) %>%
      add_animated_line(
        data = graph_compressors, 
        layer_id = "graph_layer_animation", 
        origin = c('lon_begin', 'lat_begin'), 
        destination = c('lon_end', 'lat_end'), 
        stroke_colour = "#ff7a5c", 
        stroke_width = 'stroke', 
        stroke_opacity = 1 * graph_width,
        trail_length = 0.7,
        animation_speed = 0.1, 
        update_view = F,
      ) %>%
      add_polygon(
        data = chosen_field(),
        layer_id = "chosen_polygon",
        fill_colour = '#ff5f49',
        elevation = 1,
        update_view = F,
      ) 
  }, ignoreNULL=FALSE)
  
  # MACRO GRAPH (VisNetwork)
  output$macro_graph_visnetwork <- renderVisNetwork({
    nodes <- rbind(data.frame(id=region_centroids$Region,
                              label=region_centroids$Region, 
                              group=c('Region'),
                              x = region_centroids$x / 10000, 
                              y = -region_centroids$y / 10000),
                   data.frame(id=coord_compressors$Compressor,
                              label=coord_compressors$Compressor,
                              group=c('Compressor'),
                              x = LatLongToMerc(coord_compressors$Longitude, coord_compressors$Latitude)$x / 10000,
                              y = - LatLongToMerc(coord_compressors$Longitude, coord_compressors$Latitude)$y / 10000)
                              )
                   
    edges <- data.frame(from=graph_regions$Start,
                        to=graph_regions$End)
    visNetwork(nodes, edges, width = "100%", height = "100%") %>%
      visIgraphLayout() %>%
      visNodes(x = 'x',
               y = 'y') %>%
      visEdges(color=list(hover='orange', 
                          highlight='orange',
                          color='#57b9ff'),
               arrows='middle') %>%
      visGroups(groupname = "Region", 
                size = 20,
                font = list(color = "#30d5c8"),
                color = list(background="#D2E5FF",
                             border='#2B7CE9',
                             hover=list(background='orange',
                                        border='red'),
                             highlight=list(background='orange',
                                            border='red')
                             )
                ) %>% 
      visGroups(groupname = "Compressor", 
                size = 10,
                font = list(color = "white"),
                color = list(background="yellow",
                             border='orange',
                             hover=list(background='orange',
                                        border='red'),
                             highlight=list(background='orange',
                                            border='red')
                            )
                ) %>% 
      visInteraction(hover = TRUE) %>%
      visEvents(hoverNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}")
    })
}

shinyApp(ui, server)
