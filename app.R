# Diseño de dashboard en contratacion publica durante pandemia
# covid 19 en Colombia - DashCCo
# Fecha elaboracion: 15/07/2020

# 1. Librerias ----
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(leaflet)
library(DT)
library(colorspace)
library(sp)
library(sf)
library(ggiraph)
library(packcircles)
library(viridisLite)
library(viridis)
library(formattable)

# 2. Carga de conjunto de datos ----
# Conjunto de datos completo 
contratos <- read_csv(file = "Datasets/contratos_covid19_LV1.csv", 
                      locale = locale(encoding = "UTF-8"))

# Conjunto de datos indicadores por municipio
indicadoresM <- read_csv(file = "Datasets/IRC_indicadoresM.csv", 
                         locale = locale(encoding = "UTF-8"))

# Conjunto de datos indicadores por departamento
indicadoresD <- read_csv(file = "Datasets/IRC_indicadoresD.csv", 
                         locale = locale(encoding = "UTF-8"))

# Conjunto de datos indicadores por Familia
indicadoresF <- read_csv(file = "Datasets/IRC_indicadoresF.csv", 
                         locale = locale(encoding = "UTF-8"))

# Conjunto de datos indicadores por Entidad
indicadoresE <- read_csv(file = "Datasets/IRC_indicadoresE.csv", 
                         locale = locale(encoding = "UTF-8"))

# Conjunto de datos poligonos para municipios
load(file = "Datasets/municipios.rda")


load(file = "Datasets/departamentos.rda")

# 3. Funciones ----
# 3.1. Descriptivas ----  
## * Grafica resumen
grafica_serie_tiempo <- function(data, cantidad = T, acumulado = F,
                                 dpto = 'Nacional', intervalo_fecha,
                                 escala = 'semana'){
  
  # Consulta
  stat_contr <- data %>%
    filter(fecha_firma >= ymd(intervalo_fecha[1]), 
           fecha_firma <= ymd(intervalo_fecha[2]))
  
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional') {
    stat_contr <- stat_contr %>%
      filter(departamento_entidad == dpto)
  }
  
  # Definicion de la escala
  
  if (escala == 'semana') {
    stat_contr <- stat_contr %>% 
      mutate(semana = paste0(year(fecha_firma), week(fecha_firma))) %>% 
      group_by(semana) %>% 
      mutate(fecha_sem_max = max(fecha_firma),
             fecha_firma = min(fecha_firma)) %>% 
      ungroup(semana) %>% select(-semana)
    
  } else if(escala == 'mes'){
    stat_contr <- stat_contr %>% 
      mutate(fecha_firma = make_date(year = year(fecha_firma), 
                                     month = month(fecha_firma), 
                                     day = '01'))
  }
  
  label_mes <- function(mes){
    label <- case_when(mes == 1 ~ 'Ene',
                       mes == 2 ~ 'Feb',
                       mes == 3 ~ 'Mar',
                       mes == 4 ~ 'Abr',
                       mes == 5 ~ 'May',
                       mes == 6 ~ 'Jun',
                       mes == 7 ~ 'Jul',
                       mes == 8 ~ 'Ago',
                       mes == 9 ~ 'Sep',
                       mes == 10 ~ 'Oct',
                       mes == 11 ~ 'Nov',
                       mes == 12 ~ 'Dic')
    return(label)
  }
  
  # Resumen por la escala establecida
  if (escala == 'semana'){
    stat_contr <- stat_contr %>%
      group_by(fecha_firma) %>% 
      summarise(n_contratos = n(), 
                cuantia_dia = sum(valor_total, na.rm = TRUE) / 1e9,
                fecha_sem_max = min(fecha_sem_max)) %>% 
      mutate(n_contratos_acum = cumsum(n_contratos), 
             cuantia_acumulada = cumsum(cuantia_dia),
             dia_inicio = paste0(label_mes(month(fecha_firma)), " ", 
                                 day(fecha_firma)),
             dia_final = paste0(label_mes(month(fecha_sem_max)), " ", 
                                day(fecha_sem_max)))
  } else {
    stat_contr <- stat_contr %>%
      group_by(fecha_firma) %>% 
      summarise(n_contratos = n(), 
                cuantia_dia = sum(valor_total, na.rm = TRUE) / 1e9) %>% 
      mutate(n_contratos_acum = cumsum(n_contratos), 
             cuantia_acumulada = cumsum(cuantia_dia))
  }
  
  if (escala == 'dia') {
    stat_contr <- stat_contr %>%
      mutate(mes_lb = label_mes(month(fecha_firma)),
             fecha_lbl = paste0("Día: ", mes_lb, " ", day(fecha_firma),
                                ", ", year(fecha_firma), "<br>")) %>% 
      select(-mes_lb)
    
  } else if (escala == 'semana') {
    stat_contr <- stat_contr %>%
      mutate(fecha_lbl = paste0("Semana: ", dia_inicio, " - ",
                                dia_final, ", ", year(fecha_firma), 
                                "<br>")) %>% 
      select(-c(dia_inicio, dia_final))
  } else {
    stat_contr <- stat_contr %>%
      mutate(mes_lb = label_mes(month(fecha_firma)),
             fecha_lbl = paste0("Mes: ", mes_lb, ", ", 
                                year(fecha_firma), "<br>")) %>% 
      select(-mes_lb)
  }
  
  # ajuste de contratos
  if (cantidad & acumulado) {
    # Miscelaneos
    stat_contr <- stat_contr %>%
      mutate(fecha_lbl = paste0(fecha_lbl, "Cantidad: ",
                                formatC(n_contratos_acum,
                                        format = "fg", 
                                        big.mark = ".", 
                                        decimal.mark = ",", 
                                        digits = 3), 
                                "<extra></extra>"))
    
    titulo <- 'Cantidad de contratos'
    y_titulo <- "Cantidad"
    suffix <- ""
    
    # objeto de plotly
    fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                   y = ~n_contratos_acum, type = 'scatter', 
                   mode = 'lines', fill = 'tozeroy',
                   hovertemplate = ~fecha_lbl,
                   fillcolor = 'rgba(0,144,181,0.5)',
                   line = list(width = 0.5,
                               color = 'rgba(0,144,181,1)'))
    
  } else if(!cantidad & acumulado){
    # Miscelaneos
    stat_contr <- stat_contr %>%
      mutate(fecha_lbl = paste0(fecha_lbl, "Valor: ",
                                formatC(cuantia_acumulada,
                                        format = "fg", 
                                        big.mark = ".", 
                                        decimal.mark = ",", 
                                        digits = 5), 
                                " MM<extra></extra>"))
    titulo <- 'Valor de Contratos'
    y_titulo <- "Valor"
    suffix <- " MM"
    
    # objeto de plotly
    fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                   y = ~cuantia_acumulada, type = 'scatter', 
                   mode = 'lines', fill = 'tozeroy',
                   hovertemplate = ~fecha_lbl,
                   fillcolor = 'rgba(122,229,130,0.5)',
                   line = list(width = 0.5,
                               color = 'rgba(122,229,130,1)'))
    
  } else if(cantidad & !acumulado){
    # Miscelaneos
    stat_contr <- stat_contr %>%
      mutate(fecha_lbl = paste0(fecha_lbl, "Cantidad: ",
                                formatC(n_contratos,
                                        format = "fg", 
                                        big.mark = ".", 
                                        decimal.mark = ",", 
                                        digits = 3), 
                                "<extra></extra>"))
    titulo <- 'Cantidad de Contratos'
    y_titulo <- "Cantidad"
    suffix <- ""
    
    # objeto de plotly
    fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                   y = ~n_contratos, type = 'scatter', 
                   mode = 'lines',
                   hovertemplate = ~fecha_lbl,
                   line = list(color = 'rgba(0,144,181,1)'))
    
  } else{
    # Miscelaneos
    stat_contr <- stat_contr %>%
      mutate(fecha_lbl = paste0(fecha_lbl, "Valor: ",
                                formatC(cuantia_dia,
                                        format = "fg", 
                                        big.mark = ".", 
                                        decimal.mark = ",", 
                                        digits = 5), 
                                " MM<extra></extra>"))
    titulo <- 'Valor de Contratos'
    y_titulo <- "Valor"
    suffix <- " MM"
    
    # objeto de plotly
    fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                   y = ~cuantia_dia, type = 'scatter', 
                   mode = 'lines',
                   hovertemplate = ~fecha_lbl,
                   line = list(color = 'rgba(122,229,130,1)'))
  }
  
  # Parametros generales grafica
  f_titles <- list(
    family = "Arial, sans-serif",
    size = 13,
    color = "darkgrey")
  
  f_legend <- list(
    family = "Arial, sans-serif",
    size = 11,
    color = "black")
  
  # Agregacion de detalles
  fig <- fig %>%
    layout(title = list(text = titulo),
           xaxis = list(
             title = list(text = "Fecha de firma",
                          font = f_titles),
             range = c(as.character(min(stat_contr$fecha_firma)), 
                       as.character(max(stat_contr$fecha_firma))),
             type = "date",
             tickfont = f_legend),
           
           yaxis = list(
             title = list(text = y_titulo,
                          font = f_titles),
             tickfont = f_legend,
             ticksuffix = suffix),
           margin = list(
             l = 40, r = 5, t = 30, b = 30),
           plot_bgcolor='rgb(255,255,255)',
           paper_bgcolor='rgb(255,255,255)') %>% 
    config(locale = "es")
  
  return(fig)
}

# funcion de mapa
grafica_mapa <- function(data, dpto = 'Nacional', 
                         operacion = "cuantia"){
  
  addLegendCustom <- function(map, colors, labels, sizes, 
                              opacity = 0.5, title, width = '50'){
    
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, 
                             "px; border-radius: 50%; margin-top: 3px; ",
                             "margin-bottom: 3px; float: left")
    labelAdditions <- paste0("<div style = 'display: ",
                             "inline-block; height:", sizes, "px ;",
                             "line-height:", sizes, "px; ",
                             "width: ", width, "px; ",
                             "margin-top: 3px; margin-bottom: 3px; ",
                             "float: right'>", labels, "</div>")
    
    return(addLegend(map, position = "bottomright", colors = colorAdditions, 
                     labels = labelAdditions, opacity = opacity, 
                     title = title))
  }
  
  # a. Ajuste por departamento
  if (dpto != "Nacional"){
    df_mapa <- data %>% 
      filter(departamento_entidad == dpto) %>%
      mutate(nombre_lugar = municipio_entidad) %>%
      group_by(nombre_lugar, codigo_municipio_ent) %>% 
      summarise(cuantia = n(),
                valor = sum(valor_total, na.rm = TRUE),
                lat = mean(latitud, na.rm = TRUE), 
                lng = mean(longitud, na.rm = TRUE)) 
    
    df_mapa <- df_mapa %>% 
      mutate(porcentaje_cuantia = (cuantia / sum(df_mapa$cuantia)) * 100, 
             porcentaje_valor = (valor / sum(df_mapa$valor)) * 100)  %>%
      arrange(codigo_municipio_ent)
    
  } else {
    df_mapa <- data %>%
      mutate(nombre_lugar = departamento_entidad) %>%
      group_by(nombre_lugar, codigo_departamento_ent) %>% 
      summarise(cuantia = n(),
                valor = sum(valor_total, na.rm = TRUE),
                lat = mean(latitud, na.rm = TRUE), 
                lng = mean(longitud, na.rm = TRUE)) 
    
    df_mapa <- df_mapa %>% 
      mutate(porcentaje_cuantia = (cuantia / sum(df_mapa$cuantia)) * 100, 
             porcentaje_valor = (valor / sum(df_mapa$valor)) * 100)  %>%
      arrange(codigo_departamento_ent)
  }
  
  # b. Cuantia o Valor
  
  if(operacion == "cuantia") {
    df_mapa <- df_mapa %>% 
      ungroup()%>% 
      select(nombre_lugar, cuantia, 
             porcentaje_cuantia, lat, lng) %>% 
      rename(x = cuantia,
             p = porcentaje_cuantia) %>%
      arrange(desc(x))
    
  } else {
    df_mapa <-  df_mapa %>% 
      ungroup()%>%
      select(nombre_lugar, valor, porcentaje_valor, lat, lng) %>% 
      rename(x = valor,
             p = porcentaje_valor) %>%
      arrange(desc(x))
  }
  
  color = "limegreen" # si es por valor
  
  if(operacion == "cuantia") {
    color = "darkturquoise"
  }
  
  # 2. Escala de los circulos en el mapa
  if (operacion == "cuantia") 
  { #Variables
    num_intervalos <- 5
    vmax <- quantile(df_mapa$x, probs = 0.9, names = FALSE)
    #Ecuación para hallar  base
    base = vmax ^ (1 / num_intervalos)
    df_mapa <-  df_mapa %>%
      mutate(tam_escalado = case_when(x >= base ^ 2 & x < base ^ 3 ~ 2,
                                      x >= base ^ 3 & x < base ^ 4 ~ 4,
                                      x >= base ^ 4 & x < base ^ 5 ~ 8,
                                      x >= base ^ 5 ~ 16))
  } else {
    
    num_intervalos <- 5
    vmax <- quantile(df_mapa$x, probs = 0.9, names = FALSE)
    #Ecuación para hallar  base
    base = vmax ^ (1 / num_intervalos)
    df_mapa <-  df_mapa %>%
      mutate(tam_escalado = case_when(x >= base ^ 2 & x < base ^ 3 ~ 2,
                                      x >= base ^ 3 & x < base ^ 4 ~ 4,
                                      x >= base ^ 4 & x < base ^ 5 ~ 8,
                                      x >= base ^ 5 ~ 16))
  }
  
  # 3. Etiqueta
  if(operacion == "cuantia") {
    etiqueta <- paste(
      sprintf("<dfn>Lugar: </dfn><strong>%s</strong><br/> 
                <dfn>Total contratos: </dfn> %g<br/>",
              df_mapa$nombre_lugar, df_mapa$x),
      sprintf("<dfn>Porcentaje: </dfn> %3.2f",df_mapa$p),
      "%") %>% 
      lapply(htmltools::HTML)
    
    etiqueta_legend <- c(paste('< ', formatC(round(base ^ 3, 0), 
                                             format = "fg", 
                                             big.mark = ".", 
                                             decimal.mark = ",")),
                         paste('< ', formatC(round(base ^ 4, 0), 
                                             format = "fg",
                                             big.mark = ".",
                                             decimal.mark = ",")), 
                         paste('< ', formatC(round(base ^ 5, 0) - 1, 
                                             format = "fg",
                                             big.mark = ".",
                                             decimal.mark = ",")),
                         paste('> ', formatC(round(base ^ 5, 0), 
                                             format = "fg", 
                                             big.mark = ".", 
                                             decimal.mark = ",")))
    
    title_legend <- "Referencia"
    width_legend <- "50"
    colors_leg <- rep(x = color, 4)
    size_leg <- c(4, 8, 16, 32)
    
  } else {
    etiqueta <- paste(
      sprintf("<dfn>Lugar: </dfn><strong>%s</strong><br/> 
                <dfn>Total valor contratos ($): </dfn> %6.2f MM<br/>",
              df_mapa$nombre_lugar, (df_mapa$x)/1e9),
      sprintf("<dfn>Porcentaje: </dfn> %3.2f",df_mapa$p),
      "%") %>% 
      lapply(htmltools::HTML)
    
    formatC(round((base ^ 4) / 1e6, 0), 
            format = "fg", big.mark = ".", decimal.mark = ",")
    
    etiqueta_legend <- c(paste('< ', formatC(round((base ^ 4) / 1e6, 0), 
                                             format = "fg", 
                                             big.mark = ".", 
                                             decimal.mark = ",")), 
                         paste('< ', formatC(round((base ^ 5) / 1e6 - 1, 0), 
                                             format = "fg", 
                                             big.mark = ".", 
                                             decimal.mark = ",")),
                         paste('> ', formatC(round((base ^ 5) / 1e6, 0), 
                                             format = "fg", 
                                             big.mark = ".", 
                                             decimal.mark = ",")))
    
    title_legend <- "Referencia Mill."
    width_legend <- "75"
    colors_leg <- rep(x = color, 3)
    size_leg <- c(8, 16, 32)
  }
  
  # 4. Construcción de la gráfica
  map <- leaflet(data = df_mapa) %>% 
    addTiles() %>%
    addProviderTiles("Wikimedia") %>%
    addCircleMarkers(radius = ~tam_escalado, #cuantia/valor
                     color = color,
                     stroke = FALSE, fillOpacity = 0.6,
                     label = ~etiqueta,
                     labelOptions = labelOptions(
                       style = list(
                         "color" = "black",
                         "font-family" = "Arial",  #Tipo fuente 
                         #"font-style" = "italic", #Estilo
                         "font-size" = "11px",
                         "border-color" = "rgba(0,88,59,0.5)")))  %>% 
    addLegendCustom(colors = colors_leg, 
                    labels = etiqueta_legend, 
                    sizes = size_leg, 
                    title = title_legend, width = width_legend)
  
  return(map)
}

# funcion top departamento
grafica_top_dto_mun <- function(data, dpto = 'Nacional', 
                                operacion = "cuantia"){
  # a. departamento
  if (dpto != "Nacional")
  {top_Dto_Mun <- data %>% 
    filter(departamento_entidad == dpto) %>%
    mutate(nombre_lugar = municipio_entidad) %>%
    group_by(nombre_lugar, codigo_municipio_ent) %>%
    summarise(cuantia = n(),
              valor = sum(valor_total,na.rm = TRUE)/1e9) 
  } else{
    top_Dto_Mun <- data %>%
      mutate(nombre_lugar = departamento_entidad) %>%
      group_by(nombre_lugar) %>%
      summarise(cuantia = n(),
                valor = sum(valor_total,na.rm = TRUE)/1e9)
  }
  
  # b. cuantia o valor
  if(operacion == "cuantia"){
    top_Dto_Mun <- top_Dto_Mun %>% 
      ungroup()%>% 
      select(nombre_lugar, cuantia) %>% 
      rename(x = cuantia) %>%
      arrange(desc(x))
    top_Dto_Mun <- head(top_Dto_Mun, 10) 
  }else{
    top_Dto_Mun <-top_Dto_Mun %>% 
      ungroup()%>%
      select(nombre_lugar, valor) %>% 
      rename(x = valor) %>%
      arrange(desc(x))
    top_Dto_Mun <- head(top_Dto_Mun, 10)
  }
  # Establecer eje
  if(operacion == "cuantia"){
    eje = "Cantidad de contratos"
    criterio <- "Cantidad: %{value}"
    suffix <- ""
  }else {
    eje <- "Valor monetario"
    criterio <- "Valor: %{value:.2f} MM"
    suffix <- " MM"
  }
  
  # Establecer color
  if(operacion == "cuantia"){
    color = "0, 144, 181"         #azul
  }else {color ="122, 229, 130"}  #verde
  
  # 2.1 Parametros generales grafica
  f1 <- list(
    family = "Arial, sans-serif",
    size = 13,
    color = "darkgrey")
  
  f2 <- list(
    family = "Arial, sans-serif",
    size = 11,
    color = "black")
  
  # 2.2. Construcción de la gráfica
  
  top_Dto_Mun <- top_Dto_Mun %>% 
    mutate(nombre_lugar = as.character(nombre_lugar)) %>%
    mutate(etiqueta = case_when(str_length(nombre_lugar) >= 10 ~ 
                                  paste0(str_sub(nombre_lugar, 1, 10), "..."),
                                TRUE ~ nombre_lugar))
  
  top_dto <- plot_ly(data = top_Dto_Mun,
                     type = "bar",
                     x = ~x,
                     y = reorder(top_Dto_Mun$etiqueta, top_Dto_Mun$x),
                     hovertext = reorder(top_Dto_Mun$nombre_lugar, 
                                         top_Dto_Mun$x),
                     hovertemplate = ifelse(nrow(top_Dto_Mun) == 1,
                                            paste("%{y}",
                                              "<extra>", criterio,"</extra>"),
                                            paste("%{hovertext}",
                                              "<extra>", criterio,"</extra>")),
                     orientation = 'h',
                     marker = list(color = paste("rgb",color),
                                line  = list(color = paste("rgb",color)))) %>% 
    layout(xaxis = list(title =  list(text = eje,
                                      font = f1), tickfont = f2,
                        ticksuffix = suffix),
           yaxis = list(title = list(text = "Lugar",
                                     font = f1), tickfont = f2)) %>% 
    config(locale = "es")
  
  return(top_dto)
}

# funcion top objeto
grafica_top_fam <- function(data, dpto = 'Nacional', 
                            operacion = "cuantia"){
  # 1. Recibir retorno de los botones
  
  # a. Botón 1: Nacional o departamentos
  if (dpto != "Nacional")
  {top_familia <- data %>% 
    filter(departamento_entidad == dpto) %>%
    #select(departamento_entidad,nombre_familia,valor_total) %>%
    group_by(nombre_familia) %>%
    summarise(cuantia = n(),
              valor = sum(valor_total,na.rm = TRUE)/1e9) 
  } else{
    top_familia <- data %>%
      group_by(nombre_familia) %>%
      summarise(cuantia = n(),
                valor = sum(valor_total,na.rm = TRUE)/1e9)
  }
  
  # b. Botón 2: cuantia o Valor
  if(operacion == "cuantia"){
    top_familia <- top_familia %>% 
      ungroup()%>% 
      select(nombre_familia, cuantia) %>% 
      rename(x = cuantia) %>%
      arrange(desc(x))
    top_familia <- head(top_familia, 10) 
    
  }else{
    top_familia <-  top_familia %>% 
      ungroup()%>%
      select(nombre_familia, valor) %>% 
      rename(x = valor) %>%
      arrange(desc(x))
    top_familia <- head(top_familia, 10)
  }
  
  # Establecer eje
  if(operacion == "cuantia"){
    eje = "Cantidad de contratos"
    criterio <- "Cantidad: %{value}"
    suffix <- ""
  }else {
    eje ="Valor monetario"
    criterio <- "Valor: %{value:.2f} MM"
    suffix <- " MM"
  }
  
  # Establecer color
  if(operacion == "cuantia"){
    color = "0, 144, 181"         #azul
  }else {color ="122, 229, 130"}  #verde
  
  # 2.1 Parametros generales grafica
  f1 <- list(
    family = "Arial, sans-serif",
    size = 13,
    color = "darkgrey")
  
  f2 <- list(
    family = "Arial, sans-serif",
    size = 11,
    color = "black")
  
  # 2.2 Construcción de la gráfica
  top_familia <- top_familia %>% 
    mutate(nombre_familia = as.character(nombre_familia)) %>%
    mutate(etiqueta = case_when(str_length(nombre_familia) >= 16 ~ 
                              paste0(str_sub(nombre_familia, 1, 16), "..."),
                              TRUE ~ nombre_familia))
  top_fam <- plot_ly(data = top_familia,
                     x = ~x,
                     y = reorder(top_familia$etiqueta, top_familia$x),
                     hovertext =  reorder(top_familia$nombre_familia, 
                                          top_familia$x),
                     hovertemplate = paste0("%{hovertext} <extra>",
                                            criterio, "</extra>"),
                     type = "bar",
                     orientation = 'h',
                     marker = list(color = paste("rgb",color),
                               line  = list(color = paste("rgb",color)))) %>% 
    layout(xaxis = list(title =  list(text = eje,
                                      font = f1), tickfont = f2,
                        ticksuffix = suffix),
           yaxis = list(title = list(text = "Familia de productos",
                                     font = f1), tickfont = f2)) %>% 
    config(locale = "es")
  
  return(top_fam)
}

# Sankey funcion
sankey_proceso <- function(data, tipo = "Cantidad", dpto = 'Nacional'){
  
  # Valores del parametro tipo
  # tipo = {Cantidad, Valor}
  
  #Ajuste colores nodos
  if (tipo == "Cantidad") {
    interval <- 0.25
    pal_l1 <- colorRampPalette(c("#76C3D8", "#76C3D8"))
    pal_l2 <- colorRampPalette(c("#006F8D", "#006F8D"))
    pal_l3 <- colorRampPalette(c("#004E64", "#004E64"))
  } else {
    interval <- 0.25
    pal_l1 <- colorRampPalette(c("#7AE582", "#7AE582"))
    pal_l2 <- colorRampPalette(c("#50C388", "#50C388"))
    pal_l3 <- colorRampPalette(c("#25A18E", "#25A18E"))
  }
  
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional') {
    data <- data %>%
      filter(departamento_entidad == dpto)
  }
  
  # Ajuste de nodos (node)
  nodes <- rbind(
    {x <- data %>%
      group_by(regimen_contratacion) %>% 
      summarise(n = n(),
                valor = sum(valor_total)/1e9) %>%
      select(regimen_contratacion, n , valor) %>% 
      rename(name = regimen_contratacion) %>% 
      mutate(nodo = "Régimen de contratación") %>% 
      arrange(desc(n));
    x$color <-  pal_l1(nrow(x)); x}, # regimen
    
    {x <- data %>% 
      group_by(tipo_proceso) %>% 
      summarise(n = n(),
                valor = sum(valor_total)/1e9) %>%
      select(tipo_proceso, n, valor) %>% 
      rename(name = tipo_proceso) %>% 
      filter(name != "Régimen Especial") %>% 
      mutate(nodo = "Tipo de proceso") %>% 
      arrange(desc(n));
    x$color = pal_l2(nrow(x)); x}, # tipo de proceso
    
    {x <- data %>%
      group_by(tipo_contrato) %>% 
      summarise(n = n(),
                valor = sum(valor_total)) %>% 
      select(tipo_contrato, n, valor) %>%
      rename(name = tipo_contrato) %>% 
      mutate(nodo = "Tipo de contrato")%>% 
      arrange(desc(n));
    x$color = pal_l3(nrow(x)); x}) %>% #tipo contrato
    unique()
  
  ## Agregar el orden a los nodos
  nodes$orden <- seq(from = 0, 
                     to = nrow(nodes) - 1, 
                     by = 1)
  
  ## Cambio del nombre a los nodos
  nodes <- nodes %>% ungroup() %>% 
    mutate(label = name)
  
  # Enlace de cada nodo (link)
  links <- rbind(data %>% 
                   group_by(regimen_contratacion, tipo_proceso) %>% 
                   summarise(n = n(),
                             valor = sum(valor_total)/1e9) %>% 
                   merge(x = ., y = nodes %>% select(-n, -valor), 
                         by.x = "regimen_contratacion",
                         by.y = "name", all.x = TRUE) %>% 
                   rename(source = orden,
                          name_source = label,
                          name_nodo_s = nodo) %>% 
                   merge(x = ., y = nodes%>% select(-n, -valor),
                         by.x = "tipo_proceso",
                         by.y = "name", all.x = TRUE) %>% 
                   rename(target = orden,
                          name_target = label,
                          name_nodo_t = nodo,
                          color = color.x) %>% 
                   select(source, target,
                          name_source, name_target,
                          name_nodo_s, name_nodo_t, 
                          color, n, valor), #Link de regimen a proceso 
                 
                 data %>% 
                   group_by(tipo_proceso, tipo_contrato) %>% 
                   summarise(n = n(),
                             valor = sum(valor_total)/1e9) %>% 
                   merge(x = ., y = nodes%>% select(-n, -valor),
                         by.x = "tipo_proceso",
                         by.y = "name", all.x = TRUE) %>% 
                   rename(source = orden,
                          name_source = label,
                          name_nodo_s = nodo) %>% 
                   merge(x = ., y = nodes%>% select(-n, -valor),
                         by.x = "tipo_contrato",
                         by.y = "name", all.x = TRUE) %>% 
                   rename(target = orden,
                          name_target = label,
                          name_nodo_t = nodo,
                          color = color.x) %>% 
                   select(source, target,
                          name_source, name_target,
                          name_nodo_s, name_nodo_t, 
                          color, n, valor)) %>% #Link de proceso a contrato
    filter(!(name_source == "Régimen Especial" & 
               name_target == "Régimen Especial")) %>% 
    arrange(desc(n)) %>% 
    mutate(color_2 = toRGB(color, alpha = 0.175),
           label = paste(
             "<i>",name_nodo_s,":</i> ", name_source,
             "<br><i>",name_nodo_t,":</i> ", name_target,
             sep = ""))
  
  ## Ajuste etiqueta de los enlaces
  if(tipo == "Cantidad"){
    links <- links %>% 
      rename(value = n)
    
    links <- links %>% 
      mutate(label = 
               paste(label, "<br><i>Contratos: </i>",
                     formatC(value, format = "fg", big.mark = ".", 
                             decimal.mark = ",", digits = 0),
                     "</br><extra></extra>", sep = ""))
  } else {
    links <- links %>% 
      rename(value = valor)
    
    links <- links %>% 
      mutate(label = 
               paste(label, "<br><i>Valor contratos: </i>",
                     formatC(value, format = "fg", big.mark = ".", 
                             decimal.mark = ",", digits = 5),
                     " MM.</br><extra></extra>", sep = ""))
  }
  # Construcción del gráfico
  fig <- plot_ly(
    type = "sankey",
    
    domain = list(
      x = c(0.025, 0.99),
      y = c(0.1, 1)),
    
    orientation = "h",
    
    node = list(
      pad = 10,
      thickness = 25,
      line = list(
        color = "black",
        width = 0.5),
      color = nodes$color,
      label = nodes$label,
      hovertemplate = ifelse(
        tipo == "Cantidad", 
        paste("<b>%{label}</b>",
              "<br><i>Contratos: </i>",
              "%{value:.0f}</br>",
              "<extra></extra>", 
              sep = ""), 
        paste("<b>%{label}</b>",
              "<br><i>Valor contratos: </i>",
              "%{value:$.2f} MM.</br>",
              "<extra></extra>", 
              sep = ""))),
    
    link = list(
      source = links$source,
      target = links$target,
      value =  links$value,
      color = links$color_2,
      label = links$label,
      line = list(
        color = links$color,
        width = 0.1),
      hovertemplate = "%{label}")) %>%
    
    layout(
      title = list(text = "Distribución por modalidad de contratación"),
      font = list(family = "Arial, sans-serif",
                  size = 11,
                  color = "black"),
      margin = list(
        l = 0,
        r = 0,
        t = 30,
        b = 0)) %>%
    
    add_annotations(
      x= c(0, 0.5075, 1),
      y= c(0, 0, 0),
      text = c("Régimen<br>contratación", 
               "Tipo de<br>proceso", 
               "Tipo de<br>contrato"),
      showarrow = F,
      font = list(size = 13)) %>% 
    config(locale = "es")
  
  # Salida del gráfico
  return(fig)
}

# Donut function 
donut_proceso <- function(data, input = "Cantidad", 
                          variable = "estado_proceso", dpto = 'Nacional'){
  
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional') {
    data <- data %>%
      filter(departamento_entidad == dpto)
  }
  
  # Consulta
  df <- data %>%
    rename(grupo = variable) %>% 
    group_by(grupo) %>% 
    summarise(n = n(),
              valor = sum(valor_total, na.rm = TRUE)/1e9)
  
  
  # Ajuste cuando es por valor
  if (input == "Valor"){
    df <- df %>% select(grupo, valor) %>% 
      rename(n = valor)
    pal <- colorRampPalette(c("#1C796A", "#25A18E", 
                              "#44B99E", "#62D0AD",
                              "#6EDB98", "#7AE582", 
                              "#bff2c1"), bias = 0.33)
    
  } else {
    pal <- colorRampPalette(c("#004E64", "#006F8D", 
                              "#0090B5", "#209EBF",
                              "#76C3D8"), bias = 0.33)
  }
  
  colores <- pal(nrow(df))
  
  valor_t <- sum(df$n)
  
  df <- df %>% 
    mutate(porcion = n/valor_t,
           grupo = case_when(porcion <= 0.03 ~ "Otros",
                             TRUE~ grupo)) %>% 
    arrange(porcion)
  
  # grafico
  fig <- plot_ly(data = df,
                 labels = ~grupo, 
                 
                 values = ~n,
                 
                 textinfo ='percent',
                 
                 textposition='outside',
                 
                 hovertemplate = ifelse(
                   input == "Cantidad",
                   paste("<b>%{label}</b>",
                         "<br><i>Cantidad contratos:</i>",
                         " %{value:.0f}</br>", 
                         "<i>Porcentaje: </i>",
                         "%{percent:.2%}</br>",
                         "<extra></extra>",
                         sep = ""),
                   paste("<b>%{label}</b>",
                         "<br><i>Valor contratos:</i>",
                         " %{value:.2f} MM.</br>", 
                         "<i>Porcentaje: </i>",
                         "%{percent:.2%}</br>",
                         "<extra></extra>",
                         sep = "")),
                 marker = list(
                   colors = colores),
                 
                 domain = list(
                   x = c(0,1),
                   y = c(0,1)
                 ),
                 showlegend = FALSE) %>%
    
    add_pie(hole = 0.6) %>%
    
    layout(title = '',
           legend = list(x = 0.3, y = -0.3),
           margin = list(
             l = 50, r = 50, t = 30, b = 20)) %>% 
    config(locale = "es")
  
  # salida del grafico
  return(fig)
}

# Función tree map familia
treemap_familia <- function(data, tipo = "Cantidad",
                            dpto = 'Nacional'){
  
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional') {
    data <- data %>%
      filter(departamento_entidad == dpto)
  }
  
  Familia <- data %>% 
    group_by(nombre_grupo, nombre_familia) %>% 
    summarise(cuantia = sum(valor_total, na.rm = TRUE)/1e9, 
              total = n()) %>% arrange(nombre_grupo)
  
  x <- Familia %>% select(nombre_grupo) %>% unique()
  
  len_x <- length(x[[1]])
  
  a <- data %>% 
    group_by(nombre_grupo)%>% 
    summarise(total = n())
  
  y <- data %>% 
    group_by(nombre_grupo) %>% 
    summarise(cuantia = sum(valor_total, na.rm = TRUE)/1e9)
  
  nwFamilia <- Familia %>% 
    mutate(ids = paste(nombre_grupo, nombre_familia, sep = " - "))
  
  #wis
  nwFamilia <- rbind(
    tibble(nombre_grupo = rep(x = "", len_x),
           nombre_familia = x[[1]],
           ids = nombre_familia,
           total = a[[2]],
           cuantia = y[[2]]), tibble(nwFamilia))
  
  nwFamilia <- nwFamilia %>% 
    rename(parents = nombre_grupo, info = nombre_familia) %>% 
    mutate(label = paste0(str_sub(info, 1, 100), "")) %>% 
    arrange(parents)
  
  #Treemap por cuantia de contratos
  if (tipo == "Valor"){
    
    #nwFamilia <- nwFamilia %>% arrange(desc(cuantia))
    
    pal_verde <- colorRampPalette(c("#1C796A", "#25A18E", 
                                    "#44B99E", "#62D0AD",
                                    "#6EDB98", "#7AE582", 
                                    "#bff2c1"), bias = 0.33)
    fig <- plot_ly(nwFamilia,
                   type = 'treemap',
                   ids = ~ids, labels = ~label, 
                   parents = ~parents,
                   hovertext = ~info,
                   values= ~ cuantia, 
                   branchvalues="total",
                   marker=list(colors = pal_verde(len_x)),
                   hovertemplate = paste(
                     "<b>%{label}</b>",
                     "<br><i>Valor de contratos: $</i>",
                     " %{value:.2f} MM </br>", 
                     "<i>Porcentaje: </i>",
                     "%{percentParent:.2%}</br>",
                     "<extra></extra>",
                     sep = ""),
                   domain = list(x = c(0,1),
                                 y = c(0,1))) %>% 
      layout(margin = list(l = 0, r = 0, t = 40, b = 0),
             title = "Distribución por familia de productos") %>% 
      config(locale = "es")
  } else {
    #Treemap por cantidad de contratos
    pal_azul <- colorRampPalette(c("#004E64", "#006F8D", 
                                   "#0090B5", "#209EBF",
                                   "#BFE3ED"), bias = 0.33)
    fig <- plot_ly(nwFamilia,
                   type = 'treemap',
                   ids = ~ids, labels = ~label, 
                   parents = ~parents,
                   hovertext = ~info,
                   values=~total,branchvalues="total",
                   marker=list(colors = pal_azul(len_x)),
                   hovertemplate = paste(
                     "<b>%{label}</b>",
                     "<br><i>Cantidad de contratos:</i>",
                     " %{value:.0f}</br>", 
                     "<i>Porcentaje: </i>",
                     "%{percentParent:.2%}</br>",
                     "<extra></extra>",
                     sep = ""),
                   domain = list(x = c(0,1),
                                 y = c(0,1))) %>% 
      layout(margin = list(l = 0, r = 0, t = 40, b = 0),
             title = "Distribución por familia de productos") %>% 
      config(locale = "es")
  }
  
  return(fig)
} 

# Función dona por grupo
donut_grupo <- function(data, tipo = "Cantidad",
                        dpto = 'Nacional'){
  
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional') {
    data <- data %>%
      filter(departamento_entidad == dpto)
  }
  
  Grupo <- data %>% 
    group_by(nombre_grupo) %>% 
    summarise(cuantia = sum(valor_total, na.rm = TRUE)/1e9, 
              total = n()) %>% arrange(nombre_grupo)
  
  
  #Por cantidad de contratos
  if(tipo == "Cantidad") { 
    
    Grupo <- Grupo %>% 
      mutate(porcion = total/sum(Grupo$total),
             nombre_grupo = case_when(porcion <= 0.03 ~ "Otros",
                                      TRUE~ nombre_grupo)) %>% 
      arrange(porcion)
    
    pal_azul <- colorRampPalette(c("#004E64", "#006F8D", 
                                   "#0090B5", "#209EBF",
                                   "#BFE3ED"), bias = 1.75)
    
    fig1 <- Grupo %>% plot_ly(labels = ~nombre_grupo, 
                              values = ~total,
                              textposition = 'outside') %>%
      
      add_pie(hole = 0.6, marker = list(colors=pal_azul(7)), 
              hovertemplate = paste("<b>%{label}</b>",
                                    "<br>Cantidad de contratos:",
                                    " %{value:.0f}</br>", 
                                    "Porcentaje: ",
                                    "%{percent:.2%}</br>",
                                    "<extra></extra>",
                                    sep = "")) %>% 
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE,
                          showticklabels = FALSE,
                          yaxis = list(showgrid = FALSE, 
                                       zeroline = FALSE, 
                                       showticklabels = FALSE)),
             margin = list(
               l = 50, r = 50, t = 30, b = 20)) %>% 
      config(locale = "es")
    
  }
  
  #Por cuantia de contratos
  if(tipo == "Valor") {
    
    Grupo <- Grupo %>% 
      mutate(porcion = cuantia/sum(Grupo$cuantia),
             nombre_grupo = case_when(porcion <= 0.03 ~ "Otros",
                                      TRUE~ nombre_grupo)) %>% 
      arrange(porcion)
    
    pal_verde <- colorRampPalette(c("#1C796A", "#25A18E", 
                                    "#44B99E", "#62D0AD",
                                    "#6EDB98", "#7AE582", 
                                    "#bff2c1"), bias = 1)
    fig1 <- Grupo %>% 
      plot_ly(labels = ~nombre_grupo, values = ~cuantia,
              textposition = 'outside') %>% 
      add_pie(hole = 0.6,marker = list(colors=pal_verde(7)),
              hovertemplate = paste("<b>%{label}</b>",
                                    "<br>Valor contratos:",
                                    " %{value:$.2f} MM</br>", 
                                    "Porcentaje: ",
                                    "%{percent:.2%}</br>",
                                    "<extra></extra>",
                                    sep = "")) %>% 
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                          showticklabels = FALSE),
             margin = list(
               l = 50, r = 50, t = 30, b = 20)) %>% 
      config(locale = "es")
  }
  
  #Inprimir grafico
  return(fig1)
} 

# Funcion de grafica entidad
entidades_ranking <- function(data, dpto = "Nacional", 
                              tipo = "Cantidad", top = TRUE){
  
  # Generacion de indicadores
  df <- data %>% 
    filter(valor_inicial > 0, !is.na(valor_inicial)) %>% 
    mutate(porcentaje_adicion = valor_adiciones/valor_inicial,
           es_directo = (tipo_proceso == "Contratación Directa"))
  
  if (dpto == "Nacional") {
    # Consulta
    df <- df %>%
      mutate(nombre_entidad = paste(nombre_entidad, 
                                    departamento_entidad, sep = " - ")) %>% 
      group_by(nombre_entidad) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
    
  } else {
    # Consulta
    df <- df %>% 
      filter(departamento_entidad == dpto) %>% 
      group_by(nombre_entidad) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
  }
  
  # Seleccion de criterio
  if (tipo == "Cantidad") {
    df <- df %>% select(nombre_entidad, n_contratos) %>% 
      rename(criterio = n_contratos)
  } else if (tipo == "Valor") {
    df <- df %>% select(nombre_entidad, valor_contratos)%>% 
      rename(criterio = valor_contratos)
  } else if (tipo == "ProporcionAdiciones") {
    df <- df %>% select(nombre_entidad, med_porcentaje_adc) %>% 
      rename(criterio = med_porcentaje_adc)
  } else {
    df <- df %>% select(nombre_entidad, contratos_directos,
                        poj_cont_directos) %>% 
      rename(criterio = contratos_directos)
  } 
  
  # Ordenar por el criterio
  df <- df %>% arrange(desc(criterio)) 
  
  if (top) {
    
    # Seleccionar el top 15
    df <- head(df, 15) %>% 
      mutate(etiqueta = case_when(str_length(nombre_entidad) >= 25~ 
                                paste0(str_sub(nombre_entidad, 1, 25), "..."),
                                TRUE ~ nombre_entidad))
    
    # Ajuste de parámetros de la grafica
    if (tipo == "Cantidad") {
      df <- df %>% 
        mutate(info = paste0("<b>", nombre_entidad, "</b><br>",
                             "Cantidad de contratos: ", criterio,
                             "<extra></extra>"))
      
      color <- "rgb(0, 163, 204)"
      suffix <- ""
      eje <- "Cantidad de contratos"
      
    } else if (tipo == "Valor") {
      df <- df %>% 
        mutate(info = paste0("<b>", nombre_entidad, "</b><br>",
                             "Valor de contratos: ", 
                             sprintf("$%.2f", criterio) ," MM",
                             "<extra></extra>"))
      
      color <- "rgb(153, 235, 158)"
      suffix <- " MM"
      eje <- "Valor de contratos"
      
    } else if (tipo == "ProporcionAdiciones") {
      df <- df %>% 
        mutate(info = paste0("<b>", nombre_entidad, "</b><br>",
                             "Porcentaje promedio de adición: ", 
                             sprintf("%.2f", criterio) ,"%",
                             "<extra></extra>"))
      
      color <- "rgb(0, 108, 136)"
      suffix <- " %"
      eje <- "Porcentaje promedio de adición"
    } else {
      df <- df %>% 
        mutate(info = paste0("<b>", nombre_entidad, "</b><br>",
                             "Contratos bajo contratación directa: ", 
                             sprintf("%.0f", criterio) ,"<br>",
                             "Porcentaje de contratos bajo C.D.: ", 
                             sprintf("%.2f", poj_cont_directos) ,"%<br>",
                             "<extra></extra>"))
      
      color <- "rgb(42, 183, 162)"
      suffix <- ""
      eje <- "Cantidad de contratos bajo contratación directa"
    }
    
    # Parametros generales grafica
    f1 <- list(
      family = "Arial, sans-serif",
      size = 13,
      color = "darkgrey")
    
    f2 <- list(
      family = "Arial, sans-serif",
      size = 11,
      color = "black")
    
    # Grafica
    plot_top <- plot_ly(
      type = "bar",
      x = df$criterio,
      y = reorder(df$etiqueta, df$criterio),
      hovertext = df$info,
      hovertemplate = "%{hovertext}",
      orientation = 'h',
      marker = list(color = color,
                    line  = list(color = color))) %>% 
      layout(xaxis = list(title =  list(text = eje,
                                        font = f1), 
                          tickfont = f2,
                          ticksuffix = suffix),
             yaxis = list(title = list(text = "Entidad",
                                       font = f1), 
                          tickfont = f2)) %>% 
      config(locale = "es")
    
    return(plot_top)
    
  } else {
    
    # ajuste de detalles
    if (tipo == "Cantidad") {
      df <- df %>% rename(`Nombre entidad`  = nombre_entidad,
                          `Cantidad Contratos` = criterio)
    } else if (tipo == "Valor") {
      df <- df %>% 
        mutate(criterio = round(criterio, 4)*1000) %>% 
        rename(`Nombre entidad`  = nombre_entidad,
               `Valor de Contratos Mill` = criterio)
    } else if (tipo == "ProporcionAdiciones") {
      df <- df %>% 
        mutate(criterio = round(criterio, 2)) %>%
        rename(`Nombre entidad`  = nombre_entidad,
               `% Promedio Adiciones` = criterio)
    } else {
      df <- df %>% 
        mutate(poj_cont_directos = round(poj_cont_directos, 2)) %>%
        rename(`Nombre entidad`  = nombre_entidad,
               `Contratos C. Directa` = criterio,
               `Porcentaje C. Directa` = poj_cont_directos)
    } 
    
    return(df)
  }
}

# Funcion de grafico contratistas
contratistas_ranking <- function(data, dpto = "Nacional", 
                                 tipo = "Cantidad", top = TRUE){
  
  # Generacion de indicadores
  df <- data %>% 
    filter(valor_inicial > 0, !is.na(valor_inicial)) %>% 
    mutate(porcentaje_adicion = valor_adiciones/valor_inicial,
           es_directo = (tipo_proceso == "Contratación Directa"))
  
  if (dpto == "Nacional") {
    # Consulta
    df <- df %>%
      mutate(nom_contratista = paste(nom_contratista, 
                                    departamento_contratista, sep = " - ")) %>% 
      group_by(nom_contratista) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
    
  } else {
    # Consulta
    df <- df %>% 
      filter(departamento_contratista == dpto) %>% 
      group_by(nom_contratista) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
  }
  
  # Seleccion de criterio
  if (tipo == "Cantidad") {
    df <- df %>% select(nom_contratista, n_contratos) %>% 
      rename(criterio = n_contratos)
  } else if (tipo == "Valor") {
    df <- df %>% select(nom_contratista, valor_contratos)%>% 
      rename(criterio = valor_contratos)
  } else if (tipo == "ProporcionAdiciones") {
    df <- df %>% select(nom_contratista, med_porcentaje_adc) %>% 
      rename(criterio = med_porcentaje_adc)
  } else {
    df <- df %>% select(nom_contratista, contratos_directos,
                        poj_cont_directos) %>% 
      rename(criterio = contratos_directos)
  } 
  
  # Ordenar por el criterio
  df <- df %>% arrange(desc(criterio)) 
  
  if (top) {
    
    # Seleccionar el top 15
    df <- head(df, 15) %>% 
      mutate(etiqueta = case_when(str_length(nom_contratista) >= 25~ 
                                paste0(str_sub(nom_contratista, 1, 25), "..."),
                                  TRUE ~ nom_contratista))
    
    # Ajuste de parámetros de la grafica
    if (tipo == "Cantidad") {
      df <- df %>% 
        mutate(info = paste0("<b>", nom_contratista, "</b><br>",
                             "Cantidad de contratos: ", criterio,
                             "<extra></extra>"))
      
      color <- "rgb(0, 144, 181)"
      suffix <- ""
      eje <- "Cantidad de contratos"
      
    } else if (tipo == "Valor") {
      df <- df %>% 
        mutate(info = paste0("<b>", nom_contratista, "</b><br>",
                             "Valor de contratos: ", 
                             sprintf("$%.2f", criterio) ," MM",
                             "<extra></extra>"))
      
      color <- "rgb(122, 229, 130)"
      suffix <- " MM"
      eje <- "Valor de contratos"
      
    } else if (tipo == "ProporcionAdiciones") {
      df <- df %>% 
        mutate(info = paste0("<b>", nom_contratista, "</b><br>",
                             "Porcentaje promedio de adición: ", 
                             sprintf("%.2f", criterio) ,"%",
                             "<extra></extra>"))
      
      color <- "rgb(0, 98, 122)"
      suffix <- " %"
      eje <- "Porcentaje promedio de adición"
    } else {
      df <- df %>% 
        mutate(info = paste0("<b>", nom_contratista, "</b><br>",
                             "Contratos bajo contratación directa: ", 
                             sprintf("%.0f", criterio) ,"<br>",
                             "Porcentaje de contratos bajo C.D.: ", 
                             sprintf("%.2f", poj_cont_directos) ,"%<br>",
                             "<extra></extra>"))
      
      color <- "rgb(37, 161, 142)"
      suffix <- ""
      eje <- "Cantidad de contratos bajo contratación directa"
    }
    
    # Parametros generales grafica
    f1 <- list(
      family = "Arial, sans-serif",
      size = 13,
      color = "darkgrey")
    
    f2 <- list(
      family = "Arial, sans-serif",
      size = 11,
      color = "black")
    
    # Grafica
    plot_top <- plot_ly(
      type = "bar",
      x = df$criterio,
      y = reorder(df$etiqueta, df$criterio),
      hovertext = df$info,
      hovertemplate = "%{hovertext}",
      orientation = 'h',
      marker = list(color = color,
                    line  = list(color = color))) %>% 
      layout(xaxis = list(title =  list(text = eje,
                                        font = f1), 
                          tickfont = f2,
                          ticksuffix = suffix),
             yaxis = list(title = list(text = "Contratista",
                                       font = f1), 
                          tickfont = f2)) %>% 
      config(locale = "es")
    
    return(plot_top)
    
  } else{
    
    # ajuste de detalles
    if (tipo == "Cantidad") {
      df <- df %>% rename(`Nombre Contratista` = nom_contratista,
                          `Cantidad Contratos` = criterio)
    } else if (tipo == "Valor") {
      df <- df %>% 
        mutate(criterio = round(criterio, 4)* 100) %>% 
        rename(`Nombre Contratista` = nom_contratista,
               `Valor de Contratos Mill` = criterio)
    } else if (tipo == "ProporcionAdiciones") {
      df <- df %>% 
        mutate(criterio = round(criterio, 2)) %>%
        rename(`Nombre Contratista` = nom_contratista,
               `Porcentaje Promedio Adiciones` = criterio)
    } else {
      df <- df %>% 
        mutate(poj_cont_directos = round(poj_cont_directos, 2)) %>%
        rename(`Nombre Contratista` = nom_contratista,
               `Contratos C. Directa` = criterio,
               `Porcentaje C. Directa` = poj_cont_directos)
    } 
    
    return(df)
  }
}

# Funcion indicadores entidades
valor_ind_ent <- function(selec, radio){
  
  df <- contratos %>% 
    filter(valor_inicial > 0, !is.na(valor_inicial)) %>% 
    mutate(porcentaje_adicion = valor_adiciones/valor_inicial,
           es_directo = (tipo_proceso == "Contratación Directa"))
  
  if (selec == "Nacional") {
    # Consulta
    df <- df %>%
      group_by(nombre_entidad) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
    
  } else {
    # Consulta
    df <- df %>% 
      filter(departamento_entidad == selec) %>% 
      group_by(nombre_entidad) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
  }
  
  # Seleccion de criterio
  if (radio == "Cantidad") {
    df <- nrow(df %>% select(nombre_entidad) %>% 
                 unique())
    
    df <- formatC(df, format = "fg", big.mark = ".", 
                  decimal.mark = ",")
    
  } else if (radio == "Valor") {
    df <- round(mean(df$valor_contratos, na.rm = TRUE), 5)*1e3
    
    df <- paste0("$ ", formatC(df, format = "fg", big.mark = ".", 
                               decimal.mark = ","), " Mill")
  } else if (radio == "ProporcionAdiciones") {
    df <- round(mean(df$med_porcentaje_adc, na.rm = TRUE), 2)
    
    df <- paste0(formatC(df, format = "fg", big.mark = ".", 
                         decimal.mark = ","), " %")
  } else {
    df <- round(mean(df$poj_cont_directos, na.rm = TRUE), 2)
    
    df <- paste0(formatC(df, format = "fg", big.mark = ".", 
                         decimal.mark = ","), " %")
  }
  
  return(df)
  
}

# Funcion indicadores contratista
valor_ind_cont <- function(selec, radio){
  
  df <- contratos %>% 
    filter(valor_inicial > 0, !is.na(valor_inicial)) %>% 
    mutate(porcentaje_adicion = valor_adiciones/valor_inicial,
           es_directo = (tipo_proceso == "Contratación Directa"))
  
  if (selec == "Nacional") {
    # Consulta
    df <- df %>%
      group_by(nom_contratista) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
    
  } else {
    # Consulta
    df <- df %>% 
      filter(departamento_contratista == selec) %>% 
      group_by(nom_contratista) %>% 
      summarise(n_contratos = n(),
                valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
                med_porcentaje_adc = mean(porcentaje_adicion, 
                                          na.rm = TRUE)*100,
                contratos_directos = sum(es_directo, na.rm = TRUE),
                poj_cont_directos = (contratos_directos/n_contratos)*100)
  }
  
  # Seleccion de criterio
  if (radio == "Cantidad") {
    df <- nrow(df %>% select(nom_contratista) %>% 
                 unique())
    
    df <- formatC(df, format = "fg", big.mark = ".", 
                  decimal.mark = ",")
  } else if (radio == "Valor") {
    df <- round(mean(df$valor_contratos, na.rm = TRUE), 5)*1e3
    
    df <- paste0("$ ", formatC(df, format = "fg", big.mark = ".", 
                               decimal.mark = ","), " Mill")
  } else if (radio == "ProporcionAdiciones") {
    df <- round(mean(df$med_porcentaje_adc, na.rm = TRUE), 2)
    
    df <- paste0(formatC(df, format = "fg", big.mark = ".", 
                         decimal.mark = ","), " %")
  } else {
    df <- round(mean(df$poj_cont_directos, na.rm = TRUE), 2)
    
    df <- paste0(formatC(df, format = "fg", big.mark = ".", 
                         decimal.mark = ","), " %")
  }
  
  return(df)
  
}

# 3.2. Indicadores ----

# Funcion del mapa de riesgo de corrupción por región
grafica_mapa_ind <- function(data, lugar, indicador) {
  
  addLegendCustom <- function(map, colors, labels, 
                              opacity = 0.5, title){
    
    colorAdditions <- paste0(colors, "; width: 20px; height: 20px;", 
                             "margin-top: 0px; margin-bottom: 0px")
    labelAdditions <- paste0("<div style = 'display: ",
                             "inline-block; height: 20px ;",
                             "line-height: 20px; ",
                             "margin-top: 0px; margin-bottom: 0px; ",
                             "float: center'>", labels, "</div>")
    
    return(addLegend(map, position = "bottomright", colors = colorAdditions, 
                     labels = labelAdditions, opacity = opacity, 
                     title = title))
  }
  
  #1. Recibir retorno de los 2 botones
  
  # a. Botón 1: Nacional o departamentos
  if (lugar != 'Nacional'){
    df_mapa <- data %>%
      mutate(codigo_municipio_ent = as.character(codigo_municipio_ent),
          codigo_municipio_ent = ifelse(str_length(codigo_municipio_ent) == 5,
                codigo_municipio_ent, paste0("0", codigo_municipio_ent))) %>% 
      filter(departamento_entidad == lugar, !is.na(codigo_municipio_ent)) %>%
      mutate(nombre_lugar = paste(municipio_entidad, 
                                  departamento_entidad,
                                  sep = "-")) %>%
      group_by(nombre_lugar, codigo_municipio_ent) %>% 
      summarise(oferentes_prom = oferentes_prom,
                ofertas_prom = ofertas_prom, 
                perc_contr_cerrada_num = perc_contr_cerrada_num, 
                perc_contr_cerrada_val = perc_contr_cerrada_val, 
                HHI_cant = HHI_cant, 
                HHI_val = HHI_val, 
                ID_cant = ID_cant, 
                ID_val = ID_val, 
                ganadoras = ganadoras, 
                IC4K_cant = IC4K_cant, 
                IC4K_val = IC4K_val, 
                perc_tiempo_adiciones = perc_tiempo_adiciones, 
                perc_valor_adiciones = perc_valor_adiciones,
                ind_riesgo_corrupcion = ind_riesgo_corrupcion) %>% 
      rename(id = codigo_municipio_ent)
  } else{
    df_mapa <- data %>%
      mutate(codigo_departamento_ent = as.character(codigo_departamento_ent),
             codigo_departamento_ent = 
               ifelse(str_length(codigo_departamento_ent) == 2,
                      codigo_departamento_ent, 
                      paste0("0", codigo_departamento_ent))) %>%
      filter(!is.na(codigo_departamento_ent)) %>%
      mutate(nombre_lugar = departamento_entidad) %>%
      group_by(nombre_lugar, codigo_departamento_ent) %>% 
      summarise(oferentes_prom = oferentes_prom,
                ofertas_prom = ofertas_prom, 
                perc_contr_cerrada_num = perc_contr_cerrada_num, 
                perc_contr_cerrada_val = perc_contr_cerrada_val, 
                HHI_cant = HHI_cant, 
                HHI_val = HHI_val, 
                ID_cant = ID_cant, 
                ID_val = ID_val,
                ganadoras = ganadoras, 
                IC4K_cant = IC4K_cant, 
                IC4K_val = IC4K_val, 
                perc_tiempo_adiciones = perc_tiempo_adiciones, 
                perc_valor_adiciones = perc_valor_adiciones,
                ind_riesgo_corrupcion = ind_riesgo_corrupcion) %>% 
      rename(id = codigo_departamento_ent)
  }
  
  # b. Botón 2: Elección del indicador
  df_mapa <-  df_mapa %>% 
    ungroup() %>%
    select(id, nombre_lugar, all_of(indicador)) %>% 
    rename(x = indicador) %>%
    arrange(desc(x))
  
  #2. Escala y color de los circulos en el mapa
  # Condición para datos porcentuales [%]
  
  paleta <- c('#31a354','#addd8e', '#ffeda0', '#feb24c','#f03b20')
  
  if (indicador != 'oferentes_prom' & indicador != 'ofertas_prom') {
    
    df_mapa <- df_mapa %>% 
      mutate(color = case_when(x <= 44.4 ~ paleta[1],
                               x > 44.4 & x <= 59.9 ~ paleta[2],
                               x > 59.9 & x <= 74.4 ~ paleta[3],
                               x > 74.4 & x <= 89.4 ~ paleta[4],
                               x > 89.4 & x <= 100 ~ paleta[5]))
    
    labels <- c('Bajo (0-44.4)','Moderado (44.5-59.9)',
                'Medio (60-74.4)','Alto (74.5-89.4)',
                'Muy alto (89.5-100)')
    
    titulo <- 'Riesgo (%)'
    
  } else {
    
    df_mapa <- df_mapa %>% 
      mutate(color = case_when(
        x <= quantile(df_mapa$x, .444, na.rm = T)[[1]] ~ paleta[5],
                  x > quantile(df_mapa$x, .444, na.rm = T)[[1]] & 
                    x <= quantile(df_mapa$x, .599, na.rm = T)[[1]] ~ paleta[4],
                  x > quantile(df_mapa$x, .599, na.rm = T)[[1]] & 
                    x <= quantile(df_mapa$x, .744, na.rm = T)[[1]] ~ paleta[3],
                  x > quantile(df_mapa$x, .744, na.rm = T)[[1]] & 
                    x <= quantile(df_mapa$x, .894, na.rm = T)[[1]] ~ paleta[2],
                  x > quantile(df_mapa$x, .894, na.rm = T)[[1]] & 
                    x <= quantile(df_mapa$x, 1, na.rm = T)[[1]] ~ paleta[1]))
    
    labels <- c(paste0('Muy alto (0-', 
                    round(quantile(df_mapa$x, .444, na.rm = T)[[1]], 1),')'),
                paste0('Alto (', 
                    round(quantile(df_mapa$x, .445, na.rm = T)[[1]], 1),'-',
                    round(quantile(df_mapa$x, .599, na.rm = T)[[1]], 1), ')'),
                paste0('Medio (', 
                    round(quantile(df_mapa$x, .60, na.rm = T)[[1]]),'-',
                    round(quantile(df_mapa$x, .744, na.rm = T)[[1]], 1),')'),
                paste0('Moderado (', 
                    round(quantile(df_mapa$x, .745, na.rm = T)[[1]], 1),'-',
                    round(quantile(df_mapa$x, .894, na.rm = T)[[1]], 1),')'),
                paste0('Bajo (',
                    round(quantile(df_mapa$x, .895, na.rm = T)[[1]], 1),'-',
                    round(quantile(df_mapa$x, 1, na.rm = T)[[1]], 1),')'))
    
    paleta <- paleta[5:1]
    
    titulo <- 'Riesgo (Valor)'
  }
  
  #"darkturquoise", mediumspringgreen" , "limegreen" , "darkcyan"
  # Eliminar del data frame df_mapa Valores N/A
  df_mapa <- na.omit(df_mapa)
  
  if (lugar != 'Nacional') {
    
    cod <- str_sub(string = df_mapa[1, "id"], start = 1, end = 2)
    df_mapa <- merge(x = municipios, y = df_mapa, by = "id", all.y = T)
    df_mapa <- df_mapa[df_mapa$id_depto == cod, ]
    
  } else {
    
    df_mapa <- merge(x = departamentos, y = df_mapa, by = "id", all.y = T)
    
  }
  
  
  # 3. Etiqueta
  if(indicador != 'Oferentes promedio por proceso de selección' & 
     indicador != 'Participación promedio de oferentes') 
    #Condición para los indicadores [%]  
  {
    if (lugar != 'Nacional') {
      
      etiqueta <- paste(
        sprintf(
        "<dfn>Lugar: </dfn><strong>%s</strong><br/> <dfn>Puntaje: </dfn>%1.3g",
          df_mapa$municipio, df_mapa$x),
        "%") %>% 
        lapply(htmltools::HTML)
      
    } else {
      
      etiqueta <- paste(
        sprintf(
        "<dfn>Lugar: </dfn><strong>%s</strong><br/> <dfn>Puntaje: </dfn>%1.3g",
          df_mapa$depto, df_mapa$x),
        "%") %>% 
        lapply(htmltools::HTML)
      
    }
  } else { 
    if (lugar != 'Nacional') {
      
      etiqueta <- paste(
        sprintf(
        "<dfn>Lugar: </dfn><strong>%s</strong><br/> <dfn>Cantidad: </dfn>%1.3g",
          df_mapa$municipio, df_mapa$x)) %>% 
        lapply(htmltools::HTML)
      
    } else {
      
      etiqueta <- paste(
        sprintf(
        "<dfn>Lugar: </dfn><strong>%s</strong><br/> <dfn>Cantidad: </dfn>%1.3g",
          df_mapa$depto, df_mapa$x)) %>% 
        lapply(htmltools::HTML)
      
    }
    
  }
  
  # 4. Construcción de la gráfica
  map <- leaflet(data = df_mapa) %>% 
    addTiles() %>%
    addProviderTiles("Wikimedia") %>%
    addPolygons(color = ~'#292929', weight = 0.5, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.7,
                fillColor = ~color,
                label = ~etiqueta,
                labelOptions = labelOptions(
                  style = list(
                    "color" = "black",
                    "font-family" = "Arial",  #Tipo fuente
                    "font-size" = "12px",
                    "border-color" = "rgba(0,88,59,0.5)")),
                highlightOptions = highlightOptions(color = "white", 
                                        weight = 2, bringToFront = TRUE)) %>% 
    addLegendCustom(colors = paleta, labels = labels, 
                    opacity = 0.5, title = titulo)
  
  return(map)
}

# Funcion del top por region del riesgo de corrupcion
grafica_top_Dto_Mun_ind <- function(data, lugar, indicador){
  # 1. Recibir retorno de los botones
  # a. Botón 1: Nacional o departamentos
  if (lugar != "Nacional") {
    top_Dto_Mun <- data %>% 
      filter(departamento_entidad == lugar) %>%
      mutate(nombre_lugar = municipio_entidad) %>%
      group_by(nombre_lugar, codigo_municipio_ent) %>%
      summarise(oferentes_prom = oferentes_prom, 
                ofertas_prom = ofertas_prom, 
                perc_contr_cerrada_num = perc_contr_cerrada_num, 
                perc_contr_cerrada_val = perc_contr_cerrada_val, 
                HHI_cant = HHI_cant, 
                HHI_val = HHI_val, 
                ID_cant = ID_cant, 
                ID_val = ID_val, 
                ganadoras = ganadoras, 
                IC4K_cant = IC4K_cant, 
                IC4K_val = IC4K_val, 
                perc_tiempo_adiciones = perc_tiempo_adiciones, 
                perc_valor_adiciones = perc_valor_adiciones,
                ind_riesgo_corrupcion = ind_riesgo_corrupcion) 
    rango <- 'municipios'
  } else{
    top_Dto_Mun <- data %>%
      mutate(nombre_lugar = departamento_entidad) %>%
      group_by(nombre_lugar) %>%
      summarise(oferentes_prom = oferentes_prom, 
                ofertas_prom = ofertas_prom, 
                perc_contr_cerrada_num = perc_contr_cerrada_num, 
                perc_contr_cerrada_val = perc_contr_cerrada_val, 
                HHI_cant = HHI_cant, 
                HHI_val = HHI_val, 
                ID_cant = ID_cant, 
                ID_val = ID_val, 
                ganadoras = ganadoras, 
                IC4K_cant = IC4K_cant, 
                IC4K_val = IC4K_val, 
                perc_tiempo_adiciones = perc_tiempo_adiciones, 
                perc_valor_adiciones = perc_valor_adiciones,
                ind_riesgo_corrupcion = ind_riesgo_corrupcion)
    rango <- 'departamento'
  }
  
  # b. Botón 2: indicador de riesgo de corrupción
  top_Dto_Mun <-  top_Dto_Mun %>% 
    ungroup() %>%
    select(nombre_lugar, all_of(indicador)) %>% 
    rename(x = indicador) %>%
    mutate(x = round(x, 2)) %>% 
    arrange(desc(x)) %>% head(20)
  
  #2. Establecer eje y colo
  # Establecer eje
  if (indicador != 'oferentes_prom' & indicador != 'ofertas_prom') {
    #Condición para los indicadores [%] 
    eje = "Puntuación (%)"    
  }else {
    eje = "Cantidad promedio" 
  }
  
  # Establecer color
  paleta <- c('#31a354','#addd8e', '#ffeda0', '#feb24c','#f03b20')
  
  if (indicador != 'oferentes_prom' & indicador != 'ofertas_prom') {
    #Condición para los indicadores [%] 
    
    top_Dto_Mun <- top_Dto_Mun %>% 
      mutate(color = case_when(x <= 44.4 ~ paleta[1],
                               x > 44.4 & x <= 59.9 ~ paleta[2],
                               x > 59.9 & x <= 74.4 ~ paleta[3],
                               x > 74.4 & x <= 89.4 ~ paleta[4],
                               x > 89.4 & x <= 100 ~ paleta[5]))      
  }else {
    
    top_Dto_Mun <- top_Dto_Mun %>% 
      mutate(color = case_when(
        x <= quantile(top_Dto_Mun$x, .444, na.rm = T)[[1]] ~ paleta[5],
        x > quantile(top_Dto_Mun$x, .444, na.rm = T)[[1]] & 
          x <= quantile(top_Dto_Mun$x, .599, na.rm = T)[[1]] ~ paleta[4],
        x > quantile(top_Dto_Mun$x, .599, na.rm = T)[[1]] & 
          x <= quantile(top_Dto_Mun$x, .744, na.rm = T)[[1]] ~ paleta[3],
        x > quantile(top_Dto_Mun$x, .744, na.rm = T)[[1]] & 
          x <= quantile(top_Dto_Mun$x, .894, na.rm = T)[[1]] ~ paleta[2],
        x > quantile(top_Dto_Mun$x, .894, na.rm = T)[[1]] & 
          x <= quantile(top_Dto_Mun$x, 1, na.rm = T)[[1]] ~ paleta[1])) 
  }
  
  
  # 3. Construcción de la gráfica
  
  top_Dto_Mun <- top_Dto_Mun %>% 
    mutate(as.character(nombre_lugar)) %>%
    mutate(etiqueta = case_when(str_length(nombre_lugar) >= 10 ~ 
                                  paste0(str_sub(nombre_lugar, 1, 10), "..."),
                                TRUE ~ nombre_lugar))
  plot <- plot_ly(data = top_Dto_Mun,
                  type = "bar",
                  x = ~x,
                  y = reorder(top_Dto_Mun$etiqueta, top_Dto_Mun$x),
                  hovertext = ~nombre_lugar,
                hovertemplate = "%{hovertext} <extra>Valor: %{value} </extra>",
                  orientation = 'h',
                  marker = list(color = ~color)) %>% 
    layout(xaxis = list(title =  eje,
                        yaxis = list(title = "Lugar")),
           title = paste0("Top 15 ", rango)) %>% 
    config(locale = "es")
  
  return(plot)
  
}

burbuja_funcion <- function(data, input = "ind_riesgo_corrupcion"){
  
  #Generar el filtro
  burbuja <- data %>%  
    filter(indicador == input) %>%
    group_by(nombre_grupo) %>% summarise(valorb= mean(valor)) 
  
  burbuja$id <- seq(from = 1, to = nrow(burbuja), by = 1)
  burbuja$valorb <- round(burbuja$valorb, 3)
  
  burbuja <- burbuja %>%  mutate(cod = case_when( id == 1 ~ "D",
                                                  id == 2 ~ "C",
                                                  id == 3 ~ "A",
                                                  id == 4 ~ "B",
                                                  id == 5 ~ "E",
                                                  id == 6 ~ "F",
                                                  id == 7 ~ "G",))
  
  # Agregar una columna con el texto a mostrar para cada burbuja 
  burbuja$text <- paste("Grupo: ", burbuja$nombre_grupo, "\n", 
                        "Valor: ", burbuja$valorb, " %" , "\n",
                        "Id: ", burbuja$cod)
  
  # Generar el layout
  packing <- circleProgressiveLayout(burbuja$valorb, sizetype='radius')
  burbuja <- cbind(burbuja, packing)
  dat.gg <- circleLayoutVertices(packing, npoints=50)
  ids <- burbuja %>% dplyr::select(id, valorb)
  dat.gg <- merge(dat.gg, ids, by = "id")
  dat.gg <- dat.gg %>%  mutate(colorH = case_when( valorb <= 44.4 ~ "#31a354",
                                                   valorb <= 59.9 ~ "#addd8e",
                                                   valorb <= 74.4 ~ "#ffeda0",
                                                   valorb <= 89.4 ~ "#feb24c",
                                                   valorb > 89.4 ~ "#f03b20"))
  
  
  # Hacer el gr?fico con ggplot2
  
  tooltip_css <- "color:black;font-style:italic;padding:10px;border-radius:5px;"
  
  p <- ggplot() + 
    geom_polygon_interactive(data = dat.gg, 
                             aes(x, y, group = valorb, fill=valorb, 
                                 tooltip = burbuja$text[id], data_id = valorb), 
                             colour = "black", 
                             alpha = 0.5,
                             fill= dat.gg$colorH) +
    scale_fill_viridis() +
    geom_text(data = burbuja, 
              aes(x, y, label = cod, family = 'courier'), 
              size=7,
              color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  # Turn it interactive con el ejemplo de ggiraph
  plot_burbuja <-  girafe(ggobj = p, 
                          width_svg = 7, 
                          height_svg = 7, 
                          options = list(opts_tooltip(use_fill = TRUE, 
                                            css = tooltip_css, opacity = .9),
                                         opts_hover_inv(css = "opacity:0.1;"),
                                         opts_hover(css = "fill:#0032cc;"),
                                         opts_sizing(width = 0.7))) 
  plot_burbuja              
} 

OrdenEntidad_Indicador <- function(data){
  
  #Agrupar por orden Entidad
  indicadoresO <- data %>% 
    mutate(orden_entidad = case_when(
      str_detect(orden_entidad, "Nacional") ~ "Nacional",
      orden_entidad %in% c('TDC', 'TDD', "Territorial") ~ 'Departamental',
      orden_entidad %in% c('TDM1', 'TDM2', 'TDM3') ~ 'Municipal 1-3',
      orden_entidad %in% c('TDM4', 'TDM5', 'TDM6') ~ 'Municipal 4-6',
      TRUE ~ orden_entidad)) %>% 
    group_by(orden_entidad) %>% 
    summarise(ind_prom_oferentes = mean(ind_prom_oferentes, na.rm = T), 
              perc_contr_cerrada_num = mean(perc_contr_cerrada_num, na.rm = T), 
              perc_contr_cerrada_val = mean(perc_contr_cerrada_val, na.rm = T), 
              HHI_cant = mean(HHI_cant, na.rm = T), 
              HHI_val = mean(HHI_val, na.rm = T), 
              ID_cant = mean(ID_cant, na.rm = T), 
              ID_val = mean(ID_val, na.rm = T), 
              ind_prom_ofertas = mean(ind_prom_ofertas, na.rm = T), 
              ganadoras = mean(ganadoras, na.rm = T), 
              IC4K_cant = mean(IC4K_cant, na.rm = T), 
              IC4K_val = mean(IC4K_val, na.rm = T), 
              perc_tiempo_adiciones = mean(perc_tiempo_adiciones, na.rm = T), 
              ind_valor_adiciones = mean(ind_valor_adiciones, na.rm = T), 
              ind_riesgo_corrupcion = 
                mean(ind_riesgo_corrupcion, na.rm = T)) %>% 
    mutate(ind_prom_oferentes = sprintf("%.2f",ind_prom_oferentes), 
           perc_contr_cerrada_num = sprintf("%.2f",perc_contr_cerrada_num), 
           perc_contr_cerrada_val = sprintf("%.2f",perc_contr_cerrada_val), 
           HHI_cant = sprintf("%.2f",HHI_cant), 
           HHI_val = sprintf("%.2f",HHI_val), 
           ID_cant = sprintf("%.2f",ID_cant), 
           ID_val = sprintf("%.2f",ID_val), 
           ind_prom_ofertas = sprintf("%.2f",ind_prom_ofertas), 
           ganadoras = sprintf("%.2f", ganadoras), 
           IC4K_cant = sprintf("%.2f",IC4K_cant), 
           IC4K_val = sprintf("%.2f",IC4K_val), 
           perc_tiempo_adiciones = sprintf("%.2f",perc_tiempo_adiciones), 
           ind_valor_adiciones = sprintf("%.2f", ind_valor_adiciones), 
           ind_riesgo_corrupcion = sprintf("%.2f",ind_riesgo_corrupcion)) %>%
    rename('Indicador de riesgo de corrupción' = 'ind_riesgo_corrupcion',
           'Oferentes promedio por proceso de selección' = 'ind_prom_oferentes', 
           'Participación promedio de oferentes' = "ind_prom_ofertas",
           'Índ. de empresas ganadoras diferentes' = 'ganadoras', 
           'Porcentaje de procesos no abiertos' = 'perc_contr_cerrada_num' , 
      'Porcentaje del valor de procesos no abiertos' = 'perc_contr_cerrada_val',
           'Índ. concentración IHH por número de contratos' = 'HHI_cant',
           'Índ. concentración IHH por valor de contratos' = 'HHI_val', 
           'Índ. Diversidad ID por número de contratos' = 'ID_cant', 
           'Índ. Diversidad ID por valor de contratos' = 'ID_val', 
           'Índ. concentración para las 4 mayores empresas' = 'IC4K_cant',
      'Índ. concentración en valor para las 4 mayores empresas' = 'IC4K_val',
           'Porcentaje de contratos con modificaciones en tiempo' = 
             'perc_tiempo_adiciones', 
           'Porcentaje del valor de las adiciones' = 
             'ind_valor_adiciones') 
  
  
  indicadoresO <- indicadoresO %>% 
    filter(!(orden_entidad %in%  c("No Definido", "Área Metropolitana", 
                                   "Corporación Autónoma")))
  #Transformar
  indicadoresO <- as.data.frame(t(indicadoresO))
  
  indicadoresO$`Orden Entidad` <- row.names(indicadoresO)
  
  row.names(indicadoresO) <- NULL
  #Agregar Numeros como nombres de la primera columna
  colnames(indicadoresO) <- as.character(indicadoresO[1, ])
  indicadoresO <- indicadoresO[-1, ]
  #Ordenar Columnas
  indicadoresO[indicadoresO == "NaN"] <- "-"
  
  indicadoresO <- indicadoresO %>% 
    rename(Indicador = orden_entidad)
  
  indicadoresO <- indicadoresO[, c(ncol(indicadoresO), 
                                   1:(ncol(indicadoresO)- 1))]
  
  indicadoresO <- tibble(indicadoresO)
  
  indicadoresO <- indicadoresO %>%
    select(Indicador, "Nacional", "Departamental", "Distrito Capital",
           "Municipal 1-3", "Municipal 4-6")
  
  indicadoresO <- formattable(indicadoresO,
                              align = c("r", 
                                        rep("c", ncol(indicadoresO) - 1)),
                              list(Indicador = formatter("span",
                                    style = x ~ style("font-size" = "12px"))))
  
  return(indicadoresO)
  
}

Entidades_Indicador <- function(data, input1 = "Nacional", 
                                input2 = "ind_riesgo_corrupcion"){
  
  # Nombres de los indicadores
  indicadores <- c(
    'Indicador de riesgo de corrupción' = 'ind_riesgo_corrupcion', 
    'Oferentes promedio por proceso de selección' = 'oferentes_prom', 
    'Participación promedio de oferentes' = "ofertas_prom",
    'Índice de empresas ganadoras diferentes (por cada 100 contratos)' = 
      'ganadoras', 
    'Porcentaje de procesos no abiertos' = 'perc_contr_cerrada_num' , 
    'Porcentaje del valor de procesos no abiertos' = 'perc_contr_cerrada_val', 
    'Índice de concentración IHH por número de contratos' = 'HHI_cant',
    'Índice de concentración IHH por valor de contratos' = 'HHI_val', 
    'Índice de Diversidad ID por número de contratos' = 'ID_cant', 
    'Índice de Diversidad ID por valor de contratos' = 'ID_val', 
    'Índice de concentración para las 4 mayores empresas' = 'IC4K_cant',
    'Índice de concentración en valor para las 4 mayores empresas' = 'IC4K_val',
    'Índice del porcentaje de contratos con modificaciones en tiempo' = 
      'perc_tiempo_adiciones', 
    'Índice del porcentaje del valor de las adiciones' = 'perc_valor_adiciones')
  
  #Eliminar columnas que no se van a utilizar 
  q3 <- data %>%
    select(-orden_entidad, -latitud, -longitud)
  
  #1. Recibir retorno de los 2 botones
  
  #Filtrar por Nacional o departamentos
  # a. Botón 1: Nacional o departamentos
  if (input1 == "Nacional") {
    
    # Consulta
    q3 <- q3 %>%
      mutate(nombre_entidad = paste(nombre_entidad, 
                                    departamento_entidad, sep = " - "))
    q3 <- q3 %>%
      select(-municipio_entidad, -departamento_entidad)
  } else {
    # Consulta
    q3 <- q3 %>% 
      filter(departamento_entidad == input1)
    
    q3 <- q3 %>%
      select(-departamento_entidad)
  }
  # b. Botón 2: Elección del indicador
  
  
  if(input2 == 'ind_riesgo_corrupcion'){
    
    name <- names(indicadores[indicadores == input2])
    
    q3 <-  q3 %>% 
      select(nombre_entidad, input2) %>%
      filter(ind_riesgo_corrupcion > 0, !is.na(ind_riesgo_corrupcion)) %>% 
      arrange(desc(ind_riesgo_corrupcion))%>%
      mutate(ind_riesgo_corrupcion = as.numeric(sprintf("%.2f",
                                                      ind_riesgo_corrupcion)))
    
    names(q3) <- c("Nombre Entidad", "Indicador de riesgo de corrupción")
  } else {
    
    name <- names(indicadores[indicadores == input2])
    
    q3 <-  q3 %>% 
      select(nombre_entidad, input2, ind_riesgo_corrupcion) %>%
      rename(indicador = input2) %>% 
      filter(ind_riesgo_corrupcion > 0, !is.na(ind_riesgo_corrupcion)) %>% 
      arrange(desc(indicador), desc(ind_riesgo_corrupcion)) %>%
      mutate(ind_riesgo_corrupcion = as.numeric(sprintf("%.2f",
                                                    ind_riesgo_corrupcion)),
             indicador = as.numeric(sprintf("%.2f", indicador)))
    
    names(q3) <- c("Nombre Entidad", name,
                   "Indicador de riesgo de corrupción")
    
    
  }
  
  q3 <- head(q3, 20)
  
  #Formato Color
  #Diseño
  q3 <- formattable(q3, align = c("l", rep("r", NCOL(q3) - 1)), 
                    list(`Nombre Entidad` = formatter("span", 
                          style = ~ style(color = "grey", font.weight = "bold",
                                                        "font-size" = "12px")), 
                         area(col  = 2:ncol(q3)) ~ function(x) 
                           percent(x / 100, digits = 0),
                         area(col = 2:ncol(q3))  ~  
                           color_tile("#ffce33","#ff3f3f")))
  
  #Verde  #6cda6c
  
  return(q3)
  
}

donut_ind_entidad <- function(data, dpto = "Nacional", 
                              ind = "ind_riesgo_corrupcion"){
  
  # parametros
  # * data: conjunto de datos de contratacion.
  # * input: tipo de calculo: valor o cantidad.
  # * variable: variable con relación al proceso de contratacion.
  # * dpto: departamento por el cual se realiza el filtro.
  
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional') {
    data <- data %>%
      filter(departamento_entidad == dpto)
  }
  
  # Consulta
  df <- data %>%
    rename(indicador = ind) %>%
    mutate(grupo = case_when(indicador <= 44.4 ~ 'Bajo', 
                             indicador <= 59.9 ~ 'Moderado', 
                             indicador <= 74.4 ~ 'Medio', 
                             indicador <= 89.4 ~ 'Alto', 
                             TRUE ~ 'Muy alto'),
           orden_grupo = case_when(grupo == 'Bajo' ~ 'A', 
                                   grupo == 'Moderado' ~ 'B', 
                                   grupo == 'Medio' ~ 'C', 
                                   grupo == 'Alto' ~ 'D', 
                                   TRUE ~ 'E')) %>% 
    group_by(grupo, orden_grupo) %>% 
    count() %>% arrange(orden_grupo)
  
  # colores
  paleta <- c('Bajo' = '#31a354', 'Medio' =' #addd8e',
              'Moderado' = '#ffeda0', 'Alto' = '#feb24c',
              'Muy alto' = '#f03b20')
  
  paleta <- paleta[which(names(paleta) %in% unique(df$grupo))]
  
  valor_t <- sum(df$n)
  
  df <- df %>% 
    mutate(porcion = n/valor_t)
  
  # grafico
  fig <- plot_ly(data = df,
                 labels = ~grupo, 
                 
                 values = ~n,
                 
                 textinfo ='percent',
                 
                 textposition ='outside',
                 
                 hovertemplate = 
                   paste("<b>%{label}</b>",
                         "<br><i>Cantidad Entidades:</i>",
                         " %{value:.0f}</br>", 
                         "<i>Porcentaje: </i>",
                         "%{percent:.2%}</br>",
                         "<extra></extra>",
                         sep = ""),
                 marker = list(
                   colors = paleta),
                 
                 domain = list(
                   x = c(0.07,0.93),
                   y = c(0.03,0.94)
                 ),
                 showlegend = FALSE) %>%
    
    add_pie(hole = 0.6) %>%
    
    layout(title = list(text = paste0('Nivel de riesgo por \n', 
                              names(list_ind[list_ind == ind]), '\n en ',
                              names(list_dep[list_dep == dpto])),
                        font = list(size = 12)),
           legend = list(x = 0.3, y = -0.3),
           margin = list(
             l = 20, r = 20, t = 40, b = 10)) %>% 
    config(locale = "es")
  
  # salida del grafico
  return(fig)
}

mean_dpto_ind <- function(data, dpto = "Nacional", 
                          ind = "ind_riesgo_corrupcion"){
  # La seleccion es diferente al Nacional
  if (dpto != 'Nacional')
    data <- data %>%
      filter(departamento_entidad == dpto)
  
  # Consulta
  df <- data %>%
    rename(indicador = ind)
  
  return(round(mean(df$indicador, na.rm = T), 2))
}

ranking_dpto_ind <- function(data, dpto = "Nacional", 
                             ind = "ind_riesgo_corrupcion"){
  if (dpto == 'Nacional'){
    rank <- 'NA'
  } else {
    df <- data %>%
      rename(indicador = ind) %>%
      group_by(departamento_entidad) %>%
      summarize(mean = mean(indicador, na.rm = T)) %>%
      arrange(desc(mean))
    
    df$rank <- rank(df$mean)
    
    rank <- df[df$departamento_entidad == dpto, 'rank'][[1]]
  }
  
  return(rank)
}

# 4. Utileria ----
## 4.1. Lista de municipios ----
list_dep <- c("Nacional" = "Nacional",
              "Amazonas" = "Amazonas",
              "Antioquia"= "Antioquia",
              "Arauca" = "Arauca",
              "Atlántico" = "Atlántico",
              "Bogotá D.C." = "Bogotá D.C.",
              "Bolívar" = "Bolívar",
              "Boyacá" = "Boyacá",
              "Caldas" = "Caldas",
              "Caquetá" = "Caquetá",
              "Casanare" = "Casanare",
              "Cauca" = "Cauca",
              "Cesar" = "Cesar", 
              "Chocó" = "Chocó", 
              "Córdoba" = "Córdoba",
              "Cundinamarca" = "Cundinamarca",
              "Guainía" = "Guainía",
              "Guaviare" = "Guaviare", 
              "Huila" = "Huila",
              "La Guajira" = "La Guajira",
              "Magdalena" = "Magdalena",
              "Meta" = "Meta", 
              "Nariño" = "Nariño", 
              "Norte De Santander"="Norte de Santander",
              "Putumayo" = "Putumayo",
              "Quindío" = "Quindío", 
              "Risaralda" = "Risaralda",
              "Archipiélago de San Andrés" =
                "Archipiélago de San Andrés",
              "Santander" = "Santander",
              "Sucre" = "Sucre", 
              "Tolima" = "Tolima",
              "Valle del Cauca" = "Valle del Cauca", 
              "Vaupés" = "Vaupés", 
              "Vichada" = "Vichada")

## 4.2. Lista de indicadores ----
list_ind <- c('Indicador de riesgo de corrupción' = 'ind_riesgo_corrupcion', 
       'Oferentes promedio por proceso de selección' = 'oferentes_prom', 
       'Participación promedio de oferentes' = "ofertas_prom",
       'Índice de empresas ganadoras diferentes (por cada 100 contratos)' = 
           'ganadoras', 
       'Porcentaje de procesos no abiertos' = 'perc_contr_cerrada_num' , 
       'Porcentaje del valor de procesos no abiertos' = 
         'perc_contr_cerrada_val', 
       'Índice de concentración IHH por número de contratos' = 'HHI_cant',
       'Índice de concentración IHH por valor de contratos' = 'HHI_val', 
       'Índice de Diversidad ID por número de contratos' = 'ID_cant', 
       'Índice de Diversidad ID por valor de contratos' = 'ID_val', 
       'Índice de concentración para las 4 mayores empresas' = 'IC4K_cant',
       'Índice de concentración en valor para las 4 mayores empresas' = 
         'IC4K_val',
       'Índice del porcentaje de contratos con modificaciones en tiempo' = 
       'perc_tiempo_adiciones', 
       'Índice del porcentaje del valor de las adiciones' = 
         'perc_valor_adiciones')

## 4.3. Personalizacion del tema ----
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(33,37,41)"
  ,primaryFontColor = "rgb(245,245,245)"
  ,infoFontColor = "rgb(245,245,245)"
  ,successFontColor = "rgb(33,37,41)"
  ,warningFontColor = "rgb(33,37,41)"
  ,dangerFontColor = "rgb(33,37,41)"
  ,bodyBackColor = "rgb(245,245,245)"
  
  ### header
  ,logoBackColor = "rgb(98,208,173)"
  
  ,headerButtonBackColor = "rgb(98,208,173)"
  ,headerButtonIconColor = "rgb(0,96,122)"
  ,headerButtonBackColorHover = "rgb(20,154,128)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(98,208,173)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(0,96,122)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "inherit"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(0,96,122)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(245,245,245)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(0,131,163)"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(0,96,122)"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 19
  ,boxDefaultColor = "rgb(52,152,219)"
  ,boxPrimaryColor = "rgb(0,96,122)"
  ,boxInfoColor = "rgb(52,152,219)"
  ,boxSuccessColor = "rgb(85,221,94)"
  ,boxWarningColor = "rgb(243,156,18)"
  ,boxDangerColor = "rgb(231,76,60)"
  
  ,tabBoxTabColor = "rgb(0,96,122)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(24, 188, 156)"
  ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(255,255,255)"
  ,tabBoxBorderRadius = 10
  
  ### inputs
  ,buttonBackColor = "rgb(0,96,122)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(0,96,122)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(0,131,163)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(0,131,163)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(206,212,218)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(89,126,162)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(236,240,241)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

## 4.4. Panel ayuda ----
obtener_noti_ayud <- function(tab){
  
  if (tab == "general") {
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Aclaraciones"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("El valor del contrato hace referencia al",
                      " valor inicial constatado más las adiciones."),
        icon = icon("coins")
      ),
      notificationItem(
        text = paste0("MM: Miles de Millones."),
        icon = icon("dollar-sign")
      ),
      notificationItem(
        text = paste0("Los datos pertenecen a los contratos",
                      " registrados en el SECOP I y ",
                      "SECOP II: PROCESOS, donde hacen",
                      " referencia a la emergencia sanitaria en ",
                      "el campo 'detalle del objeto'."),
        icon = icon("database")
      ),
      notificationItem(
        text = paste0("El seguimiento de los contratos incia desde",
                      " el momento en el que hace referencia la",
                      " emergencia saniaria (", 
                      min(contratos$fecha_firma), "), hasta la ",
                      "última actualización (",
                      max(contratos$fecha_firma), ")."),
        icon = icon("calendar-alt")
      )
    )
    
  } else if (tab == "mapa"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Aclaraciones"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      
      notificationItem(
        text = paste0("La ubicación de los contratos se tomo por ",
                      "referencia al campo de municipio de la ",
                      "entidad."),
        icon = icon("map-marker-alt")
      ),
      notificationItem(
        text = paste0("El valor 'No definido' en el top hace ",
                      "referencia a contratos donde la entidad no ",
                      "especifica o mantiene el campo vacio. Por lo ",
                      "tanto, no se registra la información en el mapa."),
        icon = icon("question-circle")
      ),
      notificationItem(
        text = paste0("El valor del porcentaje de cada ubicación ",
                      "es relativo al departamento seleccionado."),
        icon = icon("percentage")
      ),
      notificationItem(
        text = paste0("El valor del contrato hace referencia al ",
                      "valor inicial constatado más las adiciones."),
        icon = icon("coins")
      ),
      notificationItem(
        text = paste0("MM: Miles de Millones."),
        icon = icon("dollar-sign")
      ),
      notificationItem(
        text = paste0("Los datos pertenecen a los contratos",
                      " registrados en el SECOP I y ",
                      "SECOP II: PROCESOS, donde hacen",
                      " referencia a la emergencia sanitaria en ",
                      "el campo 'detalle del objeto'."),
        icon = icon("database")
      ),
      notificationItem(
        text = paste0("El seguimiento de los contratos incia desde",
                      " el momento en el que hace referencia la",
                      " emergencia saniaria (", 
                      min(contratos$fecha_firma), "), hasta la ",
                      "última actualización (",
                      max(contratos$fecha_firma), ")."),
        icon = icon("calendar-alt")
      )
    )
    
  } else if (tab == "procesos"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Aclaraciones"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("El valor del contrato hace referencia al",
                      " valor inicial constatado más las adiciones."),
        icon = icon("coins")
      ),
      notificationItem(
        text = paste0("MM: Miles de Millones."),
        icon = icon("dollar-sign")
      ),
      notificationItem(
        text = paste0("Los datos pertenecen a los contratos",
                      " registrados en el SECOP I y ",
                      "SECOP II: PROCESOS, donde hacen",
                      " referencia a la emergencia sanitaria en ",
                      "el campo 'detalle del objeto'."),
        icon = icon("database")
      ),
      notificationItem(
        text = paste0("El seguimiento de los contratos inicia ",
                      "desde el momento en el que hace referencia ",
                      "la emergencia saniaria (", 
                      min(contratos$fecha_firma), "), hasta la ",
                      "última actualización (",
                      max(contratos$fecha_firma), ")."),
        icon = icon("calendar-alt")
      )
    )
    
  } else if (tab == "objeto"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Aclaraciones"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      
      notificationItem(
        text = paste0("La clasifiación por 'grupos de productos', ",
                      "es una modificación al nivel de segmento ",
                      "del estándar UNSPSC, para obtener 7 grupos",
                      "principales."),
        icon = icon("exclamation")
      ),
      notificationItem(
        text = paste0("El valor del contrato hace referencia al ",
                      "valor inicial constatado más las adiciones."),
        icon = icon("coins")
      ),
      notificationItem(
        text = paste0("MM: Miles de Millones."),
        icon = icon("dollar-sign")
      ),
      notificationItem(
        text = paste0("Los datos pertenecen a los contratos",
                      " registrados en el SECOP I y ",
                      "SECOP II: PROCESOS, donde hacen",
                      " referencia a la emergencia sanitaria en ",
                      "el campo 'detalle del objeto'."),
        icon = icon("database")
      ),
      notificationItem(
        text = paste0("El seguimiento de los contratos incia desde",
                      " el momento en el que hace referencia la",
                      " emergencia saniaria (", 
                      min(contratos$fecha_firma), "), hasta la ",
                      "última actualización (",
                      max(contratos$fecha_firma), ")."),
        icon = icon("calendar-alt")
      )
    )
    
  } else if(tab == "actores"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Aclaraciones"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      
      notificationItem(
        text = paste0("La ubicación de los contratos se tomo por ",
                      "referencia al campo de municipio de la ",
                      "entidad y el contratista respectivamente."),
        icon = icon("map-marker-alt")
      ),
      notificationItem(
        text = paste0("Indicador % Contratación Directa: Cantidad ",
                      "de contratos bajo el proceso de ",
                      "contratación sobre el total de contratos ",
                      "para de la entidad o contratista."),
        icon = icon("percentage")
      ),
      notificationItem(
        text = paste0("Indicador % Adición: La porción ",
                      "del valor adicional sobre el valor total ",
                      " en los contratos de la entidad ",
                      "o contratista."),
        icon = icon("plus")
      ),
      notificationItem(
        text = paste0("El valor del contrato hace referencia al",
                      " valor inicial constatado más las adiciones."),
        icon = icon("coins")
      ),
      notificationItem(
        text = paste0("MM: Miles de Millones."),
        icon = icon("dollar-sign")
      ),
      notificationItem(
        text = paste0("Los datos pertenecen a los contratos",
                      " registrados en el SECOP I y ",
                      "SECOP II: PROCESOS, donde hacen",
                      " referencia a la emergencia sanitaria en ",
                      "el campo 'detalle del objeto'."),
        icon = icon("database")
      ),
      notificationItem(
        text = paste0("El seguimiento de los contratos incia desde",
                      " el momento en el que hace referencia la",
                      " emergencia saniaria (", 
                      min(contratos$fecha_firma), "), hasta la ",
                      "última actualización (",
                      max(contratos$fecha_firma), ")."),
        icon = icon("calendar-alt")
      )
    )
    
  } else {
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Aclaraciones"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Los datos pertenecen a los contratos",
                      " registrados en el SECOP I y ",
                      "SECOP II: PROCESOS, donde hacen",
                      " referencia a la emergencia sanitaria en ",
                      "el campo 'detalle del objeto'."),
        icon = icon("database")
      ),
      notificationItem(
        text = paste0("El seguimiento de los contratos incia desde",
                      " el momento en el que hace referencia la",
                      " emergencia saniaria (", 
                      min(contratos$fecha_firma), "), hasta la ",
                      "última actualización (",
                      max(contratos$fecha_firma), ")."),
        icon = icon("calendar-alt")
      )
    )
  }
  
  return(drM)
}

## 4.5. Panel Notas ----
obtener_noti_glos <- function(tab){
  
  if (tab == "general") {
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Glosario"), 
      icon = icon("book"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Contratación Estatal: ",
                      "Todo acto jurídico generador de obligaciones ",
                      "en el que una de las partes sea una entidad ",
                      "pública."),
        icon = icon("handshake")
      ),
      notificationItem(
        text = paste0("Entidad Estatal: ",
                      "referidas en el artículo 2 de la Ley 80 de 1993, ",
                      "donde tenga una participación mayoritaria con el",
                      " Estado."),
        icon = icon("user-alt")
      ),
      notificationItem(
        text = paste0("Contratista: ",
                      "Cualquier interesado en participar en el ",
                      "Sistema de Compra Pública vendiendo bienes, ",
                      "obras o servicios a las Entidades Estatales."),
        icon = icon("user-alt")
      ),
      notificationItem(
        text = paste0("SECOP: ",
                      "Sistema Electrónico para la Contratación",
                      " Pública conformado por el conjunto de ",
                      "plataformas o soluciones tecnológicas ",
                      "puestas a disposición del Sistema de Compra ",
                      "Pública por Colombia Compra Eficiente o",
                      " quien haga sus veces."),
        icon = icon("store")
      )
    )
    
  } else if (tab == "mapa"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Glosario"), 
      icon = icon("book"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Contratación Estatal: ",
                      "Todo acto jurídico generador de obligaciones ",
                      "en el que una de las partes sea una entidad ",
                      "pública."),
        icon = icon("handshake")
      ),
      notificationItem(
        text = paste0("Clasificador de Bienes y Servicios: ",
                      "Arreglo jerárquico de productos por medio ",
                      "del código estándar UNSPSC, los niveles del",
                      " más general al más especifico son: ",
                      "Segmento, Familia, Clase y Producto."),
        icon = icon("cubes")
      ),
      notificationItem(
        text = paste0("SECOP: ",
                      "Sistema Electrónico para la Contratación",
                      " Pública conformado por el conjunto de ",
                      "plataformas o soluciones tecnológicas ",
                      "puestas a disposición del Sistema de Compra ",
                      "Pública por Colombia Compra Eficiente o",
                      " quien haga sus veces."),
        icon = icon("store")
      )
    )
    
  } else if (tab == "procesos"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Glosario"), 
      icon = icon("book"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Régimen de contratación: Cuerpo de normas ",
                      "que regulan la actividad pre contractual y ",
                      "contractual planteadas en la Ley 80 de 1993, ",
                      "la Ley 1150 de 2007, algunas disposiciones de la ",
                      "Ley 1474 de 2011 y  del Decreto Ley 019 de 2012.",
                      " Sin embargo, el régimen especial hace referencia",
                      " a excepciones en la ley cuando se está en ",
                      "situación de competencia."),
        icon = icon("gavel")
      ),
      notificationItem(
        text = paste0("Tipo de proceso: Las Entidades Estatales",
                      " deben escoger a sus contratistas a través ",
                      "de alguna de las modalidades de ",
                      "selección previstas en la Ley 1150 de 2007.",
                      " Los proceso bajo régimen especial presentan ",
                      "un proceso único."),
        icon = icon("tasks")
      ),
      notificationItem(
        text = paste0("Tipo de contrato: Modelo de contrato ",
                      "en el cual fue adjudicado el contrato."),
        icon = icon("bullseye")
      ),
      notificationItem(
        text = paste0("Estado del proceso: Estado en el que se ",
                      "encuentra actualmente los procesos."),
        icon = icon("spinner")
      ),
      notificationItem(
        text = paste0("Contratación Estatal: ",
                      "Todo acto jurídico generador de obligaciones ",
                      "en el que una de las partes sea una entidad ",
                      "pública."),
        icon = icon("handshake")
      ),
      notificationItem(
        text = paste0("SECOP: ",
                      "Sistema Electrónico para la Contratación",
                      " Pública conformado por el conjunto de ",
                      "plataformas o soluciones tecnológicas ",
                      "puestas a disposición del Sistema de Compra ",
                      "Pública por Colombia Compra Eficiente o",
                      " quien haga sus veces."),
        icon = icon("store")
      )
    )
    
  } else if (tab == "objeto"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Glosario"), 
      icon = icon("book"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Contratación Estatal: ",
                      "Todo acto jurídico generador de obligaciones ",
                      "en el que una de las partes sea una entidad ",
                      "pública."),
        icon = icon("handshake")
      ),
      notificationItem(
        text = paste0("Clasificador de Bienes y Servicios: ",
                      "Arreglo jerárquico de productos por medio ",
                      "del código estándar UNSPSC, los niveles del",
                      " más general al más especifico son: ",
                      "Segmento, Familia, Clase y Producto."),
        icon = icon("cubes")
      ),
      notificationItem(
        text = paste0("SECOP: ",
                      "Sistema Electrónico para la Contratación",
                      " Pública conformado por el conjunto de ",
                      "plataformas o soluciones tecnológicas ",
                      "puestas a disposición del Sistema de Compra ",
                      "Pública por Colombia Compra Eficiente o",
                      " quien haga sus veces."),
        icon = icon("store")
      )
    )
    
  } else if(tab == "actores"){
    
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Glosario"), 
      icon = icon("book"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Contratación Estatal: ",
                      "Todo acto jurídico generador de obligaciones ",
                      "en el que una de las partes sea una entidad ",
                      "pública."),
        icon = icon("handshake")
      ),
      notificationItem(
        text = paste0("Entidad Estatal: ",
                      "referidas en el artículo 2 de la Ley 80 de 1993, ",
                      "donde tenga una participación mayoritaria con el",
                      " Estado."),
        icon = icon("user-alt")
      ),
      notificationItem(
        text = paste0("Contratista: ",
                      "Cualquier interesado en participar en el ",
                      "Sistema de Compra Pública vendiendo bienes, ",
                      "obras o servicios a las Entidades Estatales."),
        icon = icon("user-alt")
      ),
      notificationItem(
        text = paste0("SECOP: ",
                      "Sistema Electrónico para la Contratación",
                      " Pública conformado por el conjunto de ",
                      "plataformas o soluciones tecnológicas ",
                      "puestas a disposición del Sistema de Compra ",
                      "Pública por Colombia Compra Eficiente o",
                      " quien haga sus veces."),
        icon = icon("store")
      )
    )
    
  } else {
    drM <- dropdownMenu(
      type = "notifications", 
      headerText = strong("Glosario"), 
      icon = icon("book"), 
      badgeStatus = NULL,
      notificationItem(
        text = paste0("Contratación Estatal: ",
                      "Todo acto jurídico generador de obligaciones ",
                      "en el que una de las partes sea una entidad ",
                      "pública."),
        icon = icon("handshake")
      ),
      notificationItem(
        text = paste0("Entidad Estatal: ",
                      "referidas en el artículo 2 de la Ley 80 de 1993, ",
                      "donde tenga una participación mayoritaria con el",
                      " Estado."),
        icon = icon("user-alt")
      ),
      notificationItem(
        text = paste0("Contratista: ",
                      "Cualquier interesado en participar en el ",
                      "Sistema de Compra Pública vendiendo bienes, ",
                      "obras o servicios a las Entidades Estatales."),
        icon = icon("user-alt")
      ),
      notificationItem(
        text = paste0("SECOP: ",
                      "Sistema Electrónico para la Contratación",
                      " Pública conformado por el conjunto de ",
                      "plataformas o soluciones tecnológicas ",
                      "puestas a disposición del Sistema de Compra ",
                      "Pública por Colombia Compra Eficiente o",
                      " quien haga sus veces."),
        icon = icon("store")
      )
    )
  }
  
  return(drM)
}

# 4.6. Descripcion de los indicadores----
des_ind <- c(
'ind_riesgo_corrupcion' = paste0('El índice de corrupción para cada entidad se',
    ' calcula como la media aritmética de cada uno de los índices de ',
    'corrupción estimados. El puntaje de cada indicador y del índice está ',
    'entre 0 y 100, donde 100 es el riesgo más alto de corrupción y 0 el ',
    'riesgo más bajo.'),
'oferentes_prom' = paste0('Mide el promedio de oferentes invitados en cada ',
                          'proceso de selección.'),
'ofertas_prom' = paste0('Mide la cantidad de ofertas que recibe la entidad ',
                        'estatal en sus procesos de contratación.'),
'ganadoras' = paste0('Mide el número de empresas diferentes adjudicatarias de',
    ' los procesos de contratación. Dado que cada entidad tiene un volumen ',
    'diferente de contratación, el índice se expresó en número de contratistas',
    ' por cada 100 contratos.'),
'perc_contr_cerrada_num' = paste0('Mide el porcentaje de contratos que la ',
    'entidad realiza bajo la modalidad de contratación directa y régimen ',
    'especial sobre la totalidad de los contratos de la entidad. '),
'perc_contr_cerrada_val' = paste0('Mide el porcentaje del valor de los ',
    'contratos que la entidad realiza bajo la modalidad de contratación ',
    'directa y régimen especial sobre la totalidad de los contratos de la ',
    'entidad.'),
'HHI_cant' = paste0('El índice Herfindahl-Hirschman es una medida del tamaño ',
    'de las empresas en relación con la industria y un indicador de la ',
    'cantidad de competencia entre ellas. Se calcula como la suma de los ',
    'cuadrados de las cuotas de mercado de un contratista dentro de la cuota ',
    'de mercado de toda la entidad. Teniendo en cuenta que el indicador se ',
    'da en un rango de [0,10000], se estandariza el indicador ',
    'dividiendo el HHI entre 10000.'),
'HHI_val' = paste0('El índice Herfindahl-Hirschman es una medida del tamaño ',
    'de las empresas en relación con la industria y un indicador de la ',
    'cantidad de competencia entre ellas. Se calcula como la suma de los ',
    'cuadrados del valor de las cuotas de mercado de un contratista dentro ',
    'del valor de la cuota de mercado de toda la entidad. Teniendo en ',
    'cuenta que el indicador se da en un rango de [0,10000], se estandariza ',
    'el indicador dividiendo el HHI entre 10000.'),
'ID_cant' = paste0('El índice de diversidad de Simpson (también conocido ',
    'como el índice de la diversidad de las especies o índice de dominancia)',
    ' es uno de los parámetros que permiten medir la biodiversidad de ',
    'organismos en un hábitat. Llevado al contexto de la contratación ',
    'permite medir la diversidad de contratistas en una entidad al tener en',
    ' cuenta la cantidad de los contratos que se asigna a cada uno de los ',
    'contratistas de una entidad.'),
'ID_val' = paste0('El índice de diversidad de Simpson (también conocido como ',
    'el índice de la diversidad de las especies o índice de dominancia) es ',
    'uno de los parámetros que permiten medir la biodiversidad de organismos ',
    'en un hábitat. Llevado al contexto de la contratación permite medir la ',
    'diversidad de contratistas en una entidad al tener en cuenta el valor de ',
    'los contratos que se asigna a cada uno ',
    'de los contratistas de una entidad.'),
'IC4K_cant' = paste0('Mide el número de procesos en número que la entidad ',
    'estatal adjudicó a los cuatro contratistas con más contratos.'),
'IC4K_val' = paste0('Mide el valor de los procesos que la entidad estatal ',
    'adjudicó a los cuatro contratistas con más contratos.'),
'perc_tiempo_adiciones' = paste0('Mide el porcentaje promedio de los contratos',
    ' de cada entidad que presenta adiciones en tiempo.'),
'perc_valor_adiciones' = paste0('Mide el porcentaje promedio del valor de las',
    ' adiciones en los contratos de cada entidad, y se calcula el porcentaje ',
    'de ese promedio de adiciones de la entidad con respecto al máximo',
    ' del promedio de adiciones de todas las entidades.'))

des_ind_name <- c(
  'ind_riesgo_corrupcion' = 'Índice de riesgo de corrupción',
  'oferentes_prom' = 'Oferentes promedio por proceso de selección',
  'ofertas_prom' = 'Participación promedio de ofertas',
  'ganadoras' = 
    'Índice de empresas ganadoras diferentes (por cada 100 contratos)',
  'perc_contr_cerrada_num' = 'Porcentaje de procesos no abiertos',
  'perc_contr_cerrada_val' = 'Porcentaje del valor de procesos no abiertos',
  'HHI_cant' = 'Índice de concentración IHH por número de contratos',
  'HHI_val' = 'Índice de concentración IHH por valor de contratos',
  'ID_cant' = 'Índice de Diversidad ID por número de contratos',
  'ID_val' = 'Índice de Diversidad ID por valor de contratos',
  'IC4K_cant' = 'Índice de concentración para las 4 mayores empresas',
  'IC4K_val' = 'Índice de concentración en valor para las 4 mayores empresas',
  'perc_tiempo_adiciones' = 
    'Índice del porcentaje de contratos con modificaciones en tiempo',
  'perc_valor_adiciones' = 'Índice del porcentaje del valor de las adiciones')


# 5. Definicion del UI (Frontend) ----
ui <- dashboardPage(
  
  ## 5.1. Encabezado del dash ----
  dashboardHeader(title = "Contratación Covid 19",
                  dropdownMenuOutput('dropdown_glos'),
                  dropdownMenuOutput('dropdown_ayuda')),
  
  ## 5.2. Menu de navegacion -----
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                # Descriptivo  
                menuItem(text = "Análisis Exploratorio", tabName = "EDA", 
                 icon = icon("chart-line"),
                 menuSubItem(text = "Resumen General", tabName = "general", 
                             icon = icon("clipboard-list")),
                 menuSubItem(text = "Contratación por Región", tabName = "mapa", 
                             icon = icon("map")),
                 menuSubItem(text = "Proceso y Tipo de Contrato", 
                             tabName = "procesos", icon = icon("sitemap")),
                 menuSubItem(text = "Bienes y Servicios", tabName = "objeto", 
                             icon = icon("cart-plus")),
                 menuSubItem(text = "Entidades y Contratistas", 
                             tabName = "actores", icon = icon("address-card"))),
                # Indicadores
                menuItem(text = "Indicadores de Riesgo", tabName = "IRC", 
                 icon = icon("flag"),
                 menuSubItem(text = "Riesgo por Región", tabName = "IRC1", 
                             icon = icon("chart-line")),
                 menuSubItem(text = "Orden de Entidad", tabName = "IRC2", 
                             icon = icon("map")),
                         menuSubItem(text = "Entidades", tabName = "IRC3", 
                             icon = icon("address-card"))),
                # Descarga de contratos
                menuItem(text = "Contratos", tabName = "contratos", 
                         icon = icon("database"))
    )
  ),
  
  ## 5.3. Cuerpo del dash ----
  dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "contc_style.css")
    ),
    
    # Tema del dashboard
    customTheme,
    
    tabItems(
      ### 5.3.1. Seccion general ----
      
      tabItem(
        tabName = "general",
        fluidRow(
          box(
            width = 3, status = "primary", solidHeader = F,
            height = 80, h1(strong(textOutput("n_contratos")), 
                            style = "margin-top: 0; margin-bottom: 0; 
                                      color:#0090B5"),
            h5("Número Total de Contratos", 
               style = "color:#0090B5; margin-top: 2px")
          ),
          box(
            width = 3, status = "success", solidHeader = F,
            height = 80, h1(strong(textOutput("valor_contratos")), 
                            style = "margin-top: 0; margin-bottom: 0;
                                      color:#55DD5E"),
            h5("Valor Total de Contratos", 
               style = "color:#55DD5E; margin-top: 2px")
          ),
          box(
            width = 3, status = "primary", solidHeader = F,
            height = 80, h1(strong(textOutput("n_entidades")), 
                            style = "margin-top: 0; margin-bottom: 0;
                                      color:#0090B5"),
            h5("Número de Entidades", 
               style = "color:#0090B5; margin-top: 2px")
          ),
          box(
            width = 3, status = "success", solidHeader = F,
            height = 80, h1(strong(textOutput("n_contratistas")), 
                            style = "margin-top: 0; margin-bottom: 0;
                                      color:#55DD5E"),
            h5("Número de Contratistas", 
               style = "color:#55DD5E; margin-top: 2px")
          )
        ),
        fluidRow(
          box(
            title = 'Menú de opciones', width = 12,
            solidHeader = T, collapsible = TRUE,
            column(width = 3, 
                   dateRangeInput(inputId = "sel_rango_fecha",
                                  label = "Rango Fecha:",
                                  start = min(contratos$fecha_firma),
                                  end = max(contratos$fecha_firma),
                                  language = "es")),
            column(width = 3,
                   selectInput(
                     inputId = "sel_escala_fecha",
                     label = h5(strong("Escala de tiempo:"),
                                style = "margin-top: 0; margin-bottom: 0"),
                     choices =  c("Diario" = "dia", 
                                  "Semana" = 'semana', 
                                  "Mes" = "mes"),
                     selected = "semana")),
            column(width = 3,
                   selectInput(
                     inputId = "sel_acum_fecha",
                     label = h5(strong("Tipo de vista:"),
                                style = "margin-top: 0; margin-bottom: 0"),
                     choices =  c("Normal" = 'normal',
                                  "Acumulado" = "acuml"),
                     selected = "Normal")),
            column(width = 3,
                   selectInput(
                     inputId = "selec_dpto_g",
                     label = h5(strong("Departamento:"),
                                style = "margin-top: 0; margin-bottom: 0"),
                     choices =  list_dep,
                     selected = "Nacional"))
          )
        ),
        fluidRow(
          box(width = 6, solidHeader = T,
              plotlyOutput("general_cantidad", 
                           width = "100%", height = 450)
          ),
          box(width = 6, solidHeader = T,
              plotlyOutput("general_valor", 
                           width = "100%", height = 450)
          )
        )
      ),
      
      ### 5.3.2. Seccion Mapa ----
      tabItem(
        tabName = "mapa",
        
        fluidRow(
          column(width = 8,
                 box(
                   status = NULL, solidHeader = T, width = NULL,
                   leafletOutput("mapa_plot",
                                 width = "100%", height = 680)
                 )),
          column(width = 4,
                 box(
                   title = "Menú de opciones",
                   status = NULL, solidHeader = T, width = NULL,
                   collapsible = TRUE, align = "center",
                   selectInput(inputId = "selec_mapa_c",
                               label = h5(strong("Criterio:"),
                                    style = "margin-top: 0; margin-bottom: 0"),
                               choices = c("Cantidad de contratos" = "cuantia",
                                           "Valor de contratos" = "Valor"),
                               selected = "Cantidad de contratos"),
                   selectInput(
                     inputId = "selec_mapa_d",
                     label = h5(strong("Departamento:"),
                                style = "margin-top: 0; margin-bottom: 0"),
                     choices =  list_dep,
                     selected = "Nacional"
                   )),
                 tabBox(
                   title = "Top", side = "right", width = NULL,
                   selected = textOutput("tab_mapa"),
                   tabPanel(title = textOutput("tab_mapa"), 
                            plotlyOutput("top_municipios", 
                                         width = "100%", height = 400)),
                   tabPanel(title = "Producto", 
                            plotlyOutput("top_productos", 
                                         width = "100%", height = 400))
                 )
          )       
        )
      ),
      
      ### 5.3.3. Seccion Proceso ----
      tabItem(
        tabName = "procesos",
        
        fluidRow(
          column(width = 8,
                 box(
                   status = NULL, solidHeader = T, width = NULL,
                   plotlyOutput("sankey_plot", 
                                width = "100%", height = 680)
                 )
          ),
          column(
            width = 4,
            box(
              title = 'Menú de opciones',
              status = NULL, solidHeader = T, width = NULL,
              collapsible = TRUE, align = "center",
              selectInput(
                inputId = "selec_proceso_c",
                label = h5(strong("Criterio:"),
                           style = "margin-top: 0; margin-bottom: 0"),
                choices = c("Cantidad de contratos" = "Cantidad",
                            "Valor de contratos" = "Valor"),
                selected = "Cantidad de contratos"),
              selectInput(
                inputId = "selec_proceso_d",
                label = h5(strong("Departamento:"),
                           style = "margin-top: 0; margin-bottom: 0"),
                choices =  list_dep,
                selected = "Nacional"
              ),
              selectInput(
                inputId = "selec_proceso_v",
                label = h5(strong("Variable del proceso:"),
                           style = "margin-top: 0; margin-bottom: 0"),
                choices = c("Estado del proceso" =
                              "estado_proceso",
                            "Régimen de Contratación" = 
                              "regimen_contratacion",
                            "Tipo de proceso" = 
                              "tipo_proceso", 
                            "Tipo de contrato" = 
                              "tipo_contrato"),
                selected = "Estado del proceso"
              )
            ),
            box(
              title = textOutput("estado_torta"), 
              width = NULL, solidHeader = T, align = "center",
              plotlyOutput("composicion_proceso", 
                           width = "100%", height = 330))
          )       
        )
      ),
      
      ### 5.3.4. Seccion Objeto a contratar ----
      tabItem(
        tabName = "objeto",
        
        fluidRow(
          column(width = 8,
                 box(width = NULL, solidHeader = T,
                     plotlyOutput("objeto_tree", width = "auto",
                                  height = 680)
                 )),
          column(
            width = 4,
            box(title = "Menú de opciones",
                status = NULL, solidHeader = T, width = NULL,
                collapsible = TRUE, align = "center",
                selectInput(
                  inputId = "selec_objeto",
                  label = h5(strong("Criterio:"),
                             style = "margin-top: 0; margin-bottom: 0"),
                  choices = c("Cantidad de contratos" = "Cantidad",
                              "Valor de contratos" = "Valor"),
                  selected = "Cantidad de contratos"
                ),
                selectInput(
                  inputId = "selec_objeto_d",
                  label = h5(strong("Departamento:"),
                             style = "margin-top: 0; margin-bottom: 0"),
                  choices =  list_dep,
                  selected = "Nacional"
                )
            ),
            box(title = "Grupo de productos",
                status = NULL, solidHeader = T, width = NULL,
                plotlyOutput("objeto_grupo_torta", 
                             width = "100%", height = 405)
            )
          ))
      ),
      
      ### 5.3.5. Seccion entidades y contratistas ----
      tabItem(
        tabName = "actores",
        
        fluidRow(
          box(width = 3, status = "primary", solidHeader = F,
              height = 120, h3(strong("Entidades"), 
                               style = "margin-top: 0; margin-bottom: 0"),
              h1(strong(textOutput("ind_entidades")), 
                 style = "margin-top: 0; margin-bottom: 0;
                          color:#00728F"),
              h5(textOutput("info_entidades"), 
                 style = "margin-top: 0; margin-bottom: 0;
                          color:#00728F")
          ),
          box(width = 3, status = "primary", solidHeader = F,
              height = 120, h3(strong("Contratistas"), 
                               style = "margin-top: 0; margin-bottom: 0"),
              h1(strong(textOutput("ind_contratistas")), 
                 style = "margin-top: 0; margin-bottom: 0;
                               color:#00728F"),
              h5(textOutput("info_contratistas"), 
                 style = "margin-top: 0; margin-bottom: 0;
                               color:#00728F")
          ),
          box(title = 'Menú de opciones',
              status = NULL, solidHeader = T, width = 6, height = 120,
              column(width = 6,
                     selectInput(
                       inputId = "selec_ent_cont",
                       label = h5(strong("Departamento:"),
                                  style = "margin-top: 0; margin-bottom: 0"),
                       choices =  list_dep,
                       selected = "Nacional")  
              ),
              column(width = 6,
                     selectInput(
                       inputId = "selec_crt_ec",
                       label = h5(strong("Criterio:"), style = "margin-top: 0; 
                                                margin-bottom: 0"),
                       choices = c("Cantidad" = "Cantidad",
                                   "Valor" = "Valor",
                                   "% Adición" = "ProporcionAdiciones",
                                   "% Cont. Directa" = "ContratacionDirecta"), 
                       selected = "Cantidad")
              )
          )
        ),
        fluidRow(
          column(width = 6,
                 tabBox(
                   title = textOutput("estado_entidad"), side = "right",
                   width = NULL, selected = "Top",
                   tabPanel(title = "Top",
                            plotlyOutput("entidades_plot", width = "100%",
                                         height = 520)),
                   tabPanel(title = "Detalle",
                            dataTableOutput("entidades_table", 
                                            width = "100%"))
                 )
          ),
          column(width = 6,
                 tabBox(
                   title = textOutput("estado_contratista"), 
                   side = "right",
                   width = NULL, selected = "Top",
                   tabPanel(title = "Top",
                            plotlyOutput("contratistas_plot", 
                                         width = "100%",
                                         height = 520)),
                   tabPanel(title = "Detalle",
                            dataTableOutput("contratistas_table", 
                                            width = "100%"))
                 )
          )
        )
        
      ),
      ### 5.3.6. Seccion IRC por region ----
      tabItem(
        tabName = "IRC1",
        
        fluidRow(
          column(width = 8,
              box(status = NULL, solidHeader = T, width = NULL, height = 80,
        column(width = 8,
           selectInput(inputId = "selec_ind_IRC1",
            label = h4("Indicador:", style = "margin-top: 0; margin-bottom: 0"),
           choices =  list_ind,
           selected = "ind_riesgo_corrupcion")),
         column(width = 4, 
                selectInput(inputId = "selec_reg_IRC1",
                 label = h4("Departamento:",
                   style = "margin-top: 0; margin-bottom: 0"),
                 choices =  list_dep,
                 selected = "Nacional"))),
         box(status = NULL, solidHeader = T, width = NULL,
           leafletOutput("mapa_plot_ind", width = "100%", height = 600))
        ),
        column(width = 4,
         box(title = textOutput('descripcion_IRC1_name'), status = NULL, 
          solidHeader = T, width = NULL, collapsible = T,
          tags$div(
            `style` = 'margin-left:12px; margin-right:12px; margin-top:0px;
                       text-align:justify',
            tags$p(textOutput('descripcion_IRC1'))
          )),
         box(status = NULL, solidHeader = T, width = NULL,
          plotlyOutput("top_reg_ind", width = "100%", height = 400))
          )
        )
      ),
      
      ### 5.3.7. Seccion IRC en general ----
      tabItem(
        tabName = "IRC2", 
        fluidRow(column(width = 7,
                        box(status = NULL, solidHeader = T, 
                            width = NULL, align = "center",
                            h4(strong("Riesgo por orden de entidad"),
                               style = "margin-top: 4px; margin-bottom: 5px"),
                            formattableOutput("tb_orden", width = '100%',
                                              height = '650px'))),
                 column(width = 5,
                        fluidRow(
                          box(status = NULL, solidHeader = T, 
                              width = NULL,
                              selectInput(
                                inputId = "selec_ind_IRC2",
                                label = h4("Indicador:",
                                    style = "margin-top: 0; margin-bottom: 0"),
                                choices =  list_ind,
                                selected = "ind_riesgo_corrupcion"),
                              tags$div(
                                `style` = 'margin-left:12px; margin-right:12px; 
                                    margin-top:0px;text-align:justify',
                                tags$p(textOutput('descripcion_IRC2'))
                              ))),
                        fluidRow(
                          box(status = NULL, solidHeader = T,
                              width = NULL, align = "center",
                              h4(strong("Riesgo por grupo de producto"),
                                 style = "margin-top: 4px; margin-bottom: 5px"),
                              girafeOutput("burbuja_plot", width = "100%",
                                           height = 450)
                          ))))),
      
      ### 5.3.8. Seccion IRC actores ----
      tabItem(
        tabName = "IRC3",
        fluidRow(
        column(width = 8,
                 box(status = NULL, solidHeader = T, width = NULL, height = 80,
                     column(width = 8,
                            selectInput(inputId = "selec_ind_IRC3",
                                        label = h4("Indicador:",
                                    style = "margin-top: 0; margin-bottom: 0"),
                                        choices =  list_ind,
                                        selected = "ind_riesgo_corrupcion")),
                     column(width = 4, 
                            selectInput(inputId = "selec_reg_IRC3",
                                        label = h4("Departamento:",
                                    style = "margin-top: 0; margin-bottom: 0"),
                                        choices =  list_dep,
                                        selected = "Nacional")))
               ,
                 box(status = NULL, solidHeader = T, 
                     width = NULL,
                     formattableOutput("tb_entidad", width = '100%'))
               ),
        column(width = 4,
                 box(
                   title = textOutput('descripcion_IRC3_name'), status = NULL, 
                   solidHeader = T, width = NULL, collapsible = T, 
                   collapsed = T,
                   tags$div(
                `style` = 'margin-left:12px; margin-right:12px; margin-top:0px;
                      text-align:justify',
                     tags$p(textOutput('descripcion_IRC3'))
                   )
                 ),
                 box(
                   status = NULL, 
                   solidHeader = T, width = NULL, height = 100,
                   align = "center",
                   column(width = 8,
                   h1(strong(textOutput("ind_IRC_ent")), 
                      style = "margin-top: 5px; margin-bottom: 5px;
                          color:#212529"),
                   h5(textOutput("info_ind_IRC_ent"), 
                      style = "margin-top: 0; margin-bottom: 0;
                          color:#343A40")
                 ),
                 column(width = 4,
                    h1(strong(textOutput("ind_IRC_ent_2")), 
                        style = "margin-top: 5px; margin-bottom: 5px;
                        color:#212529"),
                    h5('Puesto del municipio', 
                        style = "margin-top: 0; margin-bottom: 0;
                        color:#343A40")
                 )),
                 box(
                   width = NULL, solidHeader = T, align = "center",
                   plotlyOutput("composicion_IRC", 
                                width = "100%", height = 380)
                 )
               ))),
      
      ### 5.3.9. Descargar ----
      tabItem(
        tabName = "contratos",
        DTOutput(outputId = "datos_contratos", width = "100%", 
                 height = "auto"),
        downloadButton(outputId = "downloadCsv", 
                       label = "Descargar Archivo")
      )
    )
  )
)

# 6. Creacion del server (Backend) ----
server <- function(input, output, session) {
  ## 6.1. Seccion general ----  
  # Contador de contratos
  # Cantidad
  output$n_contratos <- renderText({
    formatC(dim(contratos)[1], format = "fg", 
            big.mark = ".", decimal.mark = ",")
  })
  
  # Valor
  output$valor_contratos <- renderText({
    paste("$", formatC(
      sum(contratos$valor_total, na.rm = TRUE) / 1e12, 
      format = "f", big.mark = ".", decimal.mark = ",", digits = 2), " B.")
  })
  
  # numero de entidades
  output$n_entidades <- renderText({
    formatC(
      nrow(contratos %>% filter(valor_inicial > 0) %>% 
             select(nombre_entidad) %>% unique()), 
      format = "fg", big.mark = ".", decimal.mark = ",")
  })
  
  # numero de contratistas
  output$n_contratistas <- renderText({
    formatC(
      nrow(contratos %>% filter(valor_inicial > 0) %>% 
             select(nom_contratista) %>% unique()), 
      format = "fg", big.mark = ".", decimal.mark = ",")
  })
  
  # Graficas de series de tiempo
  # Cantidad diaria
  output$general_cantidad <- renderPlotly({
    grafica_serie_tiempo(data = contratos, cantidad = T, 
                         acumulado = input$sel_acum_fecha == "acuml", 
                         dpto = input$selec_dpto_g, 
                         escala = input$sel_escala_fecha,
                         intervalo_fecha = c(format(input$sel_rango_fecha[1]),
                                             format(input$sel_rango_fecha[2])))
  })
  # Cantidad acumulado
  output$general_valor <- renderPlotly({
    grafica_serie_tiempo(data = contratos, cantidad = F, 
                         acumulado = input$sel_acum_fecha == "acuml", 
                         dpto = input$selec_dpto_g, 
                         escala = input$sel_escala_fecha,
                         intervalo_fecha = c(format(input$sel_rango_fecha[1]),
                                             format(input$sel_rango_fecha[2])))
  })
  
  # Ultima actualizacion
  output$ultima_act <- renderText({
    fecha <- max(contratos$fecha_firma)
    paste(day(fecha), month(fecha), year(fecha), sep = "/")
  })
  
  ## 6.2. Seccion mapa ----
  # Mapa
  output$mapa_plot <- renderLeaflet({
    
    grafica_mapa(data = contratos, dpto = input$selec_mapa_d, 
                 operacion = input$selec_mapa_c)
    
  })
  
  # Top municipio
  output$top_municipios <- renderPlotly({
    
    grafica_top_dto_mun(data = contratos,
                        dpto = input$selec_mapa_d, 
                        operacion = input$selec_mapa_c)
  })
  
  # Top producto
  output$top_productos <- renderPlotly({
    
    grafica_top_fam(data = contratos,
                    dpto = input$selec_mapa_d, 
                    operacion = input$selec_mapa_c)
  })
  
  # Etiqueta tab
  output$tab_mapa <- renderText({
    if (input$selec_mapa_d == "Nacional") {
      valor <- "Departamento"
    } else {
      valor <- "Municipio"
    }
    valor
  })
  
  ## 6.3. Seccion procesos ----
  # Sankey plot
  output$sankey_plot <- renderPlotly({
    
    sankey_proceso(data = contratos, input$selec_proceso_c, 
                   dpto = input$selec_proceso_d)
  })
  
  # grafico de torta composicion
  output$composicion_proceso <- renderPlotly({
    
    donut_proceso(data = contratos, input = input$selec_proceso_c, 
                  variable = input$selec_proceso_v,
                  dpto = input$selec_proceso_d)
  })
  
  # texto titulo
  output$estado_torta <- renderText({
    if (input$selec_proceso_v == "estado_proceso") {
      estado <- "Estado de proceso"
      estado
    } else if (input$selec_proceso_v == "regimen_contratacion") {
      estado <- "Régimen de contratación"
      estado
    }else if (input$selec_proceso_v == "tipo_proceso") {
      estado <- "Tipo de proceso"
      estado
    }else {
      estado <- "Tipo de contrato"
      estado
    }
  })
  ## 6.4. Sección objeto ----
  # Gráfica tree map
  output$objeto_tree <- renderPlotly({
    treemap_familia(data = contratos, tipo = input$selec_objeto, 
                    dpto = input$selec_objeto_d)
  })
  
  # Gráfica torta grupo de objeto
  output$objeto_grupo_torta <- renderPlotly({
    donut_grupo(data = contratos, tipo = input$selec_objeto,
                dpto = input$selec_objeto_d)
  })
  
  ## 6.5. Sección entidad y contratistas ----
  # Valor del indicador entidades
  
  output$ind_entidades <- renderText({
    valor_ind_ent(selec = input$selec_ent_cont, 
                  radio = input$selec_crt_ec)
  })
  # Informacion entidades
  output$info_entidades <- renderText({
    if (input$selec_crt_ec == "Cantidad") {
      valor <- paste0("Número de entidades en ",
                      input$selec_ent_cont)
    } else if (input$selec_crt_ec == "Valor") {
      valor <- paste0("Valor de contratos promedio por entidad",
                      " en ", input$selec_ent_cont)
    } else if (input$selec_crt_ec == "ProporcionAdiciones") {
      valor <- paste0("Promedio del porcentaje de adición",
                      " en ", input$selec_ent_cont)
    } else {
      valor <- paste0("Prom. del porcentaje de contratación ",
                      "directa en ", input$selec_ent_cont)
    }
    
    valor
  })
  # Valor del indicador contratistas
  output$ind_contratistas <- renderText({
    valor_ind_cont(selec = input$selec_ent_cont, 
                   radio = input$selec_crt_ec)
  })
  # Informacion contratistas
  output$info_contratistas <- renderText({
    if (input$selec_crt_ec == "Cantidad") {
      valor <- paste0("Número de contratistas en ",
                      input$selec_ent_cont)
    } else if (input$selec_crt_ec == "Valor") {
      valor <- paste0("Valor de contratos promedio por contratista",
                      " en ", input$selec_ent_cont)
    } else if (input$selec_crt_ec == "ProporcionAdiciones") {
      valor <- paste0("Promedio del porcentaje de adición",
                      " en ", input$selec_ent_cont)
    } else {
      valor <- paste0("Prom. del porcentaje de contratación ",
                      "directa en ", input$selec_ent_cont)
    }
    
    valor
  })
  
  # Titulo del estado del cuadro entidades
  output$estado_entidad <- renderText({
    if (input$selec_crt_ec == "Cantidad") {
      valor <- "Cantidad de contratos por entidad"
    } else if (input$selec_crt_ec == "Valor") {
      valor <- "Valor de contratos  por entidad"
    } else if (input$selec_crt_ec == "ProporcionAdiciones") {
      valor <- "Porcentaje de adición por entidad"
    } else {
      valor <- "Contratación Directa por entidad"
    }
    
    valor
  })
  
  # Titulo del estado del cuadro contratistas
  output$estado_contratista <- renderText({
    if (input$selec_crt_ec == "Cantidad") {
      valor <- "Cantidad de contratos por contratista"
    } else if (input$selec_crt_ec == "Valor") {
      valor <- "Valor de contratos por contratista"
    } else if (input$selec_crt_ec == "ProporcionAdiciones") {
      valor <- "Porcentaje de adición por contratista"
    } else {
      valor <- "Contratación Directa por contratista"
    }
    
    valor
  })
  
  # Grafica de entidades
  output$entidades_plot <- renderPlotly({
    entidades_ranking(data = contratos,
                      dpto = input$selec_ent_cont, 
                      tipo = input$selec_crt_ec)
  })
  
  # Tabla de detalle entidades
  output$entidades_table <- renderDataTable({
    entidades_ranking(data = contratos,
                      dpto = input$selec_ent_cont, 
                      tipo = input$selec_crt_ec, top = F)
  })
  
  # Grafica de entidades
  output$contratistas_plot <- renderPlotly({
    contratistas_ranking(data = contratos,
                         dpto = input$selec_ent_cont, 
                         tipo = input$selec_crt_ec)
  })
  
  # Tabla de detalle contratistas
  output$contratistas_table <- renderDataTable({
    contratistas_ranking(data = contratos,
                         dpto = input$selec_ent_cont, 
                         tipo = input$selec_crt_ec, top = F)
  })
  ## 6.6. Sección IRC1 ----
  # Salida del mapa de riesgo
  output$mapa_plot_ind <- renderLeaflet({
    
    if (input$selec_reg_IRC1 == 'Nacional') {
      grafica_mapa_ind(data = indicadoresD,
                       lugar = input$selec_reg_IRC1,
                       indicador = input$selec_ind_IRC1)
    } else {
      grafica_mapa_ind(data = indicadoresM,
                       lugar = input$selec_reg_IRC1,
                       indicador = input$selec_ind_IRC1)
    }
    
  })
  
  # Top municipio
  output$top_reg_ind <- renderPlotly({
    
    if (input$selec_reg_IRC1 == 'Nacional') {
      grafica_top_Dto_Mun_ind(data = indicadoresD,
                       lugar = input$selec_reg_IRC1,
                       indicador = input$selec_ind_IRC1)
    } else {
      grafica_top_Dto_Mun_ind(data = indicadoresM,
                       lugar = input$selec_reg_IRC1,
                       indicador = input$selec_ind_IRC1)
    }
    
  })
  
  output$descripcion_IRC1 <- renderText({
    des_ind[which(names(des_ind) == input$selec_ind_IRC1)][[1]]
  })
  
  output$descripcion_IRC1_name <- renderText({
    des_ind_name[which(names(des_ind) == input$selec_ind_IRC1)][[1]]
  })
  ## 6.7. Sección IRC2 ----
  # Tabla de indicadores por orden de entidad
  output$tb_orden <- renderFormattable({
    OrdenEntidad_Indicador(indicadoresE)
  })
  
  # Grafico de burbuja
  output$burbuja_plot <- renderGirafe({
    burbuja_funcion(indicadoresF, input$selec_ind_IRC2)
  })
  
  output$descripcion_IRC2 <- renderText({
    des_ind[which(names(des_ind) == input$selec_ind_IRC2)][[1]]
  })
  ## 6.8. Sección IRC3 ----
  output$tb_entidad <- renderFormattable({
    Entidades_Indicador(indicadoresE,
                        input1 = input$selec_reg_IRC3, 
                        input2 = input$selec_ind_IRC3)
  })
  
  output$descripcion_IRC3 <- renderText({
    des_ind[which(names(des_ind) == input$selec_ind_IRC3)][[1]]
  })
  
  output$descripcion_IRC3_name <- renderText({
    des_ind_name[which(names(des_ind) == input$selec_ind_IRC3)][[1]]
  })
  
  output$ind_IRC_ent <- renderText({
    mean_dpto_ind(indicadoresE,
                  dpto = input$selec_reg_IRC3, 
                  ind = input$selec_ind_IRC3)
  })
  
  output$info_ind_IRC_ent <- renderText({
    paste0('Promedio del indicador en ', input$selec_reg_IRC3)
  })
  
  output$ind_IRC_ent_2 <- renderText({
    ranking_dpto_ind(indicadoresE,
                  dpto = input$selec_reg_IRC3, 
                  ind = input$selec_ind_IRC3)
  })
  
  output$composicion_IRC <- renderPlotly({
    donut_ind_entidad(indicadoresE,
                      dpto = input$selec_reg_IRC3, 
                      ind = input$selec_ind_IRC3)
  })
  
  ## 6.9. Sección descarga ----
  # Consulta
  contratos_table_out <- contratos %>% 
    select(nombre_entidad, tipo_contrato, tipo_proceso, 
           objeto_contratar, valor_inicial, valor_adiciones,
           valor_total, nom_contratista, fuente) %>%
    arrange(nombre_entidad) %>% 
    rename(`Nombre entidad` = nombre_entidad,
           `Tipo de contrato` = tipo_contrato,
           `Tipo de proceso` = tipo_proceso,
           `Objeto a contratar` = objeto_contratar, 
           `Valor inicial` = valor_inicial, 
           `Valor adiciones` = valor_adiciones,
           `Valor total` = valor_total, 
           `Nombre del contratista` = nom_contratista,
           Fuente = fuente)
  
  # Creacion de tabla
  output$datos_contratos <- renderDT(
    contratos_table_out, options = list(lengthChange = FALSE))
  
  # Creacion de botón de descarga
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(contratos_table_out, file)
    })
  
  ## 6.10. Muestra de la introduccion ----
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "Iniciar",
                     icon = icon("play-circle"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  ## 6.11. Notificaciones ----
  # Glosario
  output$dropdown_glos <- renderMenu({
    obtener_noti_glos(input$tabs)
  })
  # Ayuda
  output$dropdown_ayuda <- renderMenu({
    obtener_noti_ayud(input$tabs)
  })
}
# 7. Aplicacion ----
shinyApp(ui = ui, server = server)
