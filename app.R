# Diseño de dashboard en contratacion publica durante pandemia
# covid 19 en Colombia - DashCCo
# Fecha elaboracion: 15/07/2020

# 1. Librerias ----
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(colorspace)
library(leaflet)

# 2. Carga de conjunto de datos ----
  contratos <- read_csv(file = "contratos_covid19_LV1.csv", 
                      locale = locale(encoding = "UTF-8"))
  

# 3. Funciones ----
## * Grafica resumen
  grafica_serie_tiempo <- function(cantidad = T, acumulado = F){
    # Consulta
    stat_contr <- contratos %>%
      group_by(fecha_firma) %>% 
      summarise(n_contratos = n(), 
                cuantia_dia = sum(valor_total, na.rm = TRUE)/1e9) %>% 
      mutate(n_contratos_acum = cumsum(n_contratos), 
             cuantia_acumulada = cumsum(cuantia_dia))
    
    # ajuste de contratos
    if (cantidad & acumulado) {
      label <- "Día: %{x} <br>Cantidad: %{y:5.0f}<extra></extra>"
      fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                     y = ~n_contratos_acum, type = 'scatter', 
                     mode = 'lines', fill = 'tozeroy',
                     hovertemplate = label,
                     fillcolor = 'rgba(0,144,181,0.5)',
                     line = list(width = 0.5,
                                 color = 'rgba(0,144,181,1)'))
      y_titulo <- "Cantidad"
      suffix <- ""
      
    } else if(!cantidad & acumulado){
      label <- "Día: %{x} <br>Valor: %{y:$4.1f} MM<extra></extra>"
      fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                     y = ~cuantia_acumulada, type = 'scatter', 
                     mode = 'lines', fill = 'tozeroy',
                     hovertemplate = label,
                     fillcolor = 'rgba(122,229,130,0.5)',
                     line = list(width = 0.5,
                                 color = 'rgba(122,229,130,1)'))
      
      y_titulo <- "Valor"
      suffix <- " MM"
      
    } else if(cantidad & !acumulado){
      
      label <- "Día: %{x} <br>Cantidad: %{y:3.0f}<extra></extra>"
      
      fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                     y = ~n_contratos, type = 'scatter', 
                     mode = 'lines',
                     hovertemplate = label,
                     line = list(color = 'rgba(0,144,181,1)'))
      
      y_titulo <- "Cantidad"
      suffix <- ""
      } else{
      
      label <- "Día: %{x} <br>Valor: %{y:$6.1f} MM<extra></extra>"
      
      fig <- plot_ly(data = stat_contr, x = ~fecha_firma, 
                     y = ~cuantia_dia, type = 'scatter', 
                     mode = 'lines',
                     hovertemplate = label,
                     line = list(color = 'rgba(122,229,130,1)'))
      
      y_titulo <- "Valor"
      suffix <- " MM"
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
    
    # Agregacion de detalles
    fig <- fig %>%
      layout(xaxis = list(
        title = list(text = "Fecha de firma del contrato",
                     font = f1),
        range = c("2020-03-10", as.character(today())),
        type = "date",
        tickfont = f2,
        rangebreaks = list( 
          list(bounds=c("sat", "mon")))),
        
        yaxis = list(
          title = list(text = y_titulo,
                       font = f1),
          tickfont = f2,
          ticksuffix = suffix),
        title = "",
        margin = list(
          l = 38, r = 0, t = 0, b = 30),
        plot_bgcolor='rgb(255,255,255)',
        paper_bgcolor='rgb(255,255,255)')
    fig
  }

  # Sankey funcion
  sankey_diagram <- function(input = "Cantidad"){
    
    # Valores del parametro input
    # input = {Cantidad, Valor}
    
    #Ajuste colores nodos
    if (input == "Cantidad") {
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
    
    # Ajuste de nodos (node)
    nodes <- rbind(
      {x <- contratos %>%
        group_by(regimen_contratacion) %>% 
        summarise(n = n(),
                  valor = sum(valor_total)/1e9) %>%
        select(regimen_contratacion, n , valor) %>% 
        rename(name = regimen_contratacion) %>% 
        mutate(nodo = "Régimen de contratación") %>% 
        arrange(desc(n));
      x$color <-  pal_l1(nrow(x)); x}, # regimen
      
      {x <- contratos %>% 
        group_by(tipo_proceso) %>% 
        summarise(n = n(),
                  valor = sum(valor_total)/1e9) %>%
        select(tipo_proceso, n, valor) %>% 
        rename(name = tipo_proceso) %>% 
        filter(name != "Régimen Especial") %>% 
        mutate(nodo = "Tipo de proceso") %>% 
        arrange(desc(n));
      x$color = pal_l2(nrow(x)); x}, # tipo de proceso
      
      {x <- contratos %>%
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
    links <- rbind(contratos %>% 
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
                   
       contratos %>% 
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
    if(input == "Cantidad"){
      links <- links %>% 
        rename(value = n)
      
      links <- links %>% 
        mutate(label = 
          paste(label, "<br><i>Contratos: </i>",
           sprintf("%4g", value),
                   "</br><extra></extra>", sep = ""))
    } else {
      links <- links %>% 
        rename(value = valor)
      
      links <- links %>% 
        mutate(label = 
          paste(label, "<br><i>Valor contratos: </i>",
           sprintf("$%.3f", value),
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
          input == "Cantidad", 
          paste("<b>%{label}</b>",
                "<br><i>Contratos: </i>",
                "%{value:.0f}</br>",
                "<extra></extra>", 
                sep = ""), 
          paste("<b>%{label}</b>",
                "<br><i>Valor contratos: </i>",
                "%{value:$.3f} MM.</br>",
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
        title = list(text =""),
        font = list(family = "Arial, sans-serif",
                    size = 11,
                    color = "black"),
        margin = list(
          l = 0,
          r = 0,
          t = 0,
          b = 0)) %>%
      
      add_annotations(
        x= c(0, 0.5075, 1),
        y= c(0, 0, 0),
        text = c("Régimen<br>contratación", 
                 "Tipo de<br>proceso", 
                 "Tipo de<br>contrato"),
        showarrow = F,
        font = list(size = 13))
    
    # Salida del gráfico
    fig
  }  
  
  # Donut function 
  diagrama_dona <- function(input = "Cantidad", 
                            variable = "estado_proceso"){
    
    # Consulta
    df <- contratos %>%
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
             grupo = case_when(porcion <= 0.05 ~ "Otros",
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
               l = 50, r = 50, t = 30, b = 20))
    
    # salida del grafico
    fig
  }
  
  # funcion de mapa
  grafica_mapa <- function(input, operacion,
                          plot.nivel.general = FALSE){
    # 1. Recibir retorno de los 2 botones--------------------------
    
    # a. Botón 1: Nacional o departamentos----
    
    if (input != "Nacional")
    {df_mapa <- contratos %>% 
      filter(departamento_entidad == input) %>%
      mutate(nombre_lugar = paste(municipio_entidad, 
                                  departamento_entidad,
                                  sep = "-")) %>%
      group_by(nombre_lugar, codigo_municipio_ent) %>% 
      summarise(cuantia = n(),
                valor = sum(valor_total, na.rm = TRUE),
                lat = mean(latitud, na.rm = TRUE), 
                lng = mean(longitud, na.rm = TRUE)) 
    
    df_mapa <- df_mapa %>% 
      mutate(porcentaje_cuantia = (cuantia/sum(df_mapa$cuantia))*100, 
             porcentaje_valor = (valor/sum(df_mapa$valor))*100)  %>%
      arrange(codigo_municipio_ent)
    } else
    {df_mapa <- contratos %>%
      mutate(nombre_lugar = departamento_entidad) %>%
      group_by(nombre_lugar, codigo_departamento_ent) %>% 
      summarise(cuantia = n(),
                valor = sum(valor_total, na.rm = TRUE),
                lat = mean(latitud, na.rm = TRUE), 
                lng = mean(longitud, na.rm = TRUE)) 
    
    df_mapa <- df_mapa %>% 
      mutate(porcentaje_cuantia = (cuantia/sum(df_mapa$cuantia))*100, 
             porcentaje_valor = (valor/sum(df_mapa$valor))*100)  %>%
      arrange(codigo_departamento_ent)
    }
    
    # b. Botón 2: Cuantia o Valor----
    
    if(operacion == "cuantia"){
      df_mapa <- df_mapa %>% 
        ungroup()%>% 
        select(nombre_lugar, cuantia, 
               porcentaje_cuantia, lat, lng) %>% 
        rename(x = cuantia,
               p = porcentaje_cuantia) %>%
        arrange(desc(x))
      
    }else{
      df_mapa <-  df_mapa %>% 
        ungroup()%>%
        select(nombre_lugar, valor, porcentaje_valor, lat, lng) %>% 
        rename(x = valor,
               p = porcentaje_valor) %>%
        arrange(desc(x))
    }
    # Establecer color
    #AZULES: 
    #lightseagreen
    #darkturquoise
    #VERDES: 
    #lawngreen = Chillón
    #lightgreen = claro
    #mediumseagreen = Oscuro
    #mediumspringgreen
    
    color = "o"
    
    if(operacion == "cuantia")
    {color = "darkturquoise"#azul
    }else {
      color ="limegreen"}#verde
    
    
    # 2. Escala de los circulos en el mapa----------------------------
    if (operacion == "cuantia") 
    { #Variables
      num_intervalos <- 4
      vmax <- max(df_mapa$x)
      #Ecuación para hallar  base
      base = vmax^(1/num_intervalos)
      df_mapa <-  df_mapa %>%
        mutate(tam_escalado = case_when(x  < base ~ 2,
                                        x  >= base & x < base^2 ~ 4,
                                        x  >= base^2 & x < base^3 ~ 8,
                                        x  <= base^4+1 ~ 16))
    } else 
    {num_intervalos <- 5
    vmax <- quantile(df_mapa$x,probs=0.9,names = FALSE)
    #Ecuación para hallar  base
    base = vmax^(1/num_intervalos)
    df_mapa <-  df_mapa %>%
      mutate(tam_escalado = case_when(x  < base^2 ~ 1,
                                      x  >= base^2 & x < base^3 ~ 2,
                                      x  >= base^3 & x < base^4 ~ 4,
                                      x  >= base^4 & x < (base^5) ~ 8,
                                      TRUE ~ 16))
    }
    
    # 3. Etiqueta------------------------------------------------
    if(operacion == "cuantia") 
    {
      etiqueta <- paste(
        sprintf("<dfn>Lugar: </dfn><strong>%s</strong><br/> 
                <dfn>Total contratos: </dfn> %g<br/>",
                df_mapa$nombre_lugar, df_mapa$x),
        sprintf("<dfn>Porcentaje: </dfn> %3.2f",df_mapa$p),
        "%") %>% 
        lapply(htmltools::HTML)
    } else 
    {
      etiqueta <- paste(
        sprintf("<dfn>Lugar: </dfn><strong>%s</strong><br/> 
                <dfn>Total valor contratos ($): </dfn> %6.2f MM<br/>",
                df_mapa$nombre_lugar, (df_mapa$x)/1e9),
        sprintf("<dfn>Porcentaje: </dfn> %3.2f",df_mapa$p),
        "%") %>% 
        lapply(htmltools::HTML)
    }
    
    # 4. Construcción de la gráfica--------------------
    leaflet(data = df_mapa) %>% 
      addTiles() %>%
      addProviderTiles("Wikimedia") %>%
      addCircleMarkers(radius = ~ tam_escalado, #cuantia/valor
                       color = color,
                       stroke = FALSE, fillOpacity = 0.6,
                       label = ~ etiqueta,
                       labelOptions = labelOptions(
                         style = list(
                           "color" = "black",
                           "font-family" = "Times",  #Tipo fuente 
                           #"font-style" = "italic", #Estilo
                           "font-size" = "12px",
                           "border-color" = "rgba(0,88,59,0.5)")))
  }
   
  # funcion top departamento
  grafica_top_Dto_Mun <- function(input, operacion,
                                  plot.nivel.general = FALSE){
    # 1. Recibir retorno de los botones-----------------------------
    # a. Botón 1: Nacional o departamentos----
    if (input != "Nacional")
    {top_Dto_Mun <- contratos %>% 
      filter(departamento_entidad == input) %>%
      mutate(nombre_lugar = paste(municipio_entidad,
                                  departamento_entidad,
                                  sep = "-")) %>%
      group_by(nombre_lugar) %>%
      summarise(cuantia = n(),
                valor = sum(valor_total,na.rm = TRUE)/1e9) 
    } else{
      top_Dto_Mun <- contratos %>%
        mutate(nombre_lugar = departamento_entidad) %>%
        group_by(nombre_lugar) %>%
        summarise(cuantia = n(),
                  valor = sum(valor_total,na.rm = TRUE)/1e9)
    }
    
    # b. Botón 2: Cuantia o Valor----
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
    
    # 2.1 Parametros generales grafica ----
    f1 <- list(
      family = "Arial, sans-serif",
      size = 13,
      color = "darkgrey")
    
    f2 <- list(
      family = "Arial, sans-serif",
      size = 11,
      color = "black")
    
    # 2.2. Construcción de la gráfica-----------------------------
    
    top_Dto_Mun <- top_Dto_Mun %>% 
      mutate(nombre_lugar = as.character(nombre_lugar)) %>%
      mutate(etiqueta = case_when(str_length(nombre_lugar) >= 10 ~ 
                        paste0(str_sub(nombre_lugar, 1, 10), "..."),
                                  TRUE ~ nombre_lugar))
    
    plot_ly(data = top_Dto_Mun,
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
                                       font = f1), tickfont = f2))
    
  }

  # funcion top objeto
  grafica_top_fam <- function(input, operacion,
                              plot.nivel.general = FALSE){
    # 1. Recibir retorno de los botones-----------------------------
    
    # a. Botón 1: Nacional o departamentos----
    if (input != "Nacional")
    {top_familia <- contratos %>% 
      filter(departamento_entidad == input) %>%
      #select(departamento_entidad,nombre_familia,valor_total) %>%
      group_by(nombre_familia) %>%
      summarise(cuantia = n(),
                valor = sum(valor_total,na.rm = TRUE)/1e9) 
    } else{
      top_familia <- contratos %>%
        group_by(nombre_familia) %>%
        summarise(cuantia = n(),
                  valor = sum(valor_total,na.rm = TRUE)/1e9)
    }
    
    # b. Botón 2: cuantia o Valor----
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
    
    # 2.1 Parametros generales grafica ----
    f1 <- list(
      family = "Arial, sans-serif",
      size = 13,
      color = "darkgrey")
    
    f2 <- list(
      family = "Arial, sans-serif",
      size = 11,
      color = "black")
    
    # 2.2 Construcción de la gráfica-----------------------------
    top_familia <- top_familia %>% 
      mutate(nombre_familia = as.character(nombre_familia)) %>%
      mutate(etiqueta = case_when(str_length(nombre_familia) >= 16 ~ 
                    paste0(str_sub(nombre_familia, 1, 16), "..."),
                                  TRUE ~ nombre_familia))
    plot_ly(data = top_familia,
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
                                       font = f1), tickfont = f2))
  }

  # Función tree map familia
  treefamilia <- function(input = "Cantidad"){
    Familia <- contratos %>% 
      group_by(nombre_grupo, nombre_familia) %>% 
      summarise(cuantia = sum(valor_total, na.rm = TRUE)/1e9, 
                total = n()) %>% arrange(nombre_grupo)
    
    x <- Familia %>% select(nombre_grupo) %>% unique()
    a <- contratos %>% 
      group_by(nombre_grupo)%>% 
      summarise(total = n())
    y <- contratos %>% 
      group_by(nombre_grupo) %>% 
      summarise(cuantia = sum(valor_total, na.rm = TRUE)/1e9)
    
    nwFamilia <- Familia %>% 
      mutate(ids = paste(nombre_grupo, nombre_familia, sep = " - "))
    
    #wis
    nwFamilia <- rbind(
      tibble(nombre_grupo = rep(x = "", 7),
             nombre_familia = x[[1]],
             ids = nombre_familia,
             total= a[[2]],
             cuantia= y[[2]]), tibble(nwFamilia))
    
    nwFamilia <- nwFamilia %>% 
      rename(parents = nombre_grupo, info = nombre_familia) %>% 
      mutate(label = paste0(str_sub(info, 1, 100), "")) %>% 
      arrange(parents)
    
    #Treemap por cuant?a de contratos
      if (input == "Valor"){
        
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
                       marker=list(colors = pal_verde(7)),
                       hovertemplate = paste(
                         "<b>%{label}</b>",
                         "<br><i>Valor de contratos: $</i>",
                         " %{value:.2f} MM </br>", 
                         "<i>Porcentaje: </i>",
                         "%{percentParent:.2%}</br>",
                         "<extra></extra>",
                         sep = ""),
                       domain = list(x = c(0,1),
                                     y = c(0,1)))%>% 
          layout(margin = list(l = 0, r = 0, t = 20, b = 0),
                 title = "")
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
                       marker=list(colors=pal_azul(7)),
                       hovertemplate = paste(
                         "<b>%{label}</b>",
                         "<br><i>Cantidad de contratos:</i>",
                         " %{value:.0f}</br>", 
                         "<i>Porcentaje: </i>",
                         "%{percentParent:.2%}</br>",
                         "<extra></extra>",
                         sep = ""),
                       domain = list(x = c(0,1),
                                     y = c(0,1)))%>% 
          layout(margin = list(l = 0, r = 0, t = 20, b = 0),
                 title = "")
      }
    
    fig
  } 
  
  # Función dona por grupo
  donagrupo <- function(input = "Cantidad"){
    
    Grupo <- contratos %>% 
      group_by(nombre_grupo) %>% 
      summarise(cuantia = sum(valor_total, na.rm = TRUE)/1e9, 
                total = n()) %>% arrange(nombre_grupo)
    
    
    #Por cantidad de contratos
    if(input == "Cantidad") { 
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
                 l = 50, r = 50, t = 30, b = 20))
      
      }
    
    #Por cuantia de contratos
      if(input == "Valor")
      {
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
                   l = 50, r = 50, t = 30, b = 20))
      }
    
    #Inprimir gr?fico
    fig1
  }
  
  # Funcion de grafica entidad
  entidades_function <- function(input1 = "Nacional", 
                                 input2 = "Cantidad",
                                 top = TRUE){
    
    # Generacion de indicadores
    df <- contratos %>% 
      filter(valor_inicial > 0, !is.na(valor_inicial)) %>% 
      mutate(porcentaje_adicion = valor_adiciones/valor_inicial,
             es_directo = (tipo_proceso == "Contratación Directa"))
    
    if (input1 == "Nacional") {
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
        filter(departamento_entidad == input1) %>% 
        group_by(nombre_entidad) %>% 
        summarise(n_contratos = n(),
           valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
           med_porcentaje_adc = mean(porcentaje_adicion, 
                                     na.rm = TRUE)*100,
           contratos_directos = sum(es_directo, na.rm = TRUE),
           poj_cont_directos = (contratos_directos/n_contratos)*100)
    }
    
    # Seleccion de criterio
    if (input2 == "Cantidad") {
      df <- df %>% select(nombre_entidad, n_contratos) %>% 
        rename(criterio = n_contratos)
    } else if (input2 == "Valor") {
      df <- df %>% select(nombre_entidad, valor_contratos)%>% 
        rename(criterio = valor_contratos)
    } else if (input2 == "ProporcionAdiciones") {
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
        mutate(etiqueta = case_when(str_length(nombre_entidad) >= 16~ 
                 paste0(str_sub(nombre_entidad, 1, 16), "..."),
                 TRUE ~ nombre_entidad))
      
      # Ajuste de parámetros de la grafica
      if (input2 == "Cantidad") {
        df <- df %>% 
          mutate(info = paste0("<b>", nombre_entidad, "</b><br>",
                               "Cantidad de contratos: ", criterio,
                               "<extra></extra>"))
        
        color <- "rgb(0, 163, 204)"
        suffix <- ""
        eje <- "Cantidad de contratos"
        
      } else if (input2 == "Valor") {
        df <- df %>% 
          mutate(info = paste0("<b>", nombre_entidad, "</b><br>",
                               "Valor de contratos: ", 
                               sprintf("$%.2f", criterio) ," MM",
                               "<extra></extra>"))
        
        color <- "rgb(153, 235, 158)"
        suffix <- " MM"
        eje <- "Valor de contratos"
        
      } else if (input2 == "ProporcionAdiciones") {
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
      
      # 2.1 Parametros generales grafica ----
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
                            tickfont = f2))
      
      return(plot_top)
      
    } else {
      
      # ajuste de detalles
      if (input2 == "Cantidad") {
        df <- df %>% rename(`Nombre entidad`  = nombre_entidad,
                            `Cantidad Contratos` = criterio)
      } else if (input2 == "Valor") {
        df <- df %>% 
          mutate(criterio = round(criterio, 4)*1000) %>% 
          rename(`Nombre entidad`  = nombre_entidad,
                 `Valor de Contratos Mill` = criterio)
      } else if (input2 == "ProporcionAdiciones") {
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
  contratistas_function <- function(input1 = "Nacional", 
                                    input2 = "Cantidad",
                                    top = TRUE){
    
    # Generacion de indicadores
    df <- contratos %>% 
      filter(valor_inicial > 0, !is.na(valor_inicial)) %>% 
      mutate(porcentaje_adicion = valor_adiciones/valor_inicial,
             es_directo = (tipo_proceso == "Contratación Directa"))
    
    if (input1 == "Nacional") {
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
        filter(departamento_contratista == input1) %>% 
        group_by(nom_contratista) %>% 
        summarise(n_contratos = n(),
          valor_contratos = sum(valor_total, na.rm = TRUE)/1e9,
          med_porcentaje_adc = mean(porcentaje_adicion, 
                                    na.rm = TRUE)*100,
          contratos_directos = sum(es_directo, na.rm = TRUE),
          poj_cont_directos = (contratos_directos/n_contratos)*100)
    }
    
    # Seleccion de criterio
    if (input2 == "Cantidad") {
      df <- df %>% select(nom_contratista, n_contratos) %>% 
        rename(criterio = n_contratos)
    } else if (input2 == "Valor") {
      df <- df %>% select(nom_contratista, valor_contratos)%>% 
        rename(criterio = valor_contratos)
    } else if (input2 == "ProporcionAdiciones") {
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
       mutate(etiqueta = case_when(str_length(nom_contratista) >= 16~ 
                paste0(str_sub(nom_contratista, 1, 16), "..."),
                TRUE ~ nom_contratista))
      
      # Ajuste de parámetros de la grafica
      if (input2 == "Cantidad") {
        df <- df %>% 
          mutate(info = paste0("<b>", nom_contratista, "</b><br>",
                               "Cantidad de contratos: ", criterio,
                               "<extra></extra>"))
        
        color <- "rgb(0, 144, 181)"
        suffix <- ""
        eje <- "Cantidad de contratos"
        
      } else if (input2 == "Valor") {
        df <- df %>% 
          mutate(info = paste0("<b>", nom_contratista, "</b><br>",
                               "Valor de contratos: ", 
                               sprintf("$%.2f", criterio) ," MM",
                               "<extra></extra>"))
        
        color <- "rgb(122, 229, 130)"
        suffix <- " MM"
        eje <- "Valor de contratos"
        
      } else if (input2 == "ProporcionAdiciones") {
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
      
      # 2.1 Parametros generales grafica ----
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
                            tickfont = f2))
      
      return(plot_top)
      
    } else{
      
      # ajuste de detalles
      if (input2 == "Cantidad") {
        df <- df %>% rename(`Nombre Contratista` = nom_contratista,
                            `Cantidad Contratos` = criterio)
      } else if (input2 == "Valor") {
        df <- df %>% 
          mutate(criterio = round(criterio, 4)* 100) %>% 
          rename(`Nombre Contratista` = nom_contratista,
                 `Valor de Contratos Mill` = criterio)
      } else if (input2 == "ProporcionAdiciones") {
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
  
## 4.2. Personalizacion del tema ----
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
  
## 4.3. Panel ayuda ----
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
  
## 4.4. Panel Notas ----
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
  
# 5. Definicion del UI (Frontend) ----
ui <- dashboardPage(
    
  ## 5.1. Encabezado del dash ----
  dashboardHeader(title = "Contratación Covid 19",
                  dropdownMenuOutput('dropdown_glos'),
                  dropdownMenuOutput('dropdown_ayuda')),

  ## 5.2. Menu de navegacion -----
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem(text = "Resumen General", tabName = "general", 
               icon = icon("chart-line")),
      menuItem(text = "Contratación por Región", tabName = "mapa", 
               icon = icon("map")),
      menuItem(text = "Proceso y Tipo de Contrato", 
               tabName = "procesos", icon = icon("sitemap")),
      menuItem(text = "Bienes y Servicios", tabName = "objeto", 
               icon = icon("cart-plus")),
      menuItem(text = "Entidades y Contratistas", 
               tabName = "actores", icon = icon("address-card")),
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
          
          # valueBoxOutput(outputId = "n_contratos", width = 6),
          # valueBoxOutput(outputId = "valor_contratos", width = 6)
          
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
          tabBox(
            title = "Cantidad de contratos", side = "right", 
            width = 6,
            selected = "Diario",
              tabPanel(title = "Diario", 
                     plotlyOutput("general_cantidad_nor", 
                                     width = "100%", height = 450)),
            tabPanel(title = "Acumulado", 
                     plotlyOutput("general_cantidad_acu", 
                                     width = "100%", height = 450))
                 
          ),
          tabBox(
            title = "Valor de Contratos", side = "right", 
            width = 6,
            selected = "Diario",
            tabPanel(title = "Diario", 
                     plotlyOutput("general_valor_nor", 
                                     width = "100%", height = 450)),
            tabPanel(title = "Acumulado", 
                     plotlyOutput("general_valor_acu", 
                                     width = "100%", height = 450))
            
          )
        )
      ),
    
      ### 5.3.2. Seccion Mapa ----
      tabItem(
      tabName = "mapa",
      
      fluidRow(
        column(width = 8,
          box(status = NULL, solidHeader = T, width = NULL,
              leafletOutput("mapa_plot",
                            width = "100%", height = 655)
        )),
        column(width = 4,
          box(status = NULL, solidHeader = T, width = NULL,
              collapsible = TRUE, align = "center",
              h5(strong("Menú de opciones"), 
                 style = "margin-top: 0"),
              selectInput(
                inputId = "selec_mapa_c",
                label = h5("Criterio:",
                           style = "margin-top: 0; margin-bottom: 0"),
                choices = c("Cantidad de contratos" = "cuantia",
                            "Valor de contratos" = "Valor"),
                selected = "Cantidad de contratos"),
              selectInput(
                inputId = "selec_mapa_d",
                label = h5("Departamento:",
                        style = "margin-top: 0; margin-bottom: 0"),
                choices =  list_dep,
                selected = "Nacional"
              )),
          tabBox(
            title = "Top", side = "right", width = NULL,
            selected = textOutput("tab_mapa"),
            tabPanel(title = textOutput("tab_mapa"), 
                     plotlyOutput("top_municipios", 
                                  width = "100%", height = 370)),
            tabPanel(title = "Producto", 
                     plotlyOutput("top_productos", 
                                  width = "100%", height = 370))
          )
        )       
      )
        
      ),
      
      ### 5.3.3. Seccion Proceso ----
      tabItem(
        tabName = "procesos",
        
        fluidRow(
          column(width = 8,
              box(status = NULL, solidHeader = T, width = NULL,
              plotlyOutput("sankey_plot", 
                           width = "100%", height = 650)
          )),
         column(
          width = 4,
          box(status = NULL, solidHeader = T, width = NULL,
              collapsible = TRUE, align = "center",
              h5(strong("Menú de opciones"), 
                 style = "margin-top: 0"),
              selectInput(
                inputId = "selec_proceso_c",
                label = h5("Criterio:",
                           style = "margin-top: 0; margin-bottom: 0"),
                choices = c("Cantidad de contratos" = "Cantidad",
                            "Valor de contratos" = "Valor"),
                selected = "Cantidad de contratos"),
              selectInput(
                inputId = "selec_proceso_v",
                label = h5("Variable del proceso:",
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
              )),
          box(
            title = textOutput("estado_torta"), 
            width = NULL, solidHeader = T, align = "center",
            plotlyOutput("composicion_proceso", 
                                  width = "100%", height = 370))
          )       
        )
      ),
      
      ### 5.3.4. Seccion Objeto a contratar ----
      tabItem(
        tabName = "objeto",
        
        fluidRow(
          column(width = 8,
           box(
            title = "Familia de productos", 
            width = NULL, solidHeader = T,
            plotlyOutput("objeto_tree", width = "auto",
                         height = 520)
           )),
          column(
            width = 4,
            box(status = NULL, solidHeader = T, width = NULL,
                collapsible = TRUE, align = "center",
                h5(strong("Menú de opciones"), 
                   style = "margin-top: 0"),
                selectInput(
                  inputId = "selec_objeto",
                  label = h5("Criterio:",
                          style = "margin-top: 0; margin-bottom: 0"),
                  choices = c("Cantidad de contratos" = "Cantidad",
                              "Valor de contratos" = "Valor"),
                  selected = "Cantidad de contratos"
                )),
            box(title = "Grupo de productos",
                status = NULL, solidHeader = T, width = NULL,
                plotlyOutput("objeto_grupo_torta", 
                             width = "100%", height = 355)
                )
            ))
      ),
      
      ### 5.3.5. Seccion entidades y contratistas ----
      tabItem(
        tabName = "actores",
        
        fluidRow(
          column( width = 2,
            box(status = NULL, solidHeader = T, width = NULL,
                collapsible = TRUE, align = "center",
                h5(strong("Menú de opciones"), 
                   style = "margin-top: 0"),
                selectInput(
                  inputId = "selec_ent_cont",
                  label = h5("Departamento:",
                      style = "margin-top: 0; margin-bottom: 0"),
                  choices =  list_dep,
                  selected = "Nacional")),
            box(status = NULL, solidHeader = T, width = NULL,
                collapsible = TRUE, align = "center",
                selectInput(
                  inputId = "selec_crt_ec",
                  label = h5("Criterio:", style = "margin-top: 0; 
                                                margin-bottom: 0"),
                  choices = c("Cantidad" = "Cantidad",
                          "Valor" = "Valor",
                          "% Adición" = "ProporcionAdiciones",
                          "% Cont. Directa" = "ContratacionDirecta"), 
                  selected = "Cantidad")
                )),
          column(width = 5,
            box(
             width = NULL, status = "primary", solidHeader = F,
             height = 100, h3(strong("Entidades"), 
                style = "margin-top: 0; margin-bottom: 0"),
             h1(strong(textOutput("ind_entidades")), 
                style = "margin-top: 0; margin-bottom: 0;
                          color:#00728F"),
             h5(textOutput("info_entidades"), 
                style = "margin-top: 0; margin-bottom: 0;
                          color:#00728F")
               ),
            tabBox(
              title = textOutput("estado_entidad"), side = "right",
              width = NULL, selected = "Top",
              tabPanel(title = "Top",
                       plotlyOutput("entidades_plot", width = "100%",
                                    height = 550)),
              tabPanel(title = "Detalle",
                       dataTableOutput("entidades_table", 
                                    width = "100%"))
            )),
          column(width = 5,
                 box(
                   width = NULL, status = "primary", solidHeader = F,
                   height = 100, h3(strong("Contratistas"), 
                          style = "margin-top: 0; margin-bottom: 0"),
                   h1(strong(textOutput("ind_contratistas")), 
                      style = "margin-top: 0; margin-bottom: 0;
                               color:#00728F"),
                   h5(textOutput("info_contratistas"), 
                      style = "margin-top: 0; margin-bottom: 0;
                               color:#00728F")
                 ),
                 tabBox(
                   title = textOutput("estado_contratista"), 
                   side = "right",
                   width = NULL, selected = "Top",
                   tabPanel(title = "Top",
                            plotlyOutput("contratistas_plot", 
                                         width = "100%",
                                         height = 550)),
                   tabPanel(title = "Detalle",
                            dataTableOutput("contratistas_table", 
                                            width = "100%"))
           ))
          )
        
      ),
      
      ### 5.3.6. Descargar ----
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
server <- function(input, output) {
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
      sum(contratos$valor_total, na.rm = TRUE)/1e9, 
      format = "fg", big.mark = ".", decimal.mark = ","), " MM")
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
  output$general_cantidad_nor <- renderPlotly({
    grafica_serie_tiempo(cantidad = T, acumulado = F)
  })
  # Cantidad acumulado
  output$general_cantidad_acu <- renderPlotly({
    grafica_serie_tiempo(cantidad = T, acumulado = T)
  })
  # Valor diario
  output$general_valor_nor <- renderPlotly({
    grafica_serie_tiempo(cantidad = F, acumulado = F)
  })
  # Valor acumulado
  output$general_valor_acu <- renderPlotly({
    grafica_serie_tiempo(cantidad = F, acumulado = T)
  })
  # Ultima actualizacion
  output$ultima_act <- renderText({
    fecha <- max(contratos$fecha_firma)
    paste(day(fecha), month(fecha), year(fecha), sep = "/")
  })
  
  ## 6.2. Seccion mapa ----
  # Mapa
  output$mapa_plot <- renderLeaflet({

      grafica_mapa(input = input$selec_mapa_d, 
                   operacion = input$selec_mapa_c)

  })
  
  # Top municipio
  output$top_municipios <- renderPlotly({
    
    grafica_top_Dto_Mun(input = input$selec_mapa_d, 
                   operacion = input$selec_mapa_c)
  })
  
  # Top producto
  output$top_productos <- renderPlotly({

      grafica_top_fam(input = input$selec_mapa_d, 
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
    
      sankey_diagram(input$selec_proceso_c)
  })
  
  # grafico de torta composicion
  output$composicion_proceso <- renderPlotly({
    
      diagrama_dona(input = input$selec_proceso_c, 
                    variable = input$selec_proceso_v)
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
    treefamilia(input = input$selec_objeto)
  })
  
  # Gráfica torta grupo de objeto
  output$objeto_grupo_torta <- renderPlotly({
    donagrupo(input = input$selec_objeto)
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
      valor <- "Cantidad de contratos"
    } else if (input$selec_crt_ec == "Valor") {
      valor <- "Valor de contratos"
    } else if (input$selec_crt_ec == "ProporcionAdiciones") {
      valor <- "Porcentaje de adición"
    } else {
      valor <- "Contratación Directa"
    }
    
    valor
  })
  
  # Titulo del estado del cuadro contratistas
  output$estado_contratista <- renderText({
    if (input$selec_crt_ec == "Cantidad") {
      valor <- "Cantidad de contratos"
    } else if (input$selec_crt_ec == "Valor") {
      valor <- "Valor de contratos"
    } else if (input$selec_crt_ec == "ProporcionAdiciones") {
      valor <- "Porcentaje de adición"
    } else {
      valor <- "Contratación Directa"
    }
    
    valor
  })
  
  # Grafica de entidades
  output$entidades_plot <- renderPlotly({
    entidades_function(input1 = input$selec_ent_cont, 
                       input2 = input$selec_crt_ec)
  })
  
  # Tabla de detalle entidades
  output$entidades_table <- renderDataTable({
    entidades_function(input1 = input$selec_ent_cont, 
                       input2 = input$selec_crt_ec, top = F)
  })
  
  # Grafica de entidades
  output$contratistas_plot <- renderPlotly({
    contratistas_function(input1 = input$selec_ent_cont, 
                          input2 = input$selec_crt_ec)
  })
  
  # Tabla de detalle contratistas
  output$contratistas_table <- renderDataTable({
    contratistas_function(input1 = input$selec_ent_cont, 
                          input2 = input$selec_crt_ec, top = F)
  })
  ## 6.6. Sección descarga ----
  # Consulta
  contratos_table_out <- contratos %>% 
    select(nombre_entidad, tipo_contrato, tipo_proceso, 
           objeto_contratar, valor_inicial, valor_adiciones,
           valor_total, nom_contratista, fuente) %>% 
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
  
  ## 6.7. Muestra de la introduccion ----
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
  
  ## 6.8. Notificaciones ----
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
  