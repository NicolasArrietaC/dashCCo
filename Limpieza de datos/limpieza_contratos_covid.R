##  Limpieza de contratos del conjunto de datos mixto del secop I
##  y secop II para el desarrollo del dashboard
##  Fecha de creación: 15/07/2020

# 1. Librerias ----
sapply(
  c('dplyr', 'readr', 'lubridate', 'stringr',
    'stringdist', 'fuzzyjoin'), 
  library, character.only = T
)

# 2. Carga de conjunto de datos ----
# SECOP I

direccion <- 'C:/Users/nico2/Analytica Projects/contratacion_publica/dashCCo/Datasets/'

direccion_comp <- paste0('C:/Users/nico2/Analytica Projects/',
                         'contratacion_publica/dashCCo/Datasets_complementos/')

contratos_SI <- read_csv(file = paste0(direccion_comp, 
                                       "contratos_covid19_SI.csv"), 
                      locale = locale(encoding = "UTF-8"))
# SECOP II
contratos_SII <- read_csv(file = paste0(direccion_comp, 
                                        "contratos_covid19_SII.csv"),
                         locale = locale(encoding = "UTF-8"))

# Clasificador de bienes y servicios
clasificador <- read_csv(file = 
  paste0(direccion_comp, "clasificador_bienes_servicios.csv"),
  locale = locale(encoding = "UTF-8")) 

# Coordenadas
coordenadas <- read_csv(file = paste0(direccion_comp, 
                                      "coordenadas_municipios.csv"),
                        locale = locale(encoding = "UTF-8"))
# Nombres de municipios
nombres_dep <- read_csv(file = paste0(direccion_comp, 
                                      "nombres_municipios.csv"),
                        locale = locale(encoding = "UTF-8"))

# Clasificador de municipios
cat_mup <- read_csv(file = paste0(direccion_comp, 
                                  "categorizacion_municipios.csv"),
                        locale = locale(encoding = "UTF-8"))

# 3. Unión del conjunto de datos ----
## 3.1. Alistamiento de los datos ----
### 3.1.1. SECOP I ----

### 3.1.1.1. Nombre de la entidad

# Reemplazar los nombres con ? y otros simbolos de division
contratos_SI <- contratos_SI %>% 
  mutate(nombre_de_la_entidad = case_when(
    str_detect(string = nombre_de_la_entidad, pattern = "\\¿")~
      str_replace(nombre_de_la_entidad, pattern = "\\¿", 
                  replacement = "-"),
    str_detect(string = nombre_de_la_entidad, pattern = "<U+0096>")~
      str_replace_all(nombre_de_la_entidad, pattern = "<U+0096>", 
                  replacement = "-"),
    TRUE~ nombre_de_la_entidad))

# Reemplazar los nombres con ? y otros simbolos de division
contratos_SI <- contratos_SI %>% 
  mutate(nom_raz_social_contratista = case_when(
    str_detect(string = nom_raz_social_contratista, pattern = "\\¿")~
      str_replace(nom_raz_social_contratista, pattern = "\\¿", 
                  replacement = "-"),
    str_detect(string = nom_raz_social_contratista, pattern = "<U+0096>")~
      str_replace_all(nom_raz_social_contratista, pattern = "<U+0096>", 
                      replacement = "-"),
    TRUE~ nom_raz_social_contratista))

# Remover el departamento del inicio del nombre
contratos_SI <- contratos_SI %>%
  mutate(nombre_de_la_entidad = case_when(
    str_detect(string = nombre_de_la_entidad, pattern = "-")~
      str_sub(nombre_de_la_entidad,
              start = str_locate(nombre_de_la_entidad, 
                                 pattern = "-")[, 1] + 2, 
              end = -1L),
    TRUE~ nombre_de_la_entidad))

# Remover letras
palabras <- c("DEL MUNICIPIO", "MUNICIPIO DE ", "MUNICIPIO",
              "DISTRITO TURÍSTICO CULTURAL E HIST?RICO",
              "DISTRITO ESPECIAL  INDUSTRIAL Y PORTUARIO")

for (i in palabras) {
 contratos_SI <- contratos_SI %>%
  mutate(nombre_de_la_entidad = str_remove_all(
    string = nombre_de_la_entidad, 
    pattern = i)) 
}

# Reducción de letras
palabras <- tibble(
  palabra = c("INSTITUCIÓN EDUCATIVA TÉCNICA",
              "INSTITUCIÓN EDUCATIVA TÉCNICO", 
              "INSTITUCIÓN EDUCATIVA",
              "INSTITUCIÓN UNIVERSITARIA", "INSTITUCIÓN", 
              "MUNICIPAL", "DEPARTAMENTAL", "INSTITUTO",
              "HOSPITAL", "ALCALDÍA", " INSTITUCION EDUCATIVA",
              "METROPOLITANA", "ASOCIACIÓN", "CÁMARA DE COMERCIO"),
  nueva = c("I.E.T.", "I.E.T.", "I.E.", "I.U.", "I.", "MPAL.",
            "DEPTAL.", "INST.", "HOSP.", "ALC.", "I.E.", "METR.",
            "ASN.", "C.D.C.")
)

for (i in 1:nrow(palabras)) {
  contratos_SI <- contratos_SI %>%
    mutate(nombre_de_la_entidad = str_replace_all(
      string = nombre_de_la_entidad,
      pattern = palabras[i, 1][[1]], 
      replacement = palabras[i, 2][[1]])) 
}

### 3.1.1.2. ID de la entidad 
# Eliminacion de los simbolos diferentes a numeros
contratos_SI <- contratos_SI %>%
  mutate(nit_de_la_entidad = str_remove_all(
    string = nit_de_la_entidad, 
    pattern = "[^0-9]"))

### 3.1.1.3. Ajuste de nombres en departamento contratista
contratos_SI <- contratos_SI %>% mutate(
  dpto_y_muni_contratista = case_when(
    str_detect(dpto_y_muni_contratista, "San Andrés, Providencia y")~
      "Archipiélago de San Andrés",
    TRUE~ dpto_y_muni_contratista
  ))

### 3.1.1.4. Fecha de firma
contratos_SI <- contratos_SI %>% 
  mutate(fecha_de_firma_del_contrato = 
           ymd(fecha_de_firma_del_contrato))

### 3.1.1.5. Estandarizacion adiciones
contratos_SI <- contratos_SI %>% 
  mutate(adiciones_dias = tiempo_adiciones_en_dias + 
           tiempo_adiciones_en_meses*30)

# Eliminación redundante
contratos_SI$tiempo_adiciones_en_dias <- NULL
contratos_SI$tiempo_adiciones_en_meses <- NULL

### 3.1.1.6. Estandarización de la categoría regimen especial
contratos_SI <- contratos_SI %>% 
  mutate(regimen_de_contratacion = case_when(
    str_detect(string = regimen_de_contratacion, 
               pattern = "Régimen Especial", negate = T)~ "Estatuto General de Contratación",
    TRUE~ "Régimen Especial"
  ))

### 3.1.1.7. Asignacion de nombres
contratos_SI <- contratos_SI %>% 
  rename(
    uid = uid, nivel_entidad = nivel_entidad, orden_entidad = orden_entidad,
    nombre_entidad = nombre_de_la_entidad, nit_entidad = nit_de_la_entidad,
    tipo_proceso = tipo_de_proceso, estado_proceso = estado_del_proceso,
    regimen_contratacion = regimen_de_contratacion, 
    objeto_contratar = objeto_a_contratar, 
    detalle_objeto =  detalle_del_objeto_a_contratar, 
    tipo_contrato = tipo_de_contrato, 
    municipio_ejecucion = municipios_ejecucion,
    valor_estimado = cuantia_proceso, nombre_grupo = nombre_grupo, 
    nombre_familia = nombre_familia, nombre_clase = nombre_clase, 
    tipo_id_contratista = tipo_identifi_del_contratista, 
    id_contratista = identificacion_del_contratista,
    nom_contratista = nom_raz_social_contratista, 
    departamento_contratista = dpto_y_muni_contratista,
    fecha_firma = fecha_de_firma_del_contrato, 
    valor_inicial = cuantia_contrato,
    valor_adiciones = valor_total_de_adiciones,
    valor_total = valor_contrato_con_adiciones, 
    ruta_web = ruta_proceso_en_secop_i, 
    municipio_entidad = municipio_entidad,
    departamento_entidad = departamento_entidad, 
    adiciones_dias = adiciones_dias
  )

### 3.1.1.8. Asignación de nuevas variables 
contratos_SI$fecha_adjudicacion <- contratos_SI$fecha_firma
contratos_SI$proveedores_inv <- 0
contratos_SI$proveedores_inv_personal <- 0
contratos_SI$proveedores_unicos <- 0
contratos_SI$fuente <- "SECOP I"

### 3.1.2. SECOP II ----

### 3.1.2.1. Regimen de contratacion
contratos_SII <- contratos_SII %>% 
  mutate(regimen_contratacion = case_when(
    str_detect(string = modalidad_de_contratacion, 
               pattern = "régimen especial")~ "Régimen Especial",
    TRUE~ "Estatuto General de Contratación"
  ))

### 3.1.2.2. Orden entidad
contratos_SII <- contratos_SII %>% mutate(
  codigo_pci = case_when(
    codigo_pci == "No"~ paste(ordenentidad, "Descentralizada", 
                              sep = " - "),
    codigo_pci == "Si"~ paste(ordenentidad, "Centralizada", 
                              sep = " - "),
    TRUE~ paste(ordenentidad, codigo_pci, 
                sep = " - ")))

### 3.1.2.3. Nombre entidad
contratos_SII <- contratos_SII %>% mutate(
  entidad = paste(entidad, nombre_de_la_unidad_de, sep = " - "))

# Remover letras
palabras <- c("MUNICIPIO DE ", "MUNICIPIO",
              "DISTRITO TURÍSTICO CULTURAL E HISTÓRICO",
              "DISTRITO ESPECIAL INDUSTRIAL Y PORTUARIO DE",
              "LOCAL", 
              " DEPARTAMENTO DEL PUTUMAYO - Contratación Salud",
              " DE RIS.A.RALDA - SECRETARIA DE GOBIERNO")

for (i in palabras) {
  contratos_SII <- contratos_SII %>%
    mutate(entidad = str_remove_all(
      string = entidad, 
      pattern = i)) 
}

# Reducción de letras
palabras <- tibble(
  palabra = c("<U+0096>", "INSTITUCIÓN EDUCATIVA TÉCNICA",
              "INSTITUCIÓN EDUCATIVA TÉCNICO", 
              "INSTITUCIÓN EDUCATIVA",
              "INSTITUCIÓN UNIVERSITARIA", "INSTITUCIÓN", 
              "MUNICIPAL", "DEPARTAMENTAL", "INSTITUTO",
              "HOSPITAL", "ALCALDÍA", " INSTITUCION EDUCATIVA",
              "METROPOLITANA", "ASOCIACIÓN", "CÁMARA DE COMERCIO",
              "PATRIMONIO AUTÓNOMO PROCOLOMBIA"),
  nueva = c("-", "I.E.T.", "I.E.T.", "I.E.", "I.U.", "I.", "MPAL.",
            "DEPTAL.", "INST.", "HOSP.", "ALC.", "I.E.", "METR.",
            "ASN.", "C.D.C.", 
            "PATRIMONIO AUTONOMO COLOMBIA PRODUCTIVA")
)

for (i in 1:nrow(palabras)) {
  contratos_SII <- contratos_SII %>%
    mutate(entidad = str_replace_all(
      string = entidad,
      pattern = palabras[i, 1][[1]], 
      replacement = palabras[i, 2][[1]])) 
}

### Eliminación de la variables de nombre...(redundante)
contratos_SII$nombre_de_la_unidad_de <- NULL

### 3.1.2.4. Departamento contratista
contratos_SII <- contratos_SII %>% mutate(
  departamento_contratista = case_when(
    str_detect(departamento_proveedor, "Distrito Capital de Bogotá")~
      "Bogotá D.C.",
    str_detect(departamento_proveedor, "San Andrés, Providencia y")~
      "Archipiélago de San Andrés",
    str_detect(departamento_proveedor, "No aplica")~
      "No Definido",
    TRUE~ departamento_proveedor
  ))

### 3.1.2.5. Eliminación de la variables de nombre...(redundante)
contratos_SII$departamento_proveedor <- NULL
contratos_SII$ciudad_proveedor <- NULL

### 3.1.2.6. segmento, familia y clase de productos
clasificador <- clasificador %>% 
  select(-c("codigo_producto", "nombre_producto", 
            "codigo_segmento", "codigo_familia")) %>% 
  rename(codigo_principal_de_categoria = codigo_clase,
         nombre_grupo = nombre_segmento) %>% unique()

###  Eliminación del patron V1.
contratos_SII <- contratos_SII %>%  rowwise() %>%
  mutate(codigo_principal_de_categoria = 
           str_split(string = codigo_principal_de_categoria, 
                     pattern = "\\.")[[1]][2])

### Eliminacion de la categoria de producto         
contratos_SII <- contratos_SII %>%       
  mutate(codigo_principal_de_categoria = 
           as.integer(as.numeric(codigo_principal_de_categoria)/100))

### Agregar nombres de las categorias de cada producto
contratos_SII <- contratos_SII %>% 
  merge(x = ., y = clasificador, 
        by = "codigo_principal_de_categoria",
        all.x = T)

### Reajustar la clasificacion de grupo
contratos_SII <- contratos_SII %>% 
  mutate(nombre_grupo = case_when(
    (codigo_principal_de_categoria >= 100000 &
       codigo_principal_de_categoria < 120000)~
      "[A] Material Vivo Animal y Vegetal",
    (codigo_principal_de_categoria >= 120000 &
       codigo_principal_de_categoria < 160000)~
      "[B] Materias Primas",
    (codigo_principal_de_categoria >= 200000 &
       codigo_principal_de_categoria < 280000)~
      "[C] Maquinaria, Herramientas, Equipo Industrial y Vehículos",
    (codigo_principal_de_categoria >= 300000 &
       codigo_principal_de_categoria < 500000)~
      "[D] Componentes y Suministros",
    (codigo_principal_de_categoria >= 500000 &
       codigo_principal_de_categoria < 700000)~
      "[E] Productos de Uso Final",
    (codigo_principal_de_categoria >= 700000 &
       codigo_principal_de_categoria < 950000)~
      "[F] Servicios",
    TRUE~"[G] Terrenos, Edificios, Estructuras y vías"
  ))



## Eliminación
contratos_SII$codigo_principal_de_categoria <- NULL

# 3.1.2.6. Eliminación de contratos que no tienen fecha de firma
contratos_SII <- contratos_SII %>% 
  filter(!is.na(fecha_adjudicacion))

# 3.1.2.7. Limpieza de registros con el municipio de entidad no def.
contratos_SII <- contratos_SII %>% 
  mutate(ciudad_entidad = case_when(
    ciudad_entidad == "No Definido" & 
    ciudad_de_la_unidad_de != "No Definido"~ ciudad_de_la_unidad_de,
    TRUE ~ ciudad_entidad))

### 3.1.2.8. Ajuste en las columnas
contratos_SII <- contratos_SII %>% 
  rename(
    uid = id_del_proceso, nivel_entidad = ordenentidad, 
    orden_entidad = codigo_pci, nombre_entidad = entidad,
    nit_entidad = nit_entidad, tipo_proceso = modalidad_de_contratacion, 
    estado_proceso = estado_del_procedimiento, 
    objeto_contratar = nombre_del_procedimiento, 
    detalle_objeto = descripci_n_del_procedimiento,
    tipo_contrato = tipo_de_contrato, 
    municipio_ejecucion = ciudad_de_la_unidad_de, valor_estimado = precio_base,
    id_contratista = nit_del_proveedor_adjudicado, 
    nom_contratista = nombre_del_proveedor,
    fecha_firma = fecha_de_publicacion_del, 
    fecha_adjudicacion = fecha_adjudicacion,
    valor_total = valor_total_adjudicacion, ruta_web = urlproceso, 
    municipio_entidad = ciudad_entidad,
    departamento_entidad = departamento_entidad, 
    regimen_contratacion = regimen_contratacion, 
    departamento_contratista = departamento_contratista, 
    nombre_grupo = nombre_grupo, nombre_familia = nombre_familia, 
    nombre_clase = nombre_clase, proveedores_inv = proveedores_invitados,
    proveedores_inv_personal = proveedores_con_invitacion, 
    proveedores_unicos = proveedores_unicos_con
  )

### 3.1.2.8. Asignación de nuevas variables
contratos_SII$tipo_id_contratista <- NA
contratos_SII$valor_inicial <- contratos_SII$valor_estimado
contratos_SII$adiciones_dias <- 0
contratos_SII$fuente <- "SECOP II"

contratos_SII <- contratos_SII %>% 
  mutate(valor_adiciones = ifelse(valor_inicial >= valor_total, 
                                  0, valor_total - valor_inicial))

## 3.2. Union ----
contratos <- rbind(contratos_SI, contratos_SII)

# 4. Preprocesamiento ----

## Eliminación de repetidos
contratos <- contratos %>% distinct()

## 4.1. Filtro del rango del tiempo de pandemia ----
contratos <- contratos %>%
  filter(fecha_firma >= mdy("03-06-2020"), 
         fecha_firma <= today())

## 4.2. Filtro de contratos con estado No definido ----
contratos <- contratos %>% filter(estado_proceso != "No Definido")

## 4.3. Filtro registros con NAs en el valor Inicial/Total ----
### Estos registros presentan NAs en la mayoria de columnas, y no
### tienen url del contrato
contratos <- contratos %>% filter(!is.na(valor_total))

## 4.4. Ajuste de valores en tipo de proceso ----
contratos <- contratos %>% 
  mutate(tipo_proceso = str_to_lower(tipo_proceso),
    tipo_proceso = case_when(
    str_detect(tipo_proceso, 
               pattern = "licitación pública|obra pública|subasta") ~ 
      "Licitación pública",
    str_detect(tipo_proceso, 
               pattern = "selección abreviada de menor cuantía") ~ 
      "Selección abreviada",
    str_detect(tipo_proceso, 
               pattern = "concurso de méri") ~ "Concurso de méritos",
    str_detect(tipo_proceso, pattern = "contratación directa|dos partes") ~
      "Contratación directa",
    str_detect(tipo_proceso, pattern = "mínima cuantía") ~ "Mínima cuantía",
    str_detect(tipo_proceso, pattern = "público privada") ~ 
      "Asociación público privada",
    str_detect(tipo_proceso, pattern = "régimen especial") | 
      str_detect(regimen_contratacion, pattern = "Régimen Especial") ~ 
      "Régimen especial",
    TRUE ~ tipo_proceso))

## 4.5. Ajuste de valores en tipo de contrato ----
contratos <- contratos %>% 
  mutate(tipo_contrato = case_when(
    str_detect(str_to_lower(tipo_contrato),
               pattern = "otro|aprovisionamiento|servicios")~ 
      "Prestación de servicios",
    str_detect(str_to_lower(tipo_contrato),
               pattern = "interventoría")~ "Consultoría",
    str_detect(str_to_lower(tipo_contrato),
               pattern = "fiducia")~ "Encargos fiduciarios",
    str_detect(str_to_lower(tipo_contrato),
               pattern = "092")~ 
      "Contrato con entidades privados sin ánimo de lucro",
    str_detect(str_to_lower(tipo_contrato),
               pattern = "suministro")~ "Suministros",
    str_detect(str_to_lower(tipo_contrato),
               pattern = "arrendamiento")~ "Arrendamiento",
    str_detect(str_to_lower(tipo_contrato),
               pattern = "nd")~ "No definido",
    TRUE~ tipo_contrato))

## 4.6. Ajuste de los departamento de departamento_entidad ----
contratos <- contratos %>% 
  mutate(departamento_entidad = case_when(
    str_detect(str_to_lower(departamento_entidad),
               pattern = "bogotá")~ "Bogotá D.C.",
    TRUE~ departamento_entidad))

## 4.7. Ajuste de la clasificacion grupo ----
contratos <- contratos %>% 
  mutate(nombre_grupo = case_when(
    nombre_grupo == "[A] Material Vivo Animal y Vegetal"~
      "Material Vivo Animal y Vegetal",
    nombre_grupo == "[B] Materias Primas"~ "Materias Primas",
    nombre_grupo == 
      "[C] Maquinaria, Herramientas, Equipo Industrial y Vehículos"~
      "Maquinaria, Herramientas, Equipo Industrial y Vehículos",
    nombre_grupo == "[D] Componentes y Suministros"~
      "Componentes y Suministros",
    nombre_grupo == "[E] Productos de Uso Final"~
      "Productos de Uso Final",
    nombre_grupo == "[F] Servicios"~ "Servicios",
    nombre_grupo == "[G] Terrenos, Edificios, Estructuras y vías"~
      "Terrenos, Edificios, Estructuras y vías",
  ))

## 4.8. Ajuste de las coordenadas de los municipios ----
### 4.8.1. Conjunto de coordenadas ----

## Casos de limpieza:
## 1) PUERTO SANTANDER (ANM)
## 2) PIENDAMO - TUNIA

### Municipio
coordenadas <- coordenadas %>% rowwise() %>% 
  mutate(nombre_municipio_2 = 
           strsplit(nombre_municipio, 
                    split = "\\(" )[[1]][1]) %>%  #Error de espacio
  mutate(nombre_municipio_2 = 
           strsplit(nombre_municipio_2, 
                    split = "\\ -" )[[1]][1])

### Departamento
coordenadas <- coordenadas %>% 
  mutate(departamento_municipio = 
           paste(nombre_departamento,
                 nombre_municipio_2, 
                 sep = "-"))
### Transformar todos los caracteres en minuscula
coordenadas$departamento_municipio <- str_to_lower(
  string = coordenadas$departamento_municipio)

### 4.8.2. Conjunto de datos de contratos ----

### table(contratos$municipio_entidad_2)

# Ajuste de los no definidos
ajuste_municipio_nd <- function(df, municipio, capital){
  df <- df %>% 
    mutate(municipio_entidad = case_when(
      (departamento_entidad == municipio &
         municipio_entidad == "No Definido") ~ capital,
      TRUE ~ municipio_entidad
    ))
  
  return(df)
}
# LLamado de la función
contratos <- ajuste_municipio_nd(contratos, "Antioquia", "Medellín")
contratos <- ajuste_municipio_nd(contratos, "Boyacá", "Tunja")
contratos <- ajuste_municipio_nd(contratos, "Cauca", "Popayán")
contratos <- ajuste_municipio_nd(contratos, "Risaralda", "Pereira")
contratos <- ajuste_municipio_nd(contratos, "Santander", 
                                 "Bucaramanga")
contratos <- ajuste_municipio_nd(contratos, "Valle del Cauca", 
                                 "Cali")
contratos <- ajuste_municipio_nd(contratos, "Putumayo", "Mocoa")
contratos <- ajuste_municipio_nd(contratos, 
                 "San Andrés, Providencia y Santa Catalina", 
                                 "San Andrés")
contratos <- ajuste_municipio_nd(contratos, "Bogotá D.C.", 
                                 "Bogotá D.C.")


## Casos de limpieza: municipio
## 1) Chíquiza (San Pedro de Iguaque)
## 2) Cuaspud/Carlosama

### Municipio
contratos <- contratos %>% 
  rowwise() %>% 
  mutate(municipio_entidad_2 = 
           strsplit(as.character(municipio_entidad), 
                    split = "\\(" )[[1]][1]) %>% #Error de espacio
  mutate(municipio_entidad_2 = 
           strsplit(municipio_entidad_2, 
                    split = "/" )[[1]][1])

### Departamento
contratos <- contratos %>% 
  mutate(departamento_municipio = 
           paste(departamento_entidad,
                 municipio_entidad_2, 
                 sep = "-"))

### Transformar todos los caracteres en minuscula y quitar tíldes
contratos$departamento_municipio <- str_to_lower(
  string = contratos$departamento_municipio)

contratos$departamento_municipio <- chartr(
  'áéíóúñü','aeiounu',
  contratos$departamento_municipio)


### 4.8.3. Algorítmo de emparejamiento del atributo llave ----
# Conjunto de dato
municipios_general <- contratos %>% 
  select(departamento_municipio) %>% 
  distinct()

## Algoritmo de emparejamiento
for(i in 1: nrow(municipios_general)){   
  # metodo estadistico
  valor_z <- stringdist(as.character(
    municipios_general[i, "departamento_municipio"]), 
    as.character(coordenadas$departamento_municipio), 
    method = "jw", 
    p = 0.1)
  
  posicion <- which.min(valor_z)
  
  # Emparejar valores de codigo dto y mun según el grado de similitud
  if(valor_z[posicion] < 0.15){
    
      municipios_general[i, "valor_emparejamiento"] <- valor_z[posicion]
      municipios_general[i, "municipio_coordenadas"] <-
        coordenadas[posicion, "departamento_municipio"]
      municipios_general[i, "codigo_municipio"] <-
        as.character(coordenadas[posicion, "codigo_dpto_mpio"])
      municipios_general[i, "codigo_departamento"] <- 
        as.character(coordenadas[posicion, "codigo_departamento"])
  } 
  else
  {
      municipios_general[i, "valor_emparejamiento"] <- 1
      municipios_general[i, "municipio_coordenadas"] <- "ninguno"
      municipios_general[i, "codigo_municipio"] <- "ninguno"
      municipios_general[i, "codigo_departamento"] <- "ninguno"
  }
}

print(paste("Existen",
            nrow(municipios_general %>% 
                   filter(valor_emparejamiento > 0.05)),
            "registros críticos en el emparejamiento de ", 
            'los municipios', sep = " "))

### 4.8.4. Ajuste manual ----

### Columnas de departamento_municipio // municipios_general 
### (contratos): 

### * san andres, providencia y santa catalina-san andres 
### * san andres, providencia y santa catalina-providencia  
### * sucre-toluviejo  
### * tolima-guayabal  
### *	cordoba-purisima  
### * boyaca-guican  
### * norte de santander-san jose de cucuta 
### * huila-el pital 
### * cauca-sotara 
### * bolivar-santa rosa de lima

mod_codimup <- function(municipio, cod_dep, codigo_mun,
                        nomb_mun = ""){
  municipios_general[municipios_general$departamento_municipio == 
                       municipio, "codigo_municipio"] <- codigo_mun
  municipios_general[municipios_general$departamento_municipio == 
                       municipio, "codigo_departamento"] <- cod_dep
  
  if (nomb_mun != "") {
    municipios_general[municipios_general$departamento_municipio == 
                  municipio, "departamento_municipio"] <- nomb_mun 
  }
  
  
  return(municipios_general)
}



### san andres, providencia y santa catalina-san andres
municipios_general <- mod_codimup(
  "san andres, providencia y santa catalina-san andres", 
  "88", "88001")

municipios_general <- mod_codimup(
  "san andres, providencia y santa catalina-no definido",
  "88", "88001")

### san andres, providencia y santa catalina-providencia 
municipios_general <- mod_codimup(
  "san andres, providencia y santa catalina-providencia",
  "88", "88001")

### sucre-toluviejo
municipios_general <- mod_codimup("sucre-toluviejo",
                                  "70", "70823")

### tolima-guayabal = armero
municipios_general <- mod_codimup("tolima-guayabal",
                                  "73", "73055")

# tolima-no definido
municipios_general <- mod_codimup("tolima-no definido",
                                  "73", "73001")

# risaralda-no definido
municipios_general <- mod_codimup("risaralda-no definido",
                                  "66", "66001")

# boyaca-no definido
municipios_general <- mod_codimup("boyaca-no definido",
                                  "15", "15001")

# cauca-no definido
municipios_general <- mod_codimup("cauca-no definido",
                                  "19", "19001")

### cordoba-purisima
municipios_general <- mod_codimup("cordoba-purisima",
                                  "23", "23586")

### boyaca-guican
municipios_general <- mod_codimup("boyaca-guican",
                                  "15", "15332")

### norte de santander-san jose de cucuta
municipios_general <- mod_codimup(
  "norte de santander-san jose de cucuta", "54", "54001")

### huila-el pital
municipios_general <- mod_codimup("huila-el pital", "41", "41548")

### cauca-sotara
municipios_general <- mod_codimup("cauca-sotara", "19", "19760")

### bolivar-santa rosa de lima = Santa Rosa del sur
municipios_general <- mod_codimup("bolivar-santa rosa de lima",
                                  "13", "13688")

### distrito capital de bogota-bogota
municipios_general <- mod_codimup("distrito capital de bogota-bogota",
                                  "11", "11001")

### distrito capital de bogota-bogota
municipios_general <- mod_codimup("bogota d.c.-no definida",
                                  "11", "11001")

### no definido-tunja
municipios_general <- mod_codimup("no definido-tunja",
                                  "15", "15001")

### no definido-manizales
municipios_general <- mod_codimup("no definido-manizales",
                                  "17", "17001")

### no definido-sachica
municipios_general <- mod_codimup("no definido-sachica",
                                  "15", "15638")

### no definido-barranquilla
municipios_general <- mod_codimup("no definido-barranquilla",
                                  "08", "08078")

### no definido-fusagasuga
municipios_general <- mod_codimup("no definido-fusagasuga",
                                  "25", "25290")

### no definido-pereira
municipios_general <- mod_codimup("no definido-pereira",
                                  "66", "66001")

### no definido-bogota
municipios_general <- mod_codimup("no definido-bogota",
                                  "11", "11001")

### no definido-popayan
municipios_general <- mod_codimup("no definido-popayan",
                                  "19", "19001")

### no definido-bucaramanga
municipios_general <- mod_codimup("no definido-bucaramanga",
                                  "68", "68001")

### no definido-armenia
municipios_general <- mod_codimup("no definido-armenia",
                                  "63", "63001")

### no definido-cali
municipios_general <- mod_codimup("no definido-cali",
                                  "76", "76001")

### no definido-riohacha
municipios_general <- mod_codimup("no definido-riohacha",
                                  "44", "44001")

### san andres, providencia y santa catalina-no definida
municipios_general <- mod_codimup(
  "san andres, providencia y santa catalina-no definida",
  "88", "88001")

### no definido-inirida
municipios_general <- mod_codimup("no definido-inirida",
                                  "94", "94001")

### no definido-yopal
municipios_general <- mod_codimup("no definido-yopal",
                                  "85", "85001")

### no definido-circasia
municipios_general <- mod_codimup("no definido-circasia",
                                  "63", "63190")

### no definido-cucuta
municipios_general <- mod_codimup("no definido-cucuta",
                                  "54", "54001")

### bogota d.c.-cajica
municipios_general <- mod_codimup("bogota d.c.-cajica",
                                  "11", "11001")

### no definido-madrid
municipios_general <- mod_codimup("no definido-madrid",
                                  "25", "25430")

### no definido-sincelejo
municipios_general <- mod_codimup("no definido-sincelejo",
                                  "70", "70001")

### no definido-soledad
municipios_general <- mod_codimup("no definido-soledad",
                                  "08", "08758")

### no definido-ibague
municipios_general <- mod_codimup("no definido-ibague",
                                  "73", "73001")

### no definido-quibdo
municipios_general <- mod_codimup("no definido-quibdo",
                                  "27", "27001")

### no definido-monterrey
municipios_general <- mod_codimup("no definido-monterrey",
                                  "85", "85162")

### no definido-gachantiva
municipios_general <- mod_codimup("no definido-gachantiva",
                                  "15", "15293")

### bogota d.c.-neiva
municipios_general <- mod_codimup("bogota d.c.-neiva",
                                  "11", "11001")

### boyaca-no definida
municipios_general <- mod_codimup("boyaca-no definida",
                                  "15", "15001")


### cordoba-no definida
municipios_general <- mod_codimup("cordoba-no definida",
                                  "23", "23001")


### cauca-no definida
municipios_general <- mod_codimup("cauca-no definida",
                                  "19", "19001")


### risaralda-no definida
municipios_general <- mod_codimup("risaralda-no definida",
                                  "66", "66001")

### 4.8.5. Unión del conjuntos de datos - cont-coord ----
contratos <- merge(x = contratos,
                   y = municipios_general %>% 
                     select(departamento_municipio,
                            codigo_departamento, codigo_municipio),
                   by = "departamento_municipio", 
                   all.x =TRUE) %>% 
  select(-departamento_municipio)

contratos <- merge(x = contratos,
                   y = coordenadas %>%
                     select(codigo_dpto_mpio,
                            latitud,longitud) %>% 
                     group_by(codigo_dpto_mpio) %>% 
                     summarise(latitud = mean(latitud, 
                                              na.rm = TRUE),
                               longitud = mean(longitud, 
                                               na.rm = TRUE)) %>% 
                     ungroup(),
                   by.x = "codigo_municipio", 
                   by.y = "codigo_dpto_mpio",
                   all.x = TRUE)

### 4.8.6. Ajuste manual de coordenadas ----

## Ajuste manual
lat <- c(1.679124, 9.237710, 8.947314, 2.934249,
         3.988205, 1.207692, 7.896264, 8.468918,
         7.539238, 6.693988, 3.883085, 4.212841, 
         3.861065, 2.565545)

lng <- c(-75.284151, -75.722331, -75.444541,
         -75.280831, -73.765802, -77.281705,
         -72.507036, -73.337638, -72.772970,
         -73.018938, -77.019447, -76.318793,
         -76.382855, -72.639938)

muni <- c("El Doncello", "Purísima", "Sahagún", 
          "Neiva", "Acacías", "Pasto",
          "San José de Cúcuta", "Convención", "Cucutilla",
          "Aratoca", "Buenaventura", "Trujillo",
          "Yotoco", "San José del Guaviare")

dep <- c("Caquetá", "Córdoba", "Córdoba",
         "Huila", "Meta", "Nariño", "Norte De Santander",
         "Norte De Santander", "Norte De Santander",
         "Santander", "Valle del Cauca", "Valle del Cauca",
         "Valle del Cauca", "Guaviare")

for (i in 1:length(lat)) {
  contratos <- contratos %>% 
    mutate(latitud = 
             ifelse(municipio_entidad == muni[i] &
                      departamento_entidad == dep[i], 
                    lat[i], 
                    latitud),
           longitud = 
             ifelse(municipio_entidad == muni[i] &
                      departamento_entidad == dep[i], 
                    lng[i], 
                    longitud))}

# 4.8.7. Redefinir los nombres de los municipios ----
# codigo de municipio secop
contratos <- contratos %>% 
  mutate(codigo_municipio = as.numeric(codigo_municipio))

# Ajustar los nombres del conjunto de nombres
nombres <- c("region", "codigo_dep", "departamento", 
             "codigo_munp", "municipio")

names(nombres_dep) <- nombres


# Unión del los nombres del conjunto de datos
contratos <- nombres_dep %>% 
  select(codigo_dep, codigo_munp,
         departamento, municipio) %>% distinct() %>% 
  rename(municipio_entidad = municipio,
         departamento_entidad = departamento,
         codigo_departamento = codigo_dep,
         codigo_municipio = codigo_munp) %>% 
  merge(x = ., y = contratos %>% select(-c("municipio_entidad", 
                                           "departamento_entidad", 
                                           "codigo_departamento")),
        by = "codigo_municipio",
        all.y = TRUE)

# Ajuste de los datos que presentan problemas
contratos <- contratos %>% 
  mutate(departamento_entidad = case_when(
    str_detect(departamento_entidad, "Archipiélago de San Andrés")~
      "Archipiélago de San Andrés",
    is.na(departamento_entidad)~ "No definido",
    TRUE~ departamento_entidad),
    municipio_entidad = case_when(
      is.na(municipio_entidad)~ "No definido",
      TRUE~ municipio_entidad))

# Reemplazo de nombre de atributos y eliminacion de atributos del proceso
contratos <- contratos %>% 
  rename(codigo_municipio_ent = codigo_municipio,
         codigo_departamento_ent = codigo_departamento)

contratos$municipio_entidad_2 <- NULL

# 4.9 Ajuste del atributo nivel y orden de entidad ----
contratos <- contratos %>% 
  mutate(nivel_entidad = case_when(
    nivel_entidad == "TERRITORIAL" ~ "Territorial",
    nivel_entidad == "NACIONAL" ~ "Nacional",
    TRUE ~ nivel_entidad))

contratos <- contratos %>% 
  mutate(orden_entidad = case_when(
    str_detect(orden_entidad, "Corporación Autónoma") ~ "Corporación Autónoma",
    str_detect(orden_entidad, "Nacional") ~ "Nacional",
    str_detect(orden_entidad, "Territorial") ~ "Territorial",
    orden_entidad == "AREA METROPOLITANA" ~ "Área Metropolitana",
    orden_entidad == "DISTRITO CAPITAL" ~ "Distrito Capital",
    orden_entidad == "NACIONAL CENTRALIZADO" ~ "Nacional Centralizado",
    orden_entidad == "NACIONAL DESCENTRALIZADO" ~ "Nacional Descentralizado",
    orden_entidad == "TERRITORIAL DEPARTAMENTAL CENTRALIZADO" ~ "TDC",
    orden_entidad == "TERRITORIAL DEPARTAMENTAL DESCENTRALIZADO" ~ "TDD",
    orden_entidad == "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 1" ~ "TDM1",
    orden_entidad == "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 2" ~ "TDM2",
    orden_entidad == "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 3" ~ "TDM3",
    orden_entidad == "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 4" ~ "TDM4",
    orden_entidad == "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 5" ~ "TDM5",
    orden_entidad == "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 6" ~ "TDM6",
    TRUE ~ orden_entidad
  ))

# 4.10 Asignación de la categoria del municipio ----
# Ajuste del atributo codigo munp en el conjunto cat_mup
cat_mup$Id_Entidad <- cat_mup$Id_Entidad%%1e5

contratos <- merge(x = contratos, 
      y = cat_mup %>% 
        select(Id_Entidad, `Categoria municipio - vigencia 2020`) %>% 
        rename(codigo_municipio_ent = Id_Entidad,
               categoria_munp_ent = `Categoria municipio - vigencia 2020`), 
      by = "codigo_municipio_ent", all.x = TRUE)

# Ajuste manual
contratos[is.na(contratos$codigo_municipio_ent), "codigo_municipio_ent"] <- 0
contratos[contratos$codigo_municipio_ent == "13490", "categoria_munp_ent"] <- 6
contratos[contratos$codigo_municipio_ent == "19300", "categoria_munp_ent"] <- 5
contratos[contratos$codigo_municipio_ent == "23682", "categoria_munp_ent"] <- 6
contratos[contratos$codigo_municipio_ent == "23815", "categoria_munp_ent"] <- 6
contratos[contratos$codigo_municipio_ent == "88001", "categoria_munp_ent"] <- 5

# 4.11 Ajustes en los nombres de los contratistas ----
# Limpieza
# Caracteres a limpiar
car_limpieza_c <- c("<U+0096>", "<u+0096>","\\(",": ", "<u+0093>", 
                  "<U+0093>", "<u+0094>","<U+0094>", "/\\)\\(", "\\(")

car_limpieza = "\\¿"

for (i in car_limpieza_c){
  car_limpieza = paste(car_limpieza, i, sep = "|")
}
  
# Limpieza de caracteres en el atributo
contratos <- contratos %>% 
  mutate(nom_contratista = str_remove_all(string = nom_contratista,
                                     pattern = car_limpieza),
    nom_contratista = str_to_upper(nom_contratista),
    nom_contratista = iconv(nom_contratista, 
                            from = "UTF-8",  
                            to = 'ASCII//TRANSLIT'))
# Reemplazo de caracteres
contratos <- contratos %>% 
  mutate(nom_contratista = str_replace_all(nom_contratista, 
                                      pattern = "S.A.S.", replacement = "SAS"))
contratos <- contratos %>% 
  mutate(nom_contratista = str_replace_all(nom_contratista, 
                                      pattern = "S.A.S", replacement = "SAS"))

contratos <- contratos %>% 
  mutate(nom_contratista = str_replace_all(nom_contratista, 
                                      pattern = "S.A.", replacement = "SA"))
contratos <- contratos %>% 
  mutate(nom_contratista = str_replace_all(nom_contratista, 
                                      pattern = "S.A", replacement = "SA"))


contratos <- contratos |> 
  mutate(nom_contratista = if_else(nom_contratista |> is.na(), 
                                   "No definido", nom_contratista))

# Extraccion del atributo
contratistas <- contratos %>% 
  select(id_contratista, nom_contratista) %>% unique()

# Extracción de los que presentan anomalias
# A. Varios nombres en el id
dif_nom <- contratistas %>% group_by(id_contratista) %>%
  summarize(n_dif = n_distinct(nom_contratista)) %>% filter(n_dif > 2)

# B. Varios ID con un solo nombre
dif_nit <- contratistas %>% group_by(nom_contratista) %>%
  summarize(n_dif = n_distinct(id_contratista)) %>% filter(n_dif > 2)

# Busqueda y selección de los que presentan anomalias
# Nota: Se tuvo que seleccionar solamente una representación (más frecuente)
#       de cada nombre, porque al hacer el reemplazo en la parte final se
#       repetia el problema.
contratistas_dif <- contratistas[
  contratistas$id_contratista %in% dif_nom$id_contratista |
  contratistas$nom_contratista %in% dif_nit$nom_contratista,] %>% 
  group_by(id_contratista) %>% slice_max(nom_contratista) %>% ungroup() %>% 
  group_by(nom_contratista) %>% slice_max(id_contratista) %>% ungroup()

# Ejecución del alg. Fuzzy join para emparejar los nombres y nits
resultado_match_cont <- contratistas %>% 
  stringdist_inner_join(contratistas_dif %>% 
                         rename(id_contratista_2 = id_contratista,
                                nom_contratista_2 = nom_contratista), 
                       by = c(id_contratista = "id_contratista_2",
                              nom_contratista = "nom_contratista_2"),
                       max_dist = 4, distance_col = "resl")

# Eliminación de los cruces monos efectivos y redundantes
resultado_match_cont <- resultado_match_cont %>% 
  filter(!(id_contratista.resl == 0 & nom_contratista.resl == 0), # redundancia
         !(id_contratista.resl == 4 & nom_contratista.resl == 4), # no aciertos
         !(id_contratista.resl == 4 & nom_contratista.resl >= 2),
         !(id_contratista.resl == 3 & nom_contratista.resl >= 3),
         !(id_contratista.resl == 2 & nom_contratista.resl >= 4))

# Ajuste manual
# Cruz roja
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
                          "LA CRUZ ROJA COLOMBIANA"), "nom_contratista_2"] <- 
  "CRUZ ROJA COLOMBIANA SECCIONAL CUNDINAMARCA Y BOGOTA"
# Espacio en WILLIAM  OSWALDO  DIAZ JOJOA
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
                      "WILLIAM  OSADO  DIAZ JOJOA"), "nom_contratista_2"] <- 
  "WILLIAM OSADO DIAZ JOJOA"
# Espacio en TALENTO COMERCIALIZADORA SAS
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
                 "TALENTO COMERCIALIZADORA SA"), "nom_contratista_2"] <- 
  "TALENTO COMERCIALIZADORA SAS"
# Espacio en SUMINISTROS Y NEGOCIOS DE LA SABANA SAS
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
          "SUMINISTROS Y NEGOCIOS DE LA SABANAS SAS"), "nom_contratista_2"] <- 
  "SUMINISTROS Y NEGOCIOS DE LA SABANA SAS"

# Espacio en R.R. EDITORES RAMIREZ Y RAMIREZ LIMITADA
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
          "R.R. EDITORES RAMIREZ Y RAMIREZ LIMITADA"), "nom_contratista_2"] <- 
  "R.R EDITORES RAMIREZ Y RAMIREZ LTDA"

# Espacio en PRODUCLINICOS DEL SURR LTDA
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
            "PRODUCLINICOS DEL SURR LTDA"), "nom_contratista_2"] <- 
  "PRODUCLINICOS DEL SUR LTDA"

# Espacio en LM INSTRUMENTS SA
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
                        "LM INSTRUMENTS SA"), "nom_contratista_2"] <- 
  "LM INSTRUMENTS SAS"

# Espacio en DATOS Y GESTIONES E.A.T.
resultado_match_cont[str_detect(resultado_match_cont$nom_contratista_2,
                        "DATOS Y GESTIONES E.A.T."), "nom_contratista_2"] <- 
  "DATOS Y GESTION E.A.T."


# Unión del conjunto de datos
contratos <- contratos %>% merge(x = ., y = resultado_match_cont %>% 
                  select(nom_contratista, nom_contratista_2, id_contratista_2),
                  by = "nom_contratista", all.x = T) %>% 
  mutate(id_contratista = case_when(
    !is.na(id_contratista_2) ~ id_contratista_2,
    TRUE ~ id_contratista),
    nom_contratista = case_when(
      !is.na(nom_contratista_2) ~ nom_contratista_2,
      TRUE ~ nom_contratista)) %>% 
  select(-c("nom_contratista_2", "id_contratista_2"))

contratos[str_detect(contratos$nom_contratista, 
                     "CRUZ ROJA COLOMBIANA SECCIONAL") &
            str_detect(contratos$nom_contratista, "CRUZ ROJA COLOMBIANA"),
          c("nom_contratista", "id_contratista")] <- 
  c("CRUZ ROJA COLOMBIANA", "8600703011")

# 4.12. Remover los repetidos ----
contratos <- contratos %>% distinct()

# 5. Limpieza de registros manual ----

## 5.1. Contratos con la alcaldía de Bogotá ----

### Al revisar el contrato en el prtal web se encontro que es un
### contrato más grande en el cual hacen parte otras 13 localidades

ajuste_1 <- contratos %>% 
  filter(str_detect(nombre_entidad, pattern = "ALC. LOCAL"), 
         fuente == "SECOP I",
         str_detect(str_to_lower(nom_contratista), pattern = "cruz") |
         str_detect(str_to_lower(regimen_contratacion), pattern = "ley")) %>% 
  select(uid, nombre_entidad, nom_contratista,
         valor_estimado, valor_inicial, valor_adiciones, 
         valor_total, ruta_web, regimen_contratacion, 
         tipo_proceso, tipo_contrato) %>%
  arrange(uid)

# Eliminación de registros redundantes
contratos <- contratos %>% 
  filter(!(uid == "20-19-10747417-9871095" & 
            nom_contratista == '8600703011'))

## Observación: en la consulta solamente se capturaron 13 de las 16 
## localidades mencionadas en el contrato

# Ajuste manual de valores
uids <- c("20-19-10747417-9871095", '20-22-15401-9871265',
          '20-22-15365-9868800', '20-22-15386-9867896', 
          '20-22-15331-9858284', '20-22-15375-9866611')

valor_es <- c(7809768038, 3421144656, 1294460009, 617787621,
              915828949, 869203468)

valor_in <- c(6247814430, 2736915725, 1035568007, 494230097,
              732663159, 695362774)

for (i in uids) {
  contratos[contratos$uid == i, 'valor_estimado'] <- 
      valor_es[which(uids == i)]
  
  contratos[contratos$uid == i, 'valor_inicial'] <- 
    valor_in[which(uids == i)]
  
  contratos[contratos$uid == i, 'valor_total'] <- 
    valor_in[which(uids == i)]
}


## 5.2. Limpieza de registros erroneos de V. Inicial y V. Total ----
### Nota: Se observaron los registros que presentaban un valor mayor
### a Dosmilmillones

uids <- c("20-13-10638419-9780455", "20-12-10800790-9936496",
          "20-22-15375-9866611", "20-12-10682530-9802374",
          "20-12-10763177-9890039", "20-12-10842692-9978787", 
          "20-4-10848593-9984820", "CO1.REQ.1241024",
          "CO1.REQ.1278446", "CO1.REQ.1278446", "CO1.REQ.1252914",
          "20-12-10739009-9888535", "CO1.REQ.1224992", "CO1.REQ.1615047",
          '21-4-11479484-10619711'
)

valor_est <- c(17966000, 57310400, 
               869203468, 8788150,
               6292475, 781250000, 
               0, 245174195,
               215905000, 324062500,
               3504919, 884000,
               1449420, 28528587,
               251810788
)

valor_ini <- c(17966000, 57310400, 
               869203468, 8788150,
               6292475, 781250000, 
               0, 245174195,
               215905000, 324062500,
               3504919, 884000,
               1449420, 28528587,
               251810788
)

valor_adi <- c(0, 0, 
               0, 0,
               0, 0, 
               0, 0,
               0, 0,
               0, 0,
               0, 0,
               0
)

for (i in uids) {
  contratos[contratos$uid == i, 'valor_estimado'] <- 
    valor_est[which(uids == i)]
  
  contratos[contratos$uid == i, 'valor_inicial'] <- 
    valor_ini[which(uids == i)]
  
  contratos[contratos$uid == i, 'valor_adiciones'] <- 
    valor_adi[which(uids == i)]
  
  contratos[contratos$uid == i, 'valor_total'] <- 
    (valor_ini[which(uids == i)] + valor_adi[which(uids == i)])
}

# 5.3. Limpieza personalizada ----
contratos <- contratos %>% 
  mutate(regimen_contratacion = case_when(
    regimen_contratacion == "Ley 80 de 1993" ~ 
      "Estatuto General de Contratación",
    TRUE ~ regimen_contratacion
  ))

# 5.4. Eliminacion de contratos repetidos ----
## Contrato con detalle diferente pero documento igual entre
## 20-22-18916-10274172 y 20-22-18135-10206063
contratos <- contratos %>% 
  filter(!(uid == "20-22-18916-10274172"))

# 6. Seleccion de variables ----
contratos <- contratos %>% select(-all_of(c('uid', 'detalle_objeto',
                                            'fecha_adjudicacion'))) 


# 7. Reescribir la base de datos ----
write_csv(x = contratos, file = paste0(direccion, "contratos_covid19_LV1.csv"))