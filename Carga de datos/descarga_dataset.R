##  Descarga de conjunto de datos SECOP I y SECOP II: Procesos
##  por medio de socrata
##  Fecha: 15/07/2020

# 1. Librerias ----
sapply(
  c('RSocrata', 'dplyr', 'readr'), 
  library, character.only = T
)

# 2. Funcion de extraccion de datos ----
extraccion_datos <- function(enlace, lim = 20000){
  
  # Credenciales
  correo <- "****"
  contrasenna <- "****"
  
  # Ajuste del limite en la consulta para no descargar
  # conjuntos muy grandes
  enlace <- paste0(enlace, "&$limit=", lim)
  
  # Ejecucion
  datos <- read.socrata(url = enlace,
                        email = correo, 
                        password = contrasenna)
  
  # Alerta sobre limite de consulta
  if (nrow(datos) == lim)
    print(paste0("La consulta alcanzo el limite ",
      "permitido de registros. Revise si es ",
      "necesario realizar otro filtro o aumentar el limite."))
  
  return(datos)
  
}

# 3.  Conjunto de datos SECOP I ----

# Ajuste de parametros

# Nombre del atributo a cual se le realiza el filtro
col <- "detalle_del_objeto_a_contratar"

cols <- c("uid", "nivel_entidad", "orden_entidad",
          "nombre_de_la_entidad", "nit_de_la_entidad",
          "tipo_de_proceso", "estado_del_proceso",
          "regimen_de_contratacion", "objeto_a_contratar",
          "detalle_del_objeto_a_contratar", "tipo_de_contrato",
          "municipios_ejecucion", "cuantia_proceso",
          "nombre_grupo", "nombre_familia", "nombre_clase",
          "tipo_identifi_del_contratista", 
          "identificacion_del_contratista", 
          "nom_raz_social_contratista", "dpto_y_muni_contratista",
          "fecha_de_firma_del_contrato", "cuantia_contrato",
          "valor_total_de_adiciones", "valor_contrato_con_adiciones",
          "ruta_proceso_en_secop_i",
          "municipio_entidad", "departamento_entidad",
          "tiempo_adiciones_en_dias", "tiempo_adiciones_en_meses")

cols_selection <- cols[1]

for (i in 2:length(cols)) {
  cols_selection <- paste(cols_selection, cols[i], sep = ",")
}

# 3.1. Consulta ----
enlace <- paste0(
  "https://www.datos.gov.co/resource/xvdy-vvsk.csv", #Enlace 
  "?$select=", cols_selection, # Seleccion de columnas
  "&$where=(UPPER(", col, ") like '%25PANDEMIA%25'",
  " OR UPPER(", col, ") like '%25EMERGENCIA SANITARIA%25'",
  " OR UPPER(", col, ") like '%25EMERGENCIA ECONÓMICA%25'",
  " OR UPPER(", col, ") like '%25EMERGENCIA ECONOMICA%25'",
  " OR UPPER(", col, ") like '%25CONTINGENCIA%25'",
  " OR UPPER(", col, ") like '%25CUARENTENA%25'",
  " OR UPPER(", col, ") like '%25CORONAVIRUS%25'",
  " OR UPPER(", col, ") like '%25COVID%25')")

# 3.2. Llamar a la funcion ----
contratos_SI <- extraccion_datos(enlace = enlace, lim = 65000)

# 3.3. Escritura del conjunto de datos ----
direccion_comp <- paste0('Datasets_complementos/')

contratos_SI <- contratos_SI |> distinct()

write_csv(x = contratos_SI, 
          file = paste0(direccion_comp, "contratos_covid19_SI.csv"))

# 4. Conjunto de datos SECOP II: Procesos ----
col <- "descripci_n_del_procedimiento"

cols <- c("id_del_proceso", "ordenentidad", "codigo_pci",
          "entidad", "nit_entidad","nombre_de_la_unidad_de" ,
          "modalidad_de_contratacion", "estado_del_procedimiento",
          "nombre_del_procedimiento", 
          "descripci_n_del_procedimiento", "tipo_de_contrato",
          "ciudad_de_la_unidad_de", "precio_base", 
          "codigo_principal_de_categoria",
          "nit_del_proveedor_adjudicado", "nombre_del_proveedor",
          "departamento_proveedor", "ciudad_proveedor",
          "fecha_de_publicacion_del", "fecha_adjudicacion",
          "valor_total_adjudicacion", "urlproceso", "ciudad_entidad",
          "departamento_entidad", "proveedores_invitados", 
          "proveedores_con_invitacion", "proveedores_unicos_con")

cols_selection <- cols[1]

for (i in 2:length(cols)) {
  cols_selection <- paste(cols_selection, cols[i], sep = ",")
}

# 4.1. Consulta ----
enlace <- paste0(
  "https://www.datos.gov.co/resource/p6dx-8zbt.csv", #Enlace
  "?$select=", cols_selection, # Seleccion de columnas
  "&$where=UPPER(", col, ") like '%25PANDEMIA%25'",
  " OR UPPER(", col, ") like '%25EMERGENCIA SANITARIA%25'",
  " OR UPPER(", col, ") like '%25EMERGENCIA ECONÓMICA%25'",
  " OR UPPER(", col, ") like '%25EMERGENCIA ECONOMICA%25'",
  " OR UPPER(", col, ") like '%25CONTINGENCIA%25'",
  " OR UPPER(", col, ") like '%25CUARENTENA%25'",
  " OR UPPER(", col, ") like '%25CORONAVIRUS%25'",
  " OR UPPER(", col, ") like '%25COVID%25'")

# 4.2. Llamar a la funcion ----
contratos_SII <- extraccion_datos(enlace = enlace, lim = 15000)

# 4.3. Escritura del conjunto de datos ----
contratos_SII <- contratos_SII |> distinct()

write_csv(x = contratos_SII, 
          file = paste0(direccion_comp, "contratos_covid19_SII.csv"))
