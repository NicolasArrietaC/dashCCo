# Creacion de indicadores de riesgo de corrupcion en contratacion
# publica durante la emergencia sanitaria por el Covid 19
# Fecha de elaboracion: 29/10/2020

# 1. Librerias ----
library(tidyverse)

# 2. Carga de conjunto de datos ----
contratos <- read_csv(file = "Datasets/contratos_covid19_LV1.csv", 
                      locale = locale(encoding = "UTF-8"))

# 3. Filtros ----
# Quitar contratos de prestación de servicios por valor total
contratos <- contratos %>% filter(valor_total >= 1e6)

# 4. Calculo de Indicadores ----

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

## Seleccionar Municipios y demás distintos en la BD ----
indicadoresF <- contratos %>% 
  group_by(nombre_grupo, nombre_familia) %>%
  summarise(contratos = n()) %>% 
  filter(contratos >= 5) %>% 
  select(nombre_grupo, nombre_familia)

# 4.1. Indicadores para la Falta de Competencia ----

# 4.1.1. Promedio de oferentes por proceso de selección ----
temp <- contratos %>% 
  filter(fuente == "SECOP II") %>%
  group_by(nombre_familia) %>% 
  summarise(oferentes_prom = sum(proveedores_inv, na.rm = T) / n()) %>%
  mutate(promedio_max = max(oferentes_prom)) %>% 
  mutate(ind_prom_oferentes = (1 - (oferentes_prom / promedio_max)) * 100) %>%
  select(nombre_familia, oferentes_prom, ind_prom_oferentes)

## Unir con la tabla de indicadores
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 4.1.2. Porcentaje de procedimientos que utilizaron adjudicación... ----
# directa o regimen especial

# Calcula el porcentaje de contratación en numero de contratos
temp <- contratos %>% 
  group_by(nombre_familia) %>% 
  summarise(perc_contr_cerrada_num = 
              sum((tipo_proceso == "Contratación Directa" | 
                     tipo_proceso == "Régimen Especial") / n(),
                  na.rm = T) * 100)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia") %>% 
  mutate(perc_contr_cerrada_num = ifelse(is.na(perc_contr_cerrada_num), 0, 
                                         perc_contr_cerrada_num))
rm(temp) ## Remover temp

# 4.1.3. Porcentaje del valor de procedimientos que utilizaron... ----
# adjudicación directa o regimen especial     

# Calcula el porcentaje de contratación con respecto al valor 
# de los contratos
temp <- contratos %>% 
  group_by(nombre_familia) %>% 
  filter(tipo_proceso == "Contratación Directa" |
           tipo_proceso == "Régimen Especial") %>%
  summarise(valor_cerrada = sum(valor_total, na.rm = T))

temp1 <- contratos %>% 
  group_by(nombre_familia) %>% 
  summarise(valor_total = sum(valor_total, na.rm = T))

temp <- temp %>% 
  left_join(y = temp1, by = "nombre_familia") 

rm(temp1) ## Remover temp1

temp <- temp %>% 
  mutate(perc_contr_cerrada_val = (valor_cerrada / valor_total) * 100) %>% 
  select(nombre_familia, perc_contr_cerrada_val)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")%>% 
  mutate(perc_contr_cerrada_val = ifelse(is.na(perc_contr_cerrada_val), 0,
                                         perc_contr_cerrada_val))
rm(temp) ## Remover temp

# 4.1.4. y 4.1.5. Índice de concentración de contratos HHI ----

temp <- contratos %>% 
  group_by(nombre_familia, id_contratista) %>% 
  summarise(cant_contratos = n(),
            val_contratos = sum(valor_total, na.rm = T)) %>% 
  group_by(nombre_familia) %>% 
  mutate(total_contratos_ent = sum(cant_contratos, na.rm = T),
         total_val_contratos_ent = sum(val_contratos, na.rm = T),
         si_cant = (cant_contratos / total_contratos_ent) * 100,
         si2_cant = si_cant ^ 2,
         si_val = (val_contratos / total_val_contratos_ent) * 100,
         si2_val = si_val ^ 2) %>% 
  group_by(nombre_familia) %>% 
  summarise(HHI_cant = (sum(si2_cant, na.rm = T) / 10000) * 100,
            HHI_val = (sum(si2_val, na.rm = T) / 10000) * 100)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 4.1.6. y 4.1.7. Índice de diversidad - ID ----
#El Índice no se puede aplicar a entidades con 1 solo contrato
familias_del <- contratos %>% 
  group_by(nombre_familia) %>% 
  summarise(contratos = n()) %>% 
  filter(contratos < 2)

# Quitar las entidades con menos de 2 contratos
contratos1 <- contratos %>% 
  filter(!(nombre_familia %in% familias_del$nombre_familia))

temp <- contratos1 %>% 
  group_by(nombre_familia, id_contratista) %>% 
  summarise(nj_cant = n(),
            nj_val = sum(valor_total, na.rm = T),
            sub_cant = nj_cant * (nj_cant - 1),
            sub_val = nj_val * (nj_val - 1), na.rm = T) %>% 
  group_by(nombre_familia) %>% 
  summarise(N_cant = sum(nj_cant, na.rm = T),
            N_val = sum(nj_val, na.rm = T),
            ID_cant = (sum(sub_cant, na.rm = T) / 
                         (N_cant * (N_cant - 1)) * 100),
            ID_val = (sum(sub_val, na.rm = T) / 
                        (N_val*(N_val - 1))) * 100) %>% 
  select(nombre_familia, ID_cant, ID_val)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 4.1.8. Índice de participación de oferentes ---- 

temp <- contratos %>% 
  filter(fuente == "SECOP II") %>%
  group_by(nombre_familia) %>% 
  summarise(ofertas_prom = sum(proveedores_unicos,  na.rm = T) / n()) %>%
  mutate(promedio_max = max(ofertas_prom)) %>% 
  mutate(ind_prom_ofertas = (1 - (ofertas_prom / promedio_max)) * 100) %>% 
  select(nombre_familia, ofertas_prom, ind_prom_ofertas)


indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 4.1.9. Número de empresas ganadoras diferentes por cada 100 contratos ----

# Calcular el indicador de diversidad
temp <- contratos %>% 
  group_by(nombre_familia) %>% 
  summarise(n_contratistas_dif = n_distinct(id_contratista),
            n_contratos = n(),
            ganadoras = (n_contratistas_dif / n_contratos) * 100) %>% 
  mutate(ganadoras = 100 - ganadoras) %>% 
  select(nombre_familia, ganadoras)  

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 4.1.10. y 4.1.11. Índice de concentración de las cuatro empresas...----
# con mayor numero y valor de contratos - IC4k

# Índice para el numero de contratos
temp <- contratos1 %>% 
  group_by(nombre_familia, id_contratista) %>% 
  summarise(nj = n()) %>% 
  group_by(nombre_familia) %>% 
  mutate(Ni = sum(nj, na.rm = T),
         Pi = nj / Ni) %>% 
  slice_max(n = 4, order_by = nj) %>% 
  summarise(IC4K_cant = sum(Pi, na.rm = T) * 100)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# Valor 

# Índice para el valor de los contratos
temp <- contratos1 %>% 
  group_by(nombre_familia, id_contratista) %>% 
  summarise(nj = sum(valor_total, na.rm = T)) %>% 
  group_by(nombre_familia) %>% 
  mutate(Ni = sum(nj, na.rm = T),
         Pi = nj / Ni) %>% 
  slice_max(n = 4, order_by = nj) %>% 
  summarise(IC4K_val = sum(Pi, na.rm = T) * 100)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp); rm(familias_del); rm(contratos1) ## Remover temp

# 4.2. Indicadores de Violaciones o anomalías en los procesos de compra ----
# 4.2.12. Porcentaje de modificaciones a los contratos en tiempo ----

temp <- contratos %>%
  filter(fuente == "SECOP I") %>%
  mutate(adiciones_dias = as.integer(adiciones_dias)) %>% 
  group_by(nombre_familia) %>%
  summarise(perc_tiempo_adiciones = sum((adiciones_dias >= 1), na.rm = T) / 
              n() * 100)

# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 4.2.13. Porcentaje de modificaciones a los contratos en valor ----
temp <- contratos %>%
  group_by(nombre_familia) %>%
  filter(valor_inicial > 0) %>% 
  summarise(perc_valor_adiciones = mean((valor_adiciones / valor_inicial),
                                        na.rm = T) * 100) %>% 
  mutate(max_valor_adiciones= max(perc_valor_adiciones),
         ind_valor_adiciones = (perc_valor_adiciones / 
                                  max_valor_adiciones) * 100) %>% 
  select(nombre_familia, perc_valor_adiciones, ind_valor_adiciones)


# Agrupar los indicadores en una sola tabla
indicadoresF <- indicadoresF %>% 
  left_join(y = temp, by = "nombre_familia")

rm(temp) ## Remover temp

# 5. Índice de Riesgo de Corrupción ----

#El Índice de corrupción para cada entidad se calcula como la media 
# aritmética de los Índices de corrupción estimados anteriormente. 
# Esto se hace basado en el estudio de Zuleta & Caro (2019).

ind <- c("ind_prom_oferentes", "perc_contr_cerrada_num", 
         "perc_contr_cerrada_val", "HHI_cant", "HHI_val", "ID_cant",
         "ind_prom_ofertas", "ganadoras", "IC4K_cant",              
         "IC4K_val", "perc_tiempo_adiciones", "perc_valor_adiciones", 
         "ind_valor_adiciones")

indicadoresF$ind_riesgo_corrupcion <-  apply(indicadoresF[ , ind], 
                                       1, mean, na.rm = T)

#Observar Comportamiento
#indicadoresF %>% 
#  pivot_longer(cols =  c(oferentes_prom:ind_riesgo_corrupcion), 
#               names_to = "indicador",
#               values_to = "valor") %>% 
#  ggplot(aes(x = valor)) +
#  geom_density() +
#  facet_wrap(vars(indicador), ncol = 3, scales = "free")

## Agrupar por Municipio

indicadoresG <- indicadoresF %>% 
  group_by(nombre_grupo) %>%
  summarise(oferentes_prom = mean(oferentes_prom, na.rm = T), 
            ind_prom_oferentes = mean(ind_prom_oferentes, na.rm = T), 
            perc_contr_cerrada_num = mean(perc_contr_cerrada_num, na.rm = T), 
            perc_contr_cerrada_val = mean(perc_contr_cerrada_val, na.rm = T), 
            HHI_cant = mean(HHI_cant, na.rm = T), 
            HHI_val = mean(HHI_val, na.rm = T), 
            ID_cant = mean(ID_cant, na.rm = T), 
            ID_val = mean(ID_val, na.rm = T), 
            ofertas_prom = mean(ofertas_prom, na.rm = T), 
            ind_prom_ofertas = mean(ind_prom_ofertas, na.rm = T), 
            ganadoras = mean(ganadoras, na.rm = T), 
            IC4K_cant = mean(IC4K_cant, na.rm = T), 
            IC4K_val = mean(IC4K_val, na.rm = T), 
            perc_tiempo_adiciones = mean(perc_tiempo_adiciones, na.rm = T), 
            perc_valor_adiciones = mean(perc_valor_adiciones, na.rm = T),
            ind_valor_adiciones = mean(ind_valor_adiciones, na.rm = T), 
            ind_riesgo_corrupcion = mean(ind_riesgo_corrupcion, na.rm = T))

ind <- c("ind_prom_oferentes", "perc_contr_cerrada_num", 
         "perc_contr_cerrada_val", "HHI_cant", "HHI_val", "ID_cant",               
         "ID_val", "ind_prom_ofertas", "ganadoras", "IC4K_cant",             
         "IC4K_val", "perc_tiempo_adiciones", "ind_valor_adiciones", 
         "ind_riesgo_corrupcion","oferentes_prom", "ofertas_prom", 
         "perc_valor_adiciones")

#Pasar las columnas de indicadores a filas
indicadoresF <- indicadoresF %>%  
  pivot_longer(cols = ind, names_to = "indicador", values_to = "valor")

#Eliminar los NA
indicadoresF <- indicadoresF %>%  filter(!is.na(valor))

# 6. Escritura de datos ----
## Indicadores General
write_csv(x = indicadoresF, path = "Datasets/IRC_indicadoresF.csv")
## Indicadores por Municipio
write_csv(x = indicadoresG, path = "Datasets/IRC_indicadoresG.csv")