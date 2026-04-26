#====================================================#
# Proyecto: Heatmap sistemas de secreción            #
# Autor: Enrique Isidro Coxca                        #                
# Fecha: 17/02/2026                                  #                                         
#====================================================#

# Limpieza inicial de consola ----

rm(list = ls())


# Condicional de existencia de pacman ----

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado")
} else{
  install.packages("pacman", dependencies = T)
}

# Llamado e instalación de paquetes ----

pacman::p_load(
  "tidyverse",
  "readxl",
  "openxlsx",
  "tidyplots"
)

# Leer los datos ----

Data <- read.xlsx("Resultados_procesados.xlsx")

Data <- Data %>%
  rename("Sistema de secreción" = "Sistema.de.secreción")


# Dejar solo sistemas de secreción en el dataframe ----

Data_filtrado <- Data %>%
  filter(!`Sistema de secreción` %in% c("Tad", "T4bP", "T4aP", "MSH", "Flagelo", "CONJ"))

# Renombrar sistemas a español ----

Data_filtrado <- Data_filtrado %>%
  mutate(`Sistema de secreción` = recode(`Sistema de secreción`,
                                         "T1SS" = "SST1",
                                         "T2SS" = "SST2",
                                         "T3SS" = "SST3",
                                         "pT4SSi" = "pSST4i",
                                         "pT4SSt" = "pSST4t",
                                         "T4SS_tipoB" = "SST4_tipoB",
                                         "T4SS_tipoC" = "SST4_tipoC",
                                         "T4SS_tipoF" = "SST4_tipoF",
                                         "T4SS_tipoG" = "SST4_tipoG",
                                         "T4SS_tipoI" = "SST4_tipoI",
                                         "T4SS_tipoT" = "SST4_tipoT",
                                         "T5aSS" = "SST5a",
                                         "T5bSS" = "SST5b",
                                         "T5cSS" = "SST5c",
                                         "T6SSi" = "SST6i",
                                         "T6SSii" = "SST6ii",
                                         "T6SSiii" = "SST6iii",
                                         "T9SS" = "SST9"
  ))

# Ordenar ----

Data_filtrado$`Sistema de secreción` <- factor(
  Data_filtrado$`Sistema de secreción`,
  levels = c("SST1","SST2","SST3","pSST4i","pSST4t","SST4_tipoB","SST4_tipoC","SST4_tipoF"
             ,"SST4_tipoG","SST4_tipoI","SST4_tipoT","SST5a","SST5b","SST5c",
             "SST6i","SST6ii","SST6iii","SST9")
)



# Heatmap con tidyplots ----

Data_filtrado |> 
  tidyplot(x = `Sistema de secreción`, y = Cepa, color = Cantidad) |> 
  add_heatmap() |>
  adjust_size(height = 220, width = 120) |>
  adjust_font(fontsize = 5, face = "bold") |> 
  adjust_colors(new_colors = colors_continuous_rocket)|> 
  save_plot("heatmap_TSS.pdf")

# Tabla de suma de cantidad de sistemas ---- 

tabla_resumen <- Data_filtrado %>%
  group_by(`Sistema de secreción`) %>%
  summarise(Total = sum(Cantidad, na.rm = TRUE))

