#==============================================================#
# Proyecto: Búsqueda de sistemas de secreción con "TXSScan"    #
# Autor: Enrique Isidro Coxca                                  #
# Fecha: 17/02/2026                                            #
#==============================================================#


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

Data <- read.xlsx("cepas.xlsx")

# Invertir para que ZT1 quede arriba de la col ----

Data$Cepa <- rev(Data$Cepa)

# Arreglar resultados de TXSscan ----

tss <- read.csv("Resultados.csv",
                header = FALSE,
                stringsAsFactors = FALSE)

# Agregar índice ----

tss$row_id <- seq_len(nrow(tss))

# Separar filas impares (valores) y pares (sistemas) ----

valores <- tss %>%
  filter(row_id %% 2 == 1) %>%
  select(-row_id)

sistemas <- tss %>%
  filter(row_id %% 2 == 0) %>%
  select(-row_id)

# Convertir a formato largo las columnas reales ----

valores_long <- valores %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -ID,
               names_to = "col",
               values_to = "valor")

sistemas_long <- sistemas %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -ID,
               names_to = "col",
               values_to = "sistema")

# Unir correctamente ----

final <- left_join(valores_long, sistemas_long,
                   by = c("ID","col"))

head(final)

final <- final %>%
  select(-col)

# Renombrar columna valor ----

final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/Flagellum", "Flagelo", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T2SS", "T2SS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4aP", "T4aP", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T5aSS", "T5aSS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T5bSS", "T5bSS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T5cSS", "T5cSS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/Tad", "Tad", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/pT4SSt", "pT4SSt", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/CONJ", "CONJ", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/MSH", "MSH", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T1SS", "T1SS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T3SS", "T3SS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeB", "T4SS_tipoB", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeC", "T4SS_tipoC", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeF", "T4SS_tipoF", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeG", "T4SS_tipoG", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeI", "T4SS_tipoI", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeT", "T4SS_tipoT", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4bP", "T4bP", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T6SSi", "T6SSi", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T6SSii", "T6SSii", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T6SSiii", "T6SSiii", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T9SS", "T9SS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/pT4SSi", "pT4SSi", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeG", "T4SS_tipoG", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeI", "T4SS_tipoI", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4SS_typeT", "T4SS_tipoT", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T4bP", "T4bP", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T6SSi", "T6SSi", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T6SSii", "T6SSii", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T6SSiii", "T6SSiii", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/T9SS", "T9SS", final$valor)
final$valor <- gsub("TXSScan-1.1.0/bacteria/diderm/pT4SSi", "pT4SSi", final$valor)

final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/Flagellum", "Flagelo", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T2SS", "T2SS", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T4aP", "T4aP", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T5aSS", "T5aSS", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T5bSS", "T5bSS", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T5cSS", "T5cSS", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/Tad", "Tad", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/pT4SSt", "pT4SSt", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/CONJ", "CONJ", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/MSH", "MSH", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T1SS", "T1SS", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T3SS", "T3SS", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T4SS_typeB", "T4SS_tipoB", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T4SS_typeC", "T4SS_tipoC", final$valor)
final$valor <- gsub("TXSScanAusente1.1.0/bacteria/diderm/T4SS_typeF", "T4SS_tipoF", final$valor)

final$valor <- gsub("T4SS_typeB", "T4SS_tipoB", final$valor)
final$valor <- gsub("T4SS_typeC", "T4SS_tipoC", final$valor)
final$valor <- gsub("T4SS_typeF", "T4SS_tipoF", final$valor)

# Asiganr ID al dataframe Data ----

Data$ID <- seq_len(nrow(Data))

# Fusionar las columnas de final + data (cepas) ----

final <- final %>%
  left_join(Data %>% select(ID, Cepa), by = "ID")

# Renombrar columnas ----

final <- final %>%
  rename(Cantidad = sistema)

final$Cantidad <- as.numeric(final$Cantidad)

#Pasar cepa hasta la izquierda del dataframe ----

final <- final %>%
  select(Cepa, everything())

# Exportar a xlsx ----

write.xlsx(final, "Resultados_procesados.xlsx")


