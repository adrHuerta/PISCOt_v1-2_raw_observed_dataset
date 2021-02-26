rm(list = ls()); `%>%` = magrittr::`%>%`

# reading xyz data
# preserving only needed data
xyz <- readxl::read_xlsx("./raw/IDEAM/INFORMACION/coordenadas.xlsx", 
                         sheet = "TMIN_TOTAL") %>%
  .[426:428, c("NOMBRE", "LONGITUD", "LATITUD", "ALTITUD", "TRABAJO")] %>%
  setNames(c("NAM", "LON", "LAT", "ALT", "SRC")) %>%
  dplyr::mutate(ID = NA,
                NAM = gsub(" ", "_", NAM)) %>%
  dplyr::select(ID, NAM, LON, LAT, ALT, SRC)
xyz
# IDs in order (according to file)
xyz$ID <- c(44015060, 48015050, 47075010)
# ACUEDUCTO_MOCOA, AEROPUERTO_VASQUEZ_COBO, LA_CHORRERA

# path of files
tn_data <- dir("./raw/IDEAM/DATA", full.names = TRUE)[1:3]
tx_data <- dir("./raw/IDEAM/DATA", full.names = TRUE)[4:6]

# reading data and converting to rclimdex format, everthing as a list
mapply(function(x, y){
  
  stat_tx = strsplit(strsplit(x, "@")[[1]][2], "[.]")[[1]][1]
  stat_tn = strsplit(strsplit(y, "@")[[1]][2], "[.]")[[1]][1]
  
  if(identical(stat_tn, stat_tx)){
  
    tx_data <- read.table(x, skip = 15, header = F, sep = ",")
    tx_data <- data.frame(year = as.numeric(substr(tx_data$V2, 1, 4)),
                          month = as.numeric(substr(tx_data$V2, 6, 7)),
                          day = as.numeric(substr(tx_data$V2, 9, 10)),
                          tx = tx_data[, 3])
    
    tn_data <- read.table(y, skip = 15, header = F, sep = ",")
    tn_data <- data.frame(year = as.numeric(substr(tn_data$V2, 1, 4)),
                          month = as.numeric(substr(tn_data$V2, 6, 7)),
                          day = as.numeric(substr(tn_data$V2, 9, 10)),
                          tn = tn_data[, 3])
    
    df_data <- merge(tx_data, tn_data, by = c("year", "month", "day"), 
                     all = TRUE) # all = TRUE to preserve all the data
      
  }
  
  
  
  list(data = data.frame(df_data[, 1:3], pp = NA, df_data[, 4:5]),
       name = paste("CO", stat_tn, sep = ""))
  
  }, x = tx_data, y = tn_data,
  SIMPLIFY = FALSE) -> resultados_data

# saving data
lapply(resultados_data, function(x){
  
  write.table(x$data,
              file.path(".", "processed", "IDEAM", "rclimdex_format", paste(x$name, ".txt", sep = "")),
              sep = " ",
              quote = F,
              row.names = F, col.names = F)
  
})

# saving XYZ adding "ID" from country
xyz$ID <- paste("CO", xyz$ID, sep = "")
xyz %>%
  write.table(.,
              file.path(".", "processed", "IDEAM", "xyz.txt"),
              sep = " ",
              quote = F,
              row.names = F)
