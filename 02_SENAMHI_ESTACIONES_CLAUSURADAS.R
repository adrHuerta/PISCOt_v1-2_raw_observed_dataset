rm(list = ls()); `%>%` = magrittr::`%>%`

# getting path files
path = "./raw_2021/SENAMHI/ESTACIONES CLAUSURADAS/1.CON QC/Datos con QC-FINALES/"
files_in_path = dir(path = path, full.names = T)

# reading data based on path (all in a single object)
observed_data = lapply(files_in_path, 
                       function(x){
                         df = readxl::read_xlsx(x)
                         colnames(df) = c("COD", "ESTACION", "COD_A", "AÑO", "MES", "DIA", "TMAX", "TMIN", "PP")
                         return(df)
                       }) %>%
  do.call(rbind, .)

# saving data in rclimdex format (using new code)
by(observed_data, 
   observed_data$COD,  
   function(x){
     
     write.table(x[, c("AÑO", "MES", "DIA", "PP", "TMAX", "TMIN")],
                 file = file.path(".","processed", "SENAMHI", "clausuradas", paste(x$COD[1], ".txt", sep = "")),
                 sep = " ",
                 quote = F,
                 row.names = F,
                 col.names = F)
     })

# reading xyz data and subseting only needed columns
xyz = readxl::read_xlsx("./raw_2021/SENAMHI/ESTACIONES CLAUSURADAS/TABLA_140_LON_LAT.xlsx") %>%
  .[c("COD_NUEVO", "V_NOM_ESTA", "N_LON_SIG", "N_LAT_SIG", "N_ALT_MTS")] %>%
  transform(V_NOM_ESTA = gsub(" ", "_", V_NOM_ESTA))

# writing xyz data
colnames(xyz) = c("ID", "NAME", "LON", "LAT", "ALT")
xyz %>%
  transform(PATH = file.path(".","processed", "SENAMHI", "clausuradas", paste(ID, ".txt", sep = ""))) %>%
  write.table(file.path(".","processed", "SENAMHI","xyz_clausuradas.txt"),
            sep = " ",
            quote = F,
            row.names = F)
