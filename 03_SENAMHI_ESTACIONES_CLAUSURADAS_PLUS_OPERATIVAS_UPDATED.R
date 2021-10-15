rm(list = ls()); `%>%` = magrittr::`%>%`

# all the processed data (operativas and clausuradas) may have
# 1) same station or
# 2) is not updated to at least 2019
# thus, this is done here

# reading path of proccesed data
oper = read.table(file.path(".", "processed", "SENAMHI", "xyz_operativas.txt"), sep = " ", header = T)
clau = read.table(file.path(".", "processed", "SENAMHI", "xyz_clausuradas.txt"), sep = " ", header = T)

# is there any CLAU in OPER? ( 1) same station data )
oper[match(clau$ID, oper$ID) %>% .[!is.na(.)],]
clau[match(oper$ID, clau$ID)%>% .[!is.na(.)], ]

# yes, eliminating in CLAU (it is preserved CLAU data because may OPER has more data)
clau = clau[-match(oper$ID, clau$ID)%>% .[!is.na(.)], ]

# preserving all unique stations 
rbind(oper,
      clau) %>%
  .[order(.$ID), ] -> xyz_all


# reading data to update to 2020 ( 2) updating data ) from VOZ Y DATA (Kris data)
path_3 = "raw_2021/SENAMHI/ESTACIONES OPERATIVAS/TO_UPDATE_2020/DATOS DE PP"

# reading data for each variable: 
c("2018", "2019", "2020") %>%
  lapply(function(x){
    exp = file.path(path_3, x) %>%
      dir(full.names = T) 
  }) %>% unlist() %>%
  lapply(function(x){
    
    tx = readxl::read_xlsx(x, 2, skip = 6) # SHEET 2: TMAX
    tx_exp = data.frame(tx[, c("COD", "AÑO", "MES")], tx[, 14:dim(tx)[2]])
    reshape2::melt(tx_exp, 
                   id = c("COD", "AÑO", "MES"), 
                   variable.name = "DIA",
                   value.name = "value") -> res
    res$value = as.numeric(res$value)
    res$VAR = rep("2_tx")
    return(res)
    
  }) %>% 
  do.call(rbind, .) %>%
  .[order(.$COD), ] -> tx_data

c("2018", "2019", "2020") %>%
  lapply(function(x){
    exp = file.path(path_3, x) %>%
      dir(full.names = T) 
  }) %>% unlist() %>%
  lapply(function(x){
    
    tn = readxl::read_xlsx(x, 3, skip = 6) # SHEET 3: TMIN
    tn_exp = data.frame(tn[, c("COD", "AÑO", "MES")], tn[, 14:dim(tn)[2]])
    reshape2::melt(tn_exp, 
                   id = c("COD", "AÑO", "MES"), 
                   variable.name = "DIA",
                   value.name = "value") -> res
    res$value = as.numeric(res$value)
    res$VAR = "3_tn"
    return(res)
    
  }) %>% 
  do.call(rbind, .) %>%
  .[order(.$COD), ] -> tn_data

c("2018", "2019","2020") %>%
  lapply(function(x){
    exp = file.path(path_3, x) %>%
      dir(full.names = T) 
  }) %>% unlist() %>%
  lapply(function(x){
    
    tn = readxl::read_xlsx(x, 2, skip = 6) # SHEET 1: PP
    tn_exp = data.frame(tn[, c("COD", "AÑO", "MES")], tn[, 14:dim(tn)[2]])
    reshape2::melt(tn_exp, 
                   id = c("COD", "AÑO", "MES"), 
                   variable.name = "DIA",
                   value.name = "value") -> res
    res$value = as.numeric(res$value)
    res$VAR = "1_pp"
    return(res)
    
  }) %>% 
  do.call(rbind, .) %>%
  .[order(.$COD), ] -> pp_data

# merging all data
rbind(pp_data, 
      tx_data,
      tn_data) -> all


# match(unique(tx_data$COD), 
#       as.character(xyz_all$ID)) %>%
#   .[!is.na(.)] %>%
#   xyz_all$ID[.] -> tx_adr
# 
# match(unique(tn_data$COD), 
#       as.character(xyz_all$ID)) %>%
#   .[!is.na(.)] %>%
#   xyz_all$ID[.] -> tn_adr
# 
# match(unique(pp_data$COD), 
#       as.character(xyz_all$ID)) %>%
#   .[!is.na(.)] %>%
#   xyz_all$ID[.] -> pp_adr

# from tidy to rclimdex format
reshape2::dcast(all,
                COD + AÑO + MES + DIA ~ VAR, value.var = "value", mean) -> pp_tx_tn
pp_tx_tn = pp_tx_tn[-as.numeric(rownames(pp_tx_tn[is.na(pp_tx_tn$COD),])), ]
pp_tx_tn = pp_tx_tn[order(pp_tx_tn$COD), ]

pp_tx_tn$DIA = as.numeric(gsub("X", "", pp_tx_tn$DIA))
pp_tx_tn$AÑO = as.numeric(pp_tx_tn$AÑO)
pp_tx_tn$MES = as.numeric(pp_tx_tn$MES)

pp_tx_tn$`1_pp`[is.na(pp_tx_tn$`1_pp`)] = -99.9
pp_tx_tn$`1_pp`[is.nan(pp_tx_tn$`1_pp`)] = -99.9

pp_tx_tn$`2_tx`[is.na(pp_tx_tn$`2_tx`)] = -99.9
pp_tx_tn$`2_tx`[is.nan(pp_tx_tn$`2_tx`)] = -99.9

pp_tx_tn$`3_tn`[is.na(pp_tx_tn$`3_tn`)] = -99.9
pp_tx_tn$`3_tn`[is.nan(pp_tx_tn$`3_tn`)] = -99.9

# important part!: adding "updated data" to processed data based on condition
# 1st Condition: is station code in "updated data"?, if is TRUE, continue. if is FALSE is just saved (no needed for update)
# 2st Condition response TRUE: is last time in station (old_df) in new_df (updated data)?, 
# a) if is TRUE: search for that range (last time(old_df):last time(new_df) in new_df) and merged with old_df
# b) if is FALSE: new_df is the range, and is just merged to the old_df
# In a), is the normal behaviour, because it is supposed that last time in old_df is in new_df
# In b), is due to there are some stations that has been closed and have missing data for a long period that has not been updated, 
# but is now re-open (that why is found data in new_df - 1st condition is TRUE)
# In a) and b) the data is saved as rclimdex format


for(i in 1:dim(xyz_all)[1]){
  
  station = xyz_all$ID[i]
  
  if(isTRUE(as.character(station) %in% pp_tx_tn$COD)){
    
    old_df = read.table(as.character(xyz_all$PATH[i]), header = F, sep = " ")
    new_df = pp_tx_tn[pp_tx_tn$COD %in% as.character(station), c("AÑO", "MES", "DIA", "1_pp", "2_tx", "3_tn")]
    
    subset(new_df, AÑO == old_df$V1[length(old_df$V1)] & MES == old_df$V2[length(old_df$V2)] & DIA == old_df$V3[length(old_df$V3)]) %>%
      rownames() -> rw_number
    
    match(rw_number, rownames(new_df)) %>%
      as.numeric() -> r_number 
    
    if(isTRUE(length(r_number) > 0)){
      
      df_to_add <- new_df[(r_number + 1):dim(new_df)[1], ]
      colnames(df_to_add) <- colnames(old_df)
      
      rbind(old_df, 
            df_to_add) %>%
        write.table(file = file.path(".", "processed", "SENAMHI", "operativas_clausuradas_updated", paste("PE", station, ".txt", sep = "")),
                    sep = " ",
                    quote = F,
                    row.names = F,
                    col.names = F)
      } else {
        
        df_to_add <- new_df
        colnames(df_to_add) <- colnames(old_df)
        
        rbind(old_df,
              df_to_add) %>%
          write.table(file = file.path(".","processed", "SENAMHI", "operativas_clausuradas_updated", paste("PE", station, ".txt", sep = "")),
                    sep = " ",
                    quote = F,
                    row.names = F,
                    col.names = F)
    }
    
  } else {
    
    read.table(as.character(xyz_all$PATH[i]), header = F, sep = " ") %>%
      write.table(file = file.path(".","processed", "SENAMHI", "operativas_clausuradas_updated", paste("PE", station, ".txt", sep = "")),
                  sep = " ",
                  quote = F,
                  row.names = F,
                  col.names = F)
               
    
  }
    
}

# saving xyz data using a new code (same new code + "ID" from country)
# Updating with new XYZ (given by Nataly)

xyz_all <- xyz_all %>%
  .[, c("ID", "NAME", "LON", "LAT", "ALT")] %>%
  setNames(c("ID", "NAM", "LON", "LAT", "ALT"))

rownames(xyz_all) <- NULL

# 2019
xyz_senamhi_2020 <- readxl::read_xlsx("raw_2021/SENAMHI/LISTA_DE_ESTACIONES/Lista de Observadores.xlsx",
                                      "Hoja1") %>%
  subset(CATEGORIA == "CONVENCIONAL") %>%
  transform(CODIGO = as.numeric(CODIGO)) %>%
  .[order(.$CODIGO),] %>%
  .[, c("CODIGO", "NOMBRE_ESTACION", "LONGITUD", "LATITUD", "ALTITUD")]

# 2014
# xyz_senamhi_2020 <- readxl::read_xlsx("/home/adrian/Documents/Repos/PISCOt_raw_observed_dataset/raw/SENAMHI/LISTA_DE_ESTACIONES/MAESTRA ESTACIONES.xlsx",
#                                       "Hoja1") %>%
#   subset(V_DESL_CATE != "METEOROLOGICA AUTOMATICA" &
#            V_DESL_CATE != "AGROMETEOROLOGICA AUTOMATICA" &
#            V_DESL_CATE != "HIDROLOGICA AUTOMATICA" &
#            V_DESL_CATE != "OCEANOGRAFICA AUTOMATICA" &
#            V_DESL_CATE != "AMBIENTAL AUTOMATICA" &
#            V_DESL_CATE != "HIDROMETEOROLOGICA AUTOMATICA") %>%
#   transform(V_COD_ESTA = as.numeric(V_COD_ESTA)) %>%
#   .[, c("V_COD_ESTA", "V_NOM_ESTA", "N_LON_SIG", "N_LAT_SIG", "N_ALT_MTS")] %>%
#   setNames(c("CODIGO", "NOMBRE_ESTACION", "LONGITUD", "LATITUD", "ALTITUD"))

xyz_all_after_senamhi_2020 <- xyz_all

for(n_station in 1:dim(xyz_all_after_senamhi_2020)[1]){
  
  id_station_xyz <- xyz_all_after_senamhi_2020$ID[n_station]
  id_data <- subset(xyz_senamhi_2020, CODIGO == id_station_xyz)
  
  if(dim(id_data)[1] > 0){
    
    xyz_all_after_senamhi_2020[match(id_station_xyz, xyz_all_after_senamhi_2020$ID), c("LON", "LAT", "ALT")] = 
      id_data[, c("LONGITUD", "LATITUD", "ALTITUD")]
    
  } else {
    
    next
  }
  
}

# according to DECADE dataset Tambopata has this information
xyz_all_after_senamhi_2020[match("TAMBOPATA", xyz_all_after_senamhi_2020$NAM), c("LON", "LAT", "ALT")] <- c(-69.152222, -14.220000, 1264)

#
plot(xyz_all_after_senamhi_2020$ID, xyz_all$ID) # no change
plot(xyz_all_after_senamhi_2020$LON, xyz_all$LON) 
plot(xyz_all_after_senamhi_2020$LAT, xyz_all$LAT)
plot(xyz_all_after_senamhi_2020$ALT, xyz_all$ALT) # more change in ALT data!

xyz_all_after_senamhi_2020 %>%
  transform(SRC = "SENAMHI",
            ID = paste("PE", ID, sep = "")) %>%
  write.table(file.path(".","processed", "SENAMHI", "xyz_operativas_clausuradas.txt"),
              sep = " ",
              quote = F,
              row.names = F)
