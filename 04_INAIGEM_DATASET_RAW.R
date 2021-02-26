rm(list = ls()); `%>%` = magrittr::`%>%`

# raw files 
dir_raw_files = file.path(".","raw", "INAIGEM", "Datos_nivel_0") %>%
  dir(., full.names = T) %>%
  .[-length(.)] # deleting SENAMHI file ("SENAMHI_1981-2019.xls")

lapply(dir_raw_files,
       function(x){
         
         stations <- readxl::excel_sheets(x)

         for(z in 1:length(stations)){
         
            stations_data = readxl::read_xlsx(path = x, sheet = stations[z]) %>%
              .[, c("Fecha", "Rain", "Tmax", "Tmin")]
            
            if(any(duplicated(format(stations_data$Fecha, "%Y-%m-%d"))) == TRUE){ # at sub-daily scale
              
              tx = xts::xts(stations_data$Tmax %>% as.numeric(), stations_data$Fecha) %>%
                # .[xts::.indexhour(.) %in% c(19)] %>% # to get tmax as in a conventional station 19h
                xts::apply.daily(max, na.rm = F) %>% # to get tmax as the maximum value in a day
                round(., 1)
              
              zoo::index(tx) <- as.Date(format(zoo::index(tx), "%Y-%m-%d"))
              
              tn = xts::xts(stations_data$Tmin %>% as.numeric(), stations_data$Fecha)  %>%
                # .[xts::.indexhour(.) %in% c(7)] %>% # to get tmin as in a conventional station 7h
                xts::apply.daily(min, na.rm = F) %>% # to get tmin as the minimum value in a day
                round(., 1)
              
              zoo::index(tn) <- as.Date(format(zoo::index(tn), "%Y-%m-%d"))
              
              pp = xts::xts(stations_data$Rain %>% as.numeric(), stations_data$Fecha) %>% 
                lag(-8) %>% # to accumulate according to conventional station time (7pm-7am)
                xts::apply.daily(sum, na.rm = F) %>% 
                round(., 1)
              
              zoo::index(pp) <- as.Date(format(zoo::index(pp), "%Y-%m-%d"))
              
            } else {
              
              tn = xts::xts(stations_data$Tmin %>% as.numeric(), stations_data$Fecha) %>%
                round(., 1)
              zoo::index(tn) <- as.Date(format(zoo::index(tn), "%Y-%m-%d"))
              
              tx = xts::xts(stations_data$Tmax %>% as.numeric(), stations_data$Fecha)  %>%
                round(., 1)
              zoo::index(tx) <- as.Date(format(zoo::index(tx), "%Y-%m-%d"))
              
              pp = xts::xts(stations_data$Rain %>% as.numeric(), stations_data$Fecha)  %>%
                round(., 1)
              zoo::index(pp) <- as.Date(format(zoo::index(pp), "%Y-%m-%d"))
              
            }
            
            stations_data <- cbind(pp, tx, tn) # rclimdex format (Y, m, d, pp, tx, tn)
            data.frame(zoo::index(stations_data) %>% format("%Y") %>% as.numeric(),
                       zoo::index(stations_data) %>% format("%m") %>% as.numeric(),
                       zoo::index(stations_data) %>% format("%d") %>% as.numeric(),
                       stations_data$pp %>% zoo::coredata(),
                       stations_data$tx %>% zoo::coredata(),
                       stations_data$tn %>% zoo::coredata()) -> stations_data
            colnames(stations_data) <- NULL
            return(stations_data)
            # stations_data %>%
            # write.table(file.path(".", "processed", "INAIGEM", "rclimdex_format", paste(stations[z], ".txt", sep = "")),
            #             sep = " ",
            #             quote = F,
            #             row.names = F)
  
            
            }
         
       }) -> adr


stations = file.path(".", "processed", "INAIGEM", "rclimdex_format") %>% 
  dir() %>%
  gsub(".txt", "", .)

xyz = file.path(".", "raw", "INAIGEM", "COD_EM_INAIGEM.xlsx") %>%
  readxl::read_xlsx() %>%
  subset(FUENTE != "SENAMHI") %>%
  .[, c(1, 2, 4, 6, 7, 8, 5)] %>%
  setNames(c("ID", "COD", "NAME", "LON", "LAT", "ALT", "TYP"))

xyz = xyz[match(stations, xyz$COD),]

xyz$LON = -((xyz$LON %>% strsplit(., "°") %>% sapply(function(x) as.numeric(x[[1]]))) +
  (xyz$LON %>% strsplit(., "'") %>% sapply(function(x) x[[1]]) %>% strsplit(., "°") %>% sapply(function(x) as.numeric(x[[2]])))/60 + 
  (xyz$LON %>% strsplit(., "'") %>% sapply(function(x) x[[2]]) %>% strsplit(., "/|") %>% sapply(function(x) paste0(x[-length(x)], collapse = '') %>% as.numeric()))/3600)

xyz$LAT = -((xyz$LAT %>% strsplit(., "°") %>% sapply(function(x) as.numeric(x[[1]]))) +
              (xyz$LAT %>% strsplit(., "'") %>% sapply(function(x) x[[1]]) %>% strsplit(., "°") %>% sapply(function(x) as.numeric(x[[2]])))/60 + 
              (xyz$LAT %>% strsplit(., "'") %>% sapply(function(x) x[[2]]) %>% strsplit(., "/|") %>% sapply(function(x) paste0(x[-length(x)], collapse = '') %>% as.numeric()))/3600)


xyz$ID = 900000 + xyz$ID

xyz %>%
  transform(TYP = gsub(" ", "_", TYP),
            NAME = gsub(" ", "_", NAME),
            COD = file.path(".","processed","INAIGEM","rclimdex_format", paste(COD, ".txt", sep = ""))) -> xyz

for(i in 1:length(xyz$COD)){
  
  station = xyz$COD[i]
  Name_file = xyz$ID[i] 
  read.table(station, 
             sep = " ", header = F) %>%
    write.table(file.path(".", "processed", "INAIGEM", "rclimdex_format2", paste(Name_file, ".txt", sep = "")),
                sep = " ",
                quote = F,
                row.names = F, col.names = F)
  
  
}

xyz %>%
  .[,-2] %>%
write.table(.,
            file.path(".", "processed", "INAIGEM", "xyz.txt"),
              sep = " ",
              quote = F,
              row.names = F)
