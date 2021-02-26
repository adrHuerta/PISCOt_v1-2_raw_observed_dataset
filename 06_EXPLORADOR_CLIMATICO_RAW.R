rm(list = ls()); `%>%` = magrittr::`%>%`

# same order in both folders
chile_tmax <- dir("./raw/EXPLORADOR_CLIMATICO/CHILE/TMAX", full.names = TRUE)
chile_tmin <- dir("./raw/EXPLORADOR_CLIMATICO/CHILE/TMIN", full.names = TRUE)

mapply(function(xmax, ymin){
  
  file_name = strsplit(xmax, " ") %>% sapply(function(y) y[4])
  file_lat = strsplit(xmax, " ") %>% sapply(function(y) strsplit(y[11], "º")[[1]][1])
  file_lon = strsplit(xmax, " ") %>% sapply(function(y) strsplit(y[12], "º")[[1]][1])
  file_alt = strsplit(xmax, " ") %>% sapply(function(y) strsplit(y[13], "º")[[1]][1])
  #file_alt
  
  # creating xyz data
  xyz_max = data.frame(ID = paste("CH", formatC(nchar(file_name) + abs(as.numeric(file_alt)), 5, flag = 0), sep = ""),
                       NAM = toupper(file_name),
                       LON = -as.numeric(file_lon),
                       LAT = -as.numeric(file_lat),
                       ALT = as.numeric(file_alt),
                       SRC = "EXPLORADOR_CLIMATICO")
  
  
  file_name = strsplit(ymin, " ") %>% sapply(function(y) y[4])
  file_lat = strsplit(ymin, " ") %>% sapply(function(y) strsplit(y[11], "º")[[1]][1])
  file_lon = strsplit(ymin, " ") %>% sapply(function(y) strsplit(y[12], "º")[[1]][1])
  file_alt = strsplit(ymin, " ") %>% sapply(function(y) strsplit(y[13], "º")[[1]][1])

  xyz_min = data.frame(ID = paste("CH", formatC(nchar(file_name) + abs(as.numeric(file_alt)), 5, flag = 0), sep = ""),
                       NAM = toupper(file_name),
                       LON = -as.numeric(file_lon),
                       LAT = -as.numeric(file_lat),
                       ALT = as.numeric(file_alt),
                       SRC = "EXPLORADOR_CLIMATICO")
  
  # are the same? yes!, they should be the same
  xyz = if( identical(xyz_max, xyz_min) ) xyz_max
  #xyz
  
  xmax = read.csv(xmax, header = TRUE)[, 1:4]
  colnames(xmax) = c("y", "m", "d", "tx")
  
  ymin = read.csv(ymin, header = TRUE)[, 1:4]
  colnames(ymin) = c("y", "m", "d", "tn")
  
  rclimdex_data = merge(xmax, ymin, by = c("y", "m", "d"), all = TRUE) # all = TRUE to preserve all the data
  rclimdex_data["prec"] = NA
  
  rownames(xyz) = NULL
  
  # saving as a list xyz, rclimdex data and path for saving
  list(xyz = xyz,
       data_format = rclimdex_data[, c("y", "m", "d", "prec", "tx", "tn")],
       patch = file.path(".", "processed", "EXPLORADOR_CLIMATICO", "rclimdex_format", 
                         paste("CH", formatC(nchar(file_name) + abs(as.numeric(file_alt)), 5, flag = 0), ".txt", sep = "")))
  
  },
  x = chile_tmax,
  y = chile_tmin,
  SIMPLIFY = FALSE) -> resultados_data_chile

# same order in both folders # similar to the previous one
bolivia_tmax <- dir("./raw/EXPLORADOR_CLIMATICO/BOLIVIA/TMAX", full.names = TRUE)
bolivia_tmin <- dir("./raw/EXPLORADOR_CLIMATICO/BOLIVIA/TMIN", full.names = TRUE)

mapply(function(xmax, ymin){
  
  file_name = strsplit(xmax, " ") %>% sapply(function(y) y[4])
  file_lat = strsplit(xmax, " ") %>% sapply(function(y) strsplit(y[11], "º")[[1]][1])
  file_lon = strsplit(xmax, " ") %>% sapply(function(y) strsplit(y[12], "º")[[1]][1])
  file_alt = strsplit(xmax, " ") %>% sapply(function(y) strsplit(y[13], "º")[[1]][1])
  #file_alt
  
  xyz_max = data.frame(ID = paste("BO", formatC(nchar(file_name) + abs(as.numeric(file_alt)), 5, flag = 0), sep = ""),
                       NAM = toupper(file_name),
                       LON = -as.numeric(file_lon),
                       LAT = -as.numeric(file_lat),
                       ALT = as.numeric(file_alt),
                       SRC = "EXPLORADOR_CLIMATICO")
  
  
  file_name = strsplit(ymin, " ") %>% sapply(function(y) y[4])
  file_lat = strsplit(ymin, " ") %>% sapply(function(y) strsplit(y[11], "º")[[1]][1])
  file_lon = strsplit(ymin, " ") %>% sapply(function(y) strsplit(y[12], "º")[[1]][1])
  file_alt = strsplit(ymin, " ") %>% sapply(function(y) strsplit(y[13], "º")[[1]][1])
  
  xyz_min = data.frame(ID = paste("BO", formatC(nchar(file_name) + abs(as.numeric(file_alt)), 5, flag = 0), sep = ""),
                       NAM = toupper(file_name),
                       LON = -as.numeric(file_lon),
                       LAT = -as.numeric(file_lat),
                       ALT = as.numeric(file_alt),
                       SRC = "EXPLORADOR_CLIMATICO")
  
  xyz = if( identical(xyz_max, xyz_min) ) xyz_max
  #xyz
  
  xmax = read.csv(xmax, header = TRUE)[, 1:4]
  colnames(xmax) = c("y", "m", "d", "tx")
  
  ymin = read.csv(ymin, header = TRUE)[, 1:4]
  colnames(ymin) = c("y", "m", "d", "tn")
  
  rclimdex_data = merge(xmax, ymin, by = c("y", "m", "d"), all = TRUE) # all = TRUE to preserve all the data
  rclimdex_data["prec"] = NA
  
  rownames(xyz) = NULL
  
  list(xyz = xyz,
       data_format = rclimdex_data[, c("y", "m", "d", "prec", "tx", "tn")],
       patch = file.path(".", "processed", "EXPLORADOR_CLIMATICO", "rclimdex_format", paste("BO", formatC(nchar(file_name) + abs(as.numeric(file_alt)), 5, flag = 0), ".txt", sep = "")))
  
},
x = bolivia_tmax,
y = bolivia_tmin,
SIMPLIFY = FALSE) -> resultados_data_bolivia


resultados_data <- c(resultados_data_chile, resultados_data_bolivia)

xyz <- lapply(resultados_data, function(x) x$xyz) %>% do.call(rbind, .)
rownames(xyz) = NULL
xyz <- xyz[xyz$LON < -67.5, ]

xyz %>%
write.table(.,
            file.path(".", "processed", "EXPLORADOR_CLIMATICO", "xyz.txt"),
            sep = " ",
            quote = F,
            row.names = F)


resultados_data[as.numeric(rownames(xyz))] %>%
  lapply(function(x){
    
    write.table(x$data_format,
                x$patch,
                sep = " ",
                quote = F,
                row.names = F, col.names = F)
    
  })
