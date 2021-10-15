rm(list = ls()); `%>%` = magrittr::`%>%`

# raw files reading 

"./raw_2021/INMET" %>%
  dir(full.names = T) %>%
  .[1:5] %>%
  lapply(., function(x){
    read.table(x, skip = 0, nrows = 9, header = F, sep = ":") -> res
    
    data.frame(ID = paste("BR", formatC(as.numeric(sub(".", "", res$V2[2])), 5, flag = 0), sep = ""),
               NAM = gsub(" ", "_", sub(".", "", res$V2[1])),
               LON = sub(".", "", res$V2[4]),
               LAT = sub(".", "", res$V2[3]),
               ALT = sub(".", "", res$V2[5]),
               SRC = "INMET")
    
  }) %>% 
  do.call(rbind, .) -> xyz_data

xyz_data %>%
  write.table(.,
              file.path(".", "processed", "INMET", "xyz.txt"),
              sep = " ",
              quote = F,
              row.names = F)

# same order XYZ and files in folder!
  
"./raw_2021/INMET" %>%
  dir(full.names = T) %>%
  .[1:5] %>%
  lapply(., function(x){
    
    read.table(x, skip = 10, header = T, sep = ";") %>%
      .[, c(1:3)] -> res
    
    res$Data = as.Date(res$Data.Medicao, "%Y-%m-%d")

    # res_tx = subset(res, Hora != 1200) %>% .[, -c(2,4)]
    res_tx = res %>% .[, -c(1,3)]
    # res_tn = subset(res, Hora == 1200) %>% .[, -c(2,3)]
    res_tn = res %>% .[, -c(1,2)]
    
    merge(res_tx, res_tn, by = "Data", all = TRUE) %>% # all = TRUE to preserve all the data
      transform(y = format(.$Data, "%Y"),
                m = format(.$Data, "%m"),
                d = format(.$Data, "%d"),
                pre = NA) %>%
      .[, c("y", "m", "d", "pre", "TEMPERATURA.MAXIMA..DIARIA..C.", "TEMPERATURA.MINIMA..DIARIA..C.")]
    
  }) -> data_xyz

for(i in 1:length(xyz_data$ID)){
  
  data_xyz[[i]]  %>%
    write.table(file.path(".", "processed", "INMET", "rclimdex_format", paste(xyz_data$ID[i], ".txt", sep = "")),
                sep = " ",
                quote = F,
                row.names = F, col.names = F)
    
  
}

