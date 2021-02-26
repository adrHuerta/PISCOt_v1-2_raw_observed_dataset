rm(list = ls()); `%>%` = magrittr::`%>%`

# reading xyz data
xyz <- readxl::read_xlsx("./raw/INAMHI/datos_ecuador.xlsx", sheet = "coordenadas",
                  range = cellranger::cell_cols("H:L")) %>%
  .[-1, ] %>%
  setNames(c("ID0", "NAM", "LAT", "LON", "ALT")) %>%
  dplyr::mutate(SRC = "INAMHI",
                NAM = gsub(" ", "_", NAM),
                ID = formatC(gsub("M", "", ID0) %>% as.numeric(), 5, flag = 0),
                PATH = file.path(".", "raw", "INAMHI", "diarios_ecuador", 
                                 paste("ho", 
                                       formatC(gsub("M", "", ID0) %>% as.numeric(), 7, flag = 0),
                                       ".txt",
                                       sep = "")))
# saving only needed columns
xyz$ID = paste("EC", xyz$ID, sep = "")
xyz = xyz[-11, ]
xyz %>%
  dplyr::select(c(ID, NAM, LON, LAT, ALT, SRC)) %>%
  write.table(.,
              file.path(".", "processed", "INAMHI", "xyz.txt"),
              sep = " ",
              quote = F,
              row.names = F)

# saving data as rclimdex format
for(j in 1:dim(xyz)[1]){
  
  read.table(xyz[j, "PATH"] %>% unlist, header = FALSE) %>%
    write.table(.,
                file.path(".", "processed", "INAMHI", "rclimdex_format", paste(xyz[j, "ID"] %>% unlist, ".txt", sep = "")),
                sep = " ",
                quote = F,
                row.names = F, col.names = F)
  
}
