rm(list = ls()); `%>%` = magrittr::`%>%` 

# getting path files
path_0 = "./raw/SENAMHI/ESTACIONES OPERATIVAS/NACIONAL/SET 335 ESTACIONES CON VOZ Y DATA"
files_in_path_0 = dir(path = path_0, full.names = T, pattern = ".txt")

# getting path files
path_1 = "./raw/SENAMHI/ESTACIONES OPERATIVAS/NACIONAL/SET 371 ESTACIONES QC"
files_in_path_1 = dir(path = path_1, full.names = T, pattern = ".txt")

# as some stations are in the one folder but not in the other, it is selected for a simple addition
files_in_1_but_no_in_0 <- which(match(strsplit(files_in_path_1, "/") %>% sapply(function(x) unlist(x)[7]),
                                      strsplit(files_in_path_0, "/") %>% sapply(function(x) unlist(x)[7])) %in% NA)

# merging same stations in both folders and the unique ones
# creating a single list of path for both folder
c(files_in_path_0,
  files_in_path_1[files_in_1_but_no_in_0]) %>%
  data.frame(link = ., 
             name_file = strsplit(., "/") %>% sapply(function(x) unlist(x)[7])) %>%
  .[order(.$name_file),] %>%
  .$link %>%
  as.character() -> files_in_path

# getting names of files stations of files .txt
files_in_path_names = files_in_path %>% sapply(function(x) strsplit(x, "/") %>% unlist() %>% .[7] %>% substr(., 1, 6)) %>% as.vector()

# reading list of stations 
xyz = readxl::read_xlsx("/home/adrian/Documents/Repos/PISCOt_raw_observed_dataset/raw/SENAMHI/ESTACIONES OPERATIVAS/lista_archivos_actu.V2.xlsx",
                        "REGIONES ALT") %>%
  transform(NOMBRE = gsub(" ", "_", NOMBRE)) %>%
  .[c("CODIGO_ANT", "CODIGO_NUEV", "NOMBRE", "LONGITUD", "LATITUD", "ALTITUD")] %>%
  .[order(.$CODIGO_ANT), ]

# this should be TRUE, the same number (name) stations in xslx should be in the folder (both + unique files)   
all(files_in_path_names == xyz$CODIGO_ANT)

# as is TRUE, reading data and writing in new folder with NEW CODE
# in rclimdex format
for(i in 1:length(files_in_path_names)){
  
  new_name = xyz$CODIGO_NUEV[i]
  df_data = read.table(files_in_path[i], header = F, sep = " ")
  
  write.table(df_data,
              file = file.path(".","processed", "SENAMHI" ,"operativas", paste(new_name, ".txt", sep = "")),
              sep = " ",
              quote = F,
              row.names = F,
              col.names = F)
  
  }

# subsetting list (XYZ), only needed information 
colnames(xyz) = c("ID_old", "ID", "NAME", "LON", "LAT", "ALT")
xyz %>%
  .[, c("ID", "NAME", "LON", "LAT", "ALT")] %>%
  transform(PATH = file.path(".","processed", "SENAMHI", "operativas", paste(ID, ".txt", sep = ""))) %>%
  write.table(file.path(".","processed", "SENAMHI", "xyz_operativas.txt"),
              sep = " ",
              quote = F,
              row.names = F)
