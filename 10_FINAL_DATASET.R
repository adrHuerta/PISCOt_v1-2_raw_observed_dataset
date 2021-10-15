rm(list = ls()); `%>%` = magrittr::`%>%`

all_files1 = file.path(".","processed", "SENAMHI", "xyz_operativas_clausuradas.txt") %>%
  read.table(sep = " ", header = T) %>%
  transform(LINK = file.path(".","processed", "SENAMHI", "operativas_clausuradas_updated", paste(.$ID, ".txt", sep = "")))
  
# all_files2 = file.path(".","processed", "INAIGEM", "xyz.txt") %>%
#   read.table(sep = " ", header = T) %>%
#   transform(LINK = file.path(".","processed", "INAIGEM", "rclimdex_format2", paste(.$ID, ".txt", sep = "")))

all_files2 = file.path(".","processed", "INAMHI", "xyz.txt") %>%
  read.table(sep = " ", header = T) %>%
  transform(LINK = file.path(".","processed", "INAMHI", "rclimdex_format", paste(.$ID, ".txt", sep = "")))

all_files3 = file.path(".","processed", "EXPLORADOR_CLIMATICO", "xyz.txt") %>%
  read.table(sep = " ", header = T) %>%
  transform(LINK = file.path(".","processed", "EXPLORADOR_CLIMATICO", "rclimdex_format", paste(.$ID, ".txt", sep = "")))

all_files4 = file.path(".","processed", "INMET", "xyz.txt") %>%
  read.table(sep = " ", header = T) %>%
  transform(LINK = file.path(".","processed", "INMET", "rclimdex_format", paste(.$ID, ".txt", sep = "")))

all_files5 = file.path(".","processed", "IDEAM", "xyz.txt") %>%
  read.table(sep = " ", header = T) %>%
  transform(LINK = file.path(".","processed", "IDEAM", "rclimdex_format", paste(.$ID, ".txt", sep = "")))

# merging all XYZs  
all_files = rbind(all_files1, all_files2, all_files3, all_files4, all_files5)
rownames(all_files) <- NULL

# identifying files that does not have tx neither tn
all_files$LINK %>% 
  as.character() %>%
  setNames(., all_files$ID) %>%
  lapply(., function(x){
    
    exp = read.table(x, sep = " ")
    exp[exp == -999] = NA
    exp[exp == -888] = NA
    exp[exp == -99.9] = NA
    exp[exp == -88.8] = NA
    
    if( all(is.na(exp$V6)) & all(is.na(exp$V5))){
      return(1)
    } else {
      return(0)
    }
  }) %>%
  unlist() %>% 
  .[. != 0] -> to_not_use


# deleting rclimdex files that dont have tmax neither tmin
all_files_0 = all_files[-match(names(to_not_use), as.character(all_files$ID)), ]

# NEW 2021: DELETING FILES: PE105091, PE107092, PE107058 (PRACTICALLY 0 DATA)
del_new_files <- match(c("PE105091", "PE107092", "PE107058"), as.character(all_files_0$ID))
all_files_0 <- all_files_0[-del_new_files,]
row.names(all_files_0) <- NULL

# saving XYZ 
all_files_0 %>%
  .[,-7] %>%
  .[match(stringr::str_sort(.$ID), .$ID), ] %>%
  write.table(file.path(".","final_dataset", "xyz.txt"),
              sep = " ",
              quote = F,
              row.names = F)

# XYZ order should be the same in folder

# saving data  
all_files_0$LINK %>%
  as.character() %>%
  lapply(., function(x){
    
    x_name = strsplit(x, "/") %>% unlist()
    x_name = x_name[length(x_name)]
    
      read.table(x, 
                 header = F, 
                 sep = " ") %>%
      write.table(file = file.path(".", "final_dataset", "data", x_name),
                  sep = " ",
                  quote = F,
                  row.names = F,
                  col.names = F)
    })

# data size
dim(all_files_0)
