library(tidyverse)
library(readxl)

soil_locations <- readxl::read_xlsx("../../../Downloads/Soil Sample Locations.xlsx")

paths_to_field_coords <- data.table::fread(skip = 1,"../../../Downloads/Soil Sample Locations.xlsx - Sheet2.csv") %>%
  select(1:2) %>%
  set_names(c("path","field"))



soil_2023 <- readxl::read_xlsx(
  "../../../Downloads/pH Soil Data/2023/consolidated data sheet.xlsx")

paths <- c(2016:2019,2021:2023) %>%
  purrr::map_chr(~paste0("../../../Downloads/pH Soil Data/",.x))

#printable_2018 <- readxl::read_xlsx("../../../Downloads/pH Soil Data/2018/Printable spread sheet samples.xlsx")

files <- list.files(paths,full.names = TRUE,pattern = ".xls$")


read_soil_data <- function(files){
  
  field <- readxl::read_xls(
    path = files,
    range = "P2",
    col_names = FALSE
  ) %>%
    as.character()
  
group_1_names <-  
  readxl::read_xls(
    path = files,
    range = "B7:V7",
    col_names = FALSE
  ) %>%
  as.character()

group1_units <-  
  readxl::read_xls(
    path = files,
    range = "B8:V8",
    col_names = FALSE
  ) %>%
  as.character()

group_1_data <- 
  data <- read_xls(
    path = files,
    range = "B10:V12",
    col_names = FALSE
  )

colnames(group_1_data) <- map2_chr(group_1_names,group1_units,~paste0(.x," (",.y,")"))

group_2_names <-  
  readxl::read_xls(
    path = files,
    range = "B19:V19",
    col_names = FALSE
  ) %>%
  as.character()

group2_units <-  
  readxl::read_xls(
    path = files,
    range = "B20:V20",
    col_names = FALSE
  ) %>%
  as.character()

group_2_data <- 
  data <- read_xls(
    path = files,
    range = "B22:V24",
    col_names = FALSE
  )

colnames(group_2_data) <- map2_chr(group_2_names,group2_units,~paste0(.x," (",.y,")"))

left_join(group_1_data,group_2_data, by=("Depth (inches)")) %>%
mutate(name=field)
        
}


soil_2016 <- files[1:65, ] %>% map_dfr(~read_soil_data(.x) %>% mutate(path=.x))

soil_2016 <- soil_2016  %>%
  mutate(year="2016")

soil_2017 <- files[66:106, ] %>% 
  map_dfr(~read_soil_data(.x) %>% 
            mutate(path=.x))

soil_2017 <- soil_2017 %>%
  mutate(year="2017")


setdiff(names(soil_2017),names(soil_2016))
setdiff(y=names(soil_2017),names(soil_2016))
# field <- files %>%
#     mutate(
#       field = purrr::map_chr(
#         full_paths,
#         ~ readxl::read_xls(.x, range = "P2", col_names = FALSE) %>% 
#           pull(1)))
soil_2018<- files[105:127, ] %>% 
  map_dfr(~read_soil_data(.x) %>% 
            mutate(path=.x))

soil_2018 <- soil_2018 %>%
  mutate(year="2018")


soil_2019 <- files[129:153, ] %>% 
  map_dfr(~read_soil_data(.x) %>% 
            mutate(path=.x))

soil_2019 <- soil_2019 %>%
  mutate(year="2019")

soil_2021<- files[154:186, ] %>% 
  map_dfr(~read_soil_data(.x) %>% 
            mutate(path=.x))

soil_2021 <- soil_2021 %>%
  mutate(year="2021")

soil_2022 <- files[188:201, ] %>% 
  map_dfr(~read_soil_data(.x) %>% 
            mutate(path=.x))

soil_2022 <- soil_2022 %>%
  mutate(year="2022")


soil_2023 <- files[202:236, ] %>% 
  map_dfr(~read_soil_data(.x) %>% 
            mutate(path=.x))

soil_2023 <- soil_2023 %>%
  mutate(year="2023")

all_soil <- soil_2016 %>%
  bind_rows(soil_2017) %>%
  bind_rows(soil_2018) %>%
  bind_rows(soil_2019) %>%
  bind_rows(soil_2021) %>%
  bind_rows(soil_2022) %>%
  bind_rows(soil_2023)


all_soil_final <- all_soil %>%
  left_join(paths_to_field_coords,by=c("path")) %>%
  left_join(soil_locations %>% rename(field=`Field #`),by="field")

all_soil_final %>%
  filter(is.na(Longitude)) %>%
  distinct(path,field) %>%
  data.frame()

save(all_soil_final, file = "all_soil_chemistry_final.RData")

load("data/soil_data.RData")
load("data/soil_data2.RData")

soil_data2 <- soil_data %>%
  mutate(`Al (KCl) (ppm)` = dplyr::coalesce(`Al     (KCl) (ppm)`,`Al         (KCl) (ppm)`),
         `Al. (DTPA) (ppm)`= dplyr::coalesce(`Al  (DTPA) (ppm)`,`Al    (DTPA) (ppm)`)) %>%
  select(-c(`Al         (KCl) (ppm)`,`Al     (KCl) (ppm)`,`Al    (DTPA) (ppm)`)) %>%
  rename(`K (Bicarb) (meq/100g)`=`K        (Bicarb) (meq/100g)`) %>%
  rename(`P ( Bray) (ppm) `=`P              ( Bray) (ppm)`) %>%
  select(-c(`Al  (DTPA) (ppm)`,`NA (NA).y`,`NA (NA).x`,`P     (Morgan) (ppm)`,`K (Morgan) (ppm)`)) %>%
  rename(`Al (DTPA) (ppm)`=`Al. (DTPA) (ppm)`,`Lime (%)`=`%Lime (%)`)%>%
  mutate(year=ifelse(is.na(year) & str_detect(path,'2018'),"2018",year))


names(soil_data2) <- gsub("\\s{2,}"," ",names(soil_data2))

Hmisc::contents(soil_data2)

save(soil_data2,file = "data/soil_data2.RData")


