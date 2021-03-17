library(highcharter)
library(widgetframe)
library(dplyr)
colors_grade <- c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                  E = "#D54C29")
my_own_theme <- structure(list(chart = list(backgroundColor = NULL), caption = list(
  style = list(fontSize = "10px"))), class = "hc_theme")


data <- data.table::fread("../data/data_brand.csv", encoding = "UTF-8")
data = data[grep(paste0("France", collapse = "|"),
          data$countries_fr),] 


traitement <- function(x, id){
  if(id =="nutriscore_grade"){
    id_removed = grep("nova_group", colnames(x))
  }else{
    id_removed = grep("nutriscore_grade", colnames(x))
  }
  sub_data<- reshape2::melt(x[,-id_removed],id = id,na.rm = TRUE)
  sub_data <- sub_data[sub_data$value!="",]
  grep(":", sub_data$value, value = TRUE)
  gsub("(^ *)","",gsub("\\w*:","",grep(":", sub_data$value, value = TRUE)))
  sub_data$brands = gsub(" +"," ",gsub("(^ *)|( +$)","",gsub("fr:","",sub_data$value)))
  
  sub_data[,c(id,"brands")]
}

data <- data.frame(nutriscore_grade=data$nutriscore_grade,
                   nova_group=data$nova_group,
                   stringr::str_split_fixed(data[["brands"]], ",",10))
sub_data_nutri <- traitement(data, "nutriscore_grade")
sub_data_nova <- traitement(data, "nova_group")

brand_grade <- sub_data_nutri %>% 
  mutate(brands = tolower(brands)) %>%
  count(brands,`nutriscore_grade`) %>% 
  arrange(-n) %>% group_by(brands) %>% 
  mutate(tot = sum(n)) %>% 
  ungroup() %>% mutate(prop = n/tot*100,
                       id = tolower(gsub(" ","_", brands)),
                       nutriscore_grade = toupper(nutriscore_grade))
ent_retenues <- brand_grade %>% arrange(-tot) %>% distinct(brands,tot) %>% head(15)
brand_grade_t15 <- brand_grade %>% 
  filter(brands %in% ent_retenues$brands)

brand_grade_nova <- sub_data_nova %>% 
  mutate(brands = tolower(brands)) %>% 
  filter(!is.na(nova_group),brands!="sans marque") %>% 
  count(brands,`nova_group`) %>% 
  arrange(-n) %>% group_by(brands) %>% 
  mutate(tot = sum(n)) %>% 
  ungroup() %>% mutate(prop = n/tot*100,
                       id = tolower(gsub(" ","_", brands)),
                       nova_group = toupper(nova_group))
ent_retenues_nova <- brand_grade_nova %>% arrange(-tot) %>% distinct(brands,tot) %>% head(15)
brand_grade_t15_nova <- brand_grade_nova %>% 
  filter(brands %in% ent_retenues_nova$brands)
dput(unique(c(ent_retenues$brands,ent_retenues_nova$brands)))

brands_cor <- c("Auchan" = "auchan", "Carrefour"= "carrefour", "U"="u", "Marque Repère"="marque repère",
                "Casino"="casino", "Leader Price"="leader price", 
                "Cora"="cora", "Monoprix"="monoprix", "Picard"= "picard", "Nestlé"="nestlé", 
                "Belle France" = "belle france", "Netto"="netto", 
                "Franprix"="franprix", "Le Gaulois"="le gaulois", "La Vie Claire"="la vie claire",
                "Fleury Michon" = "fleury michon", "Danone" = "danone",
                "Sans marque"="sans marque")
for(nom_cor in names(brands_cor)){
  brand_grade_t15$brands = gsub(paste0("(^",
                                 brands_cor[nom_cor],
                                 "$)", collapse = "|"),
                          nom_cor,brand_grade_t15$brands)
  brand_grade_t15_nova$brands = gsub(paste0("(^",
                                       brands_cor[nom_cor],
                                       "$)", collapse = "|"),
                                nom_cor,brand_grade_t15_nova$brands)
}

saveRDS(brand_grade_t15, file = "content/homepage/data/top15_brands.RDS")
saveRDS(brand_grade_t15_nova, file = "content/homepage/data/top15_brands_nova.RDS")

c("la vie claire", "sans marque"
)

list("Carrefour" = "carrefou")

