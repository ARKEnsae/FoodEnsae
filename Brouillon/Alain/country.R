library(ggplot2)
library(dplyr)
library(highcharter)

# donnees <- data.table::fread("data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
# colnames(donnees)
# data <- donnees[!is.na(donnees$nutriscore_grade)&
#                   !is.na(donnees$countries_fr)&
#                   !is.na(donnees$countries_tags)&
#                   !is.na(donnees$countries)&
#                   !is.na(donnees$origins)&
#                   !is.na(donnees$origins_fr)&
#                   !is.na(donnees$origins_tags),]
# data <- data[data$countries_fr!=""&data$countries_tags!=""&data$countries!=""&data$nutriscore_grade!=""&
#                data$origins_fr!=""&data$origins_tags!=""&data$origins!="",]
# data <- data[,
#              c("product_name", "abbreviated_product_name", "generic_name",
#                "brands", "brands_tags", "categories", "categories_tags", "categories_fr", "origins", 
#                "origins_tags", "origins_fr", "manufacturing_places", "manufacturing_places_tags", 
#                "cities", "cities_tags", "purchase_places", 
#                "stores", "countries", "countries_tags", "countries_fr", "nutriscore_score", 
#                "nutriscore_grade", "nova_group","brand_owner", "main_category", 
#                "main_category_fr", "image_url", "image_small_url")]
# data$nutriscore_grade <- toupper(data$nutriscore_grade)
# write.table(data,file = "data/data_countries.csv",fileEncoding="UTF-8", sep="\t")
# rm(donnees)
data$countries <- gsub(", +",",", data$countries)

# countries : list of countries where the product is sold
# origins : origins of ingredients
# manufacturing_places : places where manufactured or transformed
data <- data.table::fread("data/data_countries.csv", encoding = "UTF-8")

unique(donnees$nutriscore_grade)
head(unique(donnees$purchase_places))
head(unique(donnees$countries_tags))
head(unique(donnees$cities_tags))

data[data$countries=="af",]
variable = "countries_fr"
data$countries_fr
sub_data <- data.frame(nutriscore_grade=data$nutriscore_grade,
                       stringr::str_split_fixed(data[[variable]], ",",120))
sub_data <- reshape2::melt(sub_data,id = "nutriscore_grade",na.rm = TRUE)
sub_data <- sub_data[sub_data$value!="",]
sub_data$country = gsub("^ *","",gsub("\\w*:","",sub_data$value))
correction_pays = list("France"= c("Francia","Frankreich","france","FR",
                                   "Francja","La Réunion","Polynésie française",
                                   "Frankrijk","Guadeloupe","Martinique","reunion",
                                   "Guyane","Réunion","Frankrike"),
                       "Belgique" = c("Belgium", "België",
                                      "BE","Bélgica", "Belgien"),
                       "Espagne" = c("España", "Spain.*","Espagna","Spanien"),
                       "Suisse" = c("Switzerland","Schweiz","ch", "Suiza",
                                    "Svizzera"),
                       "Allemagne" = c("Germany", "Deutschland",
                                       "DE","Alemania","Niemcy"),
                       "Italie" = c("Italia","Italy","IT"),
                       "Royaume-Uni" =  c("United Kingdom.*","GB","UK", "Regno Unito",
                                          "Reino Unido","Wielka Brytania",
                                          "Vereinigtes Königreich"),
                       "Mexique"= c("México", "Mexico"),
                       "États-Unis"= c("USA", "United States.*",
                                       "Estados Unidos.*"),
                       "Canada"="ca",
                       "Grèce"="Greece",
                       "Australie"=c("Australia"),
                       "Autriche"= c("Österreich","Austria","at"),
                       "Suède" = c("Sverige","Sweden"),
                       "Pologne" = c("Polska", "Poland"),
                       "Colombie" = c("Colombia"),
                       "Finlande"= c("Suomi","Finland","Finland"),
                       "Roumanie"=c("România","Romania"),
                       "Irlande"= c("Ireland"),
                       "Pays-bas"= c("Netherlands","Nederland","nl"),
                       "Lettonie" = c("Latvia","Letonia"),
                       "Lituanie"=c("Lithuania","Lituania"),
                       "Estonie"="Estonia",
                       "Slovénie"=c("Slovenia","Slovenija"),
                       "Slovaquie" = c("Eslovaquia"),
                       "Maroc" = "Morocco",
                       "Argentine" = "Argentina",
                       "Géorgie" = "საქართველო",
                       "Hongrie"=c("Magyarország", "Hungary"),
                       "Inde"="India",
                       "République tchèque"=c("Czech Republic","Česko"),
                       "Serbie"= "Serbia", "Nouvelle-Zélande"="New Zealand",
                       "Bolivie" = "Bolivia", "Chili"="Chile",
                       "Pérou"="Perú",
                       "Norvège"="Norway","Bulgarie"="Bulgaria",
                       "Hong Kong"="Hong Kong.*",
                       "Singapour"="Singapore")
for(nom_cor in names(correction_pays)){
  sub_data$country = gsub(paste0("(^",
                                 c(correction_pays[[nom_cor]],
                                   tolower(correction_pays[[nom_cor]]),
                                   tolower(nom_cor),
                                   toupper(correction_pays[[nom_cor]])),
                                 "$)", collapse = "|"),
                          nom_cor,sub_data$country)
}
sub_data <- sub_data %>% 
  count(country,nutriscore_grade) %>% 
  arrange(-n) %>% group_by(country) %>% 
  mutate(tot=sum(n)) %>% ungroup() %>% 
  mutate(n_prop = n/tot*100)

head(unique(sub_data$country),50)
unique(grep("Kong",sub_data$country,value=TRUE))
head(sub_data2,50)
table((stringr::str_count(data$manufacturing_places,","))+1)
table((stringr::str_count(data$countries_fr,","))+1)
table((stringr::str_count(data$origins_fr,","))+1)
table((stringr::str_count(data$purchase_places,","))+1)
table((stringr::str_count(data$stores,","))+1)
# table((stringr::str_count(data$cities,",")))


hcoptslang_nouv <- getOption("highcharter.lang")
hcoptslang_nouv$decimalPoint <- ","
options(highcharter.lang = hcoptslang_nouv)

colors_grade = c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                 E = "#D54C29")
top_n <- sub_data[sub_data$country%in%head(unique(sub_data$country),10),]
top_n$n_prop_ab <- top_n$n_prop
top_n$n_prop_ab[!top_n$nutriscore_grade%in%c("A","B")] <- 0
top_n= top_n %>% group_by(country) %>% 
  mutate(n_prop_ab=sum(n_prop_ab)) %>% 
  ungroup() %>% 
  arrange(desc(n_prop_ab))
titre <- "Nutriscore principaux pays"
caption_txt <- paste(c(
  paste0(paste0(unique(sprintf("%s : %i produits",top_n$country,top_n$tot)),
                collapse = " ; "),"."),
  "On se restreint aux 10 principaux pays en terme de nombre de produits recensés dans la base d'OpenFoodFacs."),
  collapse = "<br>")
p <- hchart(
  top_n, 
  type="bar",
  hcaes(x = country, y = n_prop, group = nutriscore_grade,
        index = nutriscore_grade),
  color = colors_grade,
  reversed=TRUE
)

p$x$hc_opts$series[[1]]$index = 4
p$x$hc_opts$series[[2]]$index = 3
p$x$hc_opts$series[[3]]$index = 2
p$x$hc_opts$series[[4]]$index = 1
p$x$hc_opts$series[[5]]$index = 0

p%>%
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")
  ) %>%
  hc_title(
    text = titre,
    style = list(fontFamily = "Gloria Hallelujah")
  ) %>%
  hc_yAxis(
    title = list(text="Pourcentage du total"),
    labels = list(format = "{value} %"),
    max = 100
  )%>%
  hc_xAxis(
    title = list(text="")) %>%
  hc_plotOptions(series=list(stacking="normal"))%>%
  hc_tooltip(pointFormat = '{series.name} : <b>{point.y:.1f} %</b><br/>')  %>%
  hc_caption(
    text = caption_txt,
    style = list(fontSize = "10px")
  ) %>% hc_legend(reversed=T)

saveRDS(top_n, "website/content/homepage/data/country_prop_plot.RDS")
saveRDS(colors_grade, "website/content/homepage/data/colors.RDS")

  
bubble_plot <- sub_data %>% select(country,tot) %>% unique() %>% 
  filter(tot>100) %>% arrange(-tot)
saveRDS(bubble_plot, "website/content/homepage/data/bubble_plot.RDS")

titre <- "Nombre de produits par vendus par pays"
caption_txt <- sprintf(paste(c("On ne garde que les pays où il y a plus de 100 produits vendus (%i pays).",
                         "Le plus gros pays vendeur (%s) vend %.2f plus de produits que le second plus gros pays (%s).",
                         "NB : certains produits peuvent être vendus dans plusieurs pays"),
                         collapse = "<br>"),
        nrow(bubble_plot),
        bubble_plot$country[1],
        bubble_plot$tot[1]/bubble_plot$tot[2],
        bubble_plot$country[2])
hchart(
  bubble_plot,
  type="packedbubble",
  hcaes(name = country, value = tot),
  name = ""
) %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = "<b>{point.name} :</b> {point.value}",
    headerFormat=""
  ) %>% 
  hc_plotOptions(
    packedbubble = list(
      maxSize = "150%",
      # layoutAlgorithm = list(
      #   gravitationalConstant =  0.05,
      #   seriesInteraction = TRUE,
      #   dragBetweenSeries = TRUE,
      #   parentNodeLimit = TRUE
      # ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}",
        style = list(
          color = "black",
          textOutline = "none",
          fontWeight = "normal"
        )
      )
    ),
    series = list(showInLegend = FALSE)
  )%>% 
  hc_title(
    text = titre,
    style = list(fontFamily = "Gloria Hallelujah")
  )%>%
  hc_caption(
    text = caption_txt,
    style = list(fontSize = "10px")
  )


