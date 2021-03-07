library(ggplot2)
library(dplyr)
library(highcharter)

# donnees <- data.table::fread("../data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
# # colnames(donnees)
# data <- donnees[!is.na(donnees$countries_fr),]
# data <- data[data$countries_fr!="",]
# data <- data[, c("nutriscore_grade", "nova_group", "countries_fr")]
# data$nutriscore_grade <- toupper(data$nutriscore_grade)
# data$nova_group[is.na(data$nova_group)] <- ""
# write.table(data,file = "data/data_countries.csv",fileEncoding="UTF-8", sep="\t",
            # row.names = FALSE)
# rm(donnees)

# countries : list of countries where the product is sold
# origins : origins of ingredients
# manufacturing_places : places where manufactured or transformed
data <- data.table::fread("data/data_countries.csv", encoding = "UTF-8")

traitement <- function(x, id){
  if(id =="nutriscore_grade"){
    id_removed = grep("nova_group", colnames(x))
  }else{
    id_removed = grep("nutriscore_grade", colnames(x))
  }
  sub_data<- reshape2::melt(x[,-id_removed],id = id,na.rm = TRUE)
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
  sub_data
}

data <- data.frame(nutriscore_grade=data$nutriscore_grade,
                   nova_group=data$nova_group,
                   stringr::str_split_fixed(data[["countries_fr"]], ",",120))
sub_data_nutri <- traitement(data, "nutriscore_grade")
sub_data_nova <- traitement(data, "nova_group")


colors_grade = c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                 E = "#D54C29")
color_nova = c("1" = "#FCBBA1","2"="#FB6A4A","3"="#CB181D","4"="#67000D")
#######################################################
################## HISTOGRAMME NUTRI ##################
#######################################################

histo_nutri <- sub_data_nutri[!is.na(sub_data_nutri$nutriscore_grade),] %>% 
  filter(nutriscore_grade != "") %>% 
  count(country,nutriscore_grade) %>% 
  group_by(country) %>% 
  mutate(tot=sum(n))%>% ungroup() %>% 
  mutate(n_prop = n/tot*100) %>% arrange(-n)%>% 
  arrange(-tot) %>% 
  filter(., country %in% head(distinct(., country),10)$country)
histo_nutri$n_prop_ed <- histo_nutri$n_prop
histo_nutri$n_prop_ed[!histo_nutri$nutriscore_grade%in%c("E","D")] <- 0
histo_nutri= histo_nutri %>% group_by(country) %>% 
  mutate(n_prop_ed=sum(n_prop_ed)) %>% 
  ungroup() %>% 
  arrange(desc(n_prop_ed))
saveRDS(histo_nutri, "website/content/homepage/data/country_prop_plot.RDS")

titre <- "Nutri-score principaux pays"
caption_txt <- paste(c(
  paste0(paste0(unique(sprintf("%s : %s produits",histo_nutri$country,formatC(histo_nutri$tot,big.mark = " ", small.mark = " "))),
                collapse = " ; "),"."),
  "On se restreint aux 10 principaux pays en terme de nombre de produits recensés dans la base d'OpenFoodFacts pour lesquels le Nutri-score est disponible."),
  collapse = "<br>")

hchart(
  histo_nutri, 
  type="bar",
  hcaes(x = country, y = n_prop, group = nutriscore_grade,
        index = nutriscore_grade),
  color = colors_grade
) %>%
  hc_title(
    text = titre
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
    text = caption_txt
  ) %>%
  #hc_add_theme(my_own_theme) %>% 
  hc_add_theme(hc_theme(chart=list(backgroundColor="#f2efe8")))

######################################################
################## HISTOGRAMME NOVA ##################
######################################################

histo_nova <- sub_data_nova[!is.na(sub_data_nova$nova_group),] %>% 
  filter(nova_group != "") %>% 
  count(country,nova_group) %>% 
  group_by(country) %>% 
  mutate(tot=sum(n))%>% ungroup() %>% 
  mutate(n_prop = n/tot*100) %>% arrange(-n)%>% 
  arrange(-tot) %>% 
  filter(., country %in% head(distinct(., country),10)$country)
histo_nova$n_prop_4 <- histo_nova$n_prop
histo_nova$n_prop_4[!histo_nova$nova_group%in%c("4")] <- 0
histo_nova= histo_nova %>% group_by(country) %>% 
  mutate(n_prop_4=sum(n_prop_4)) %>% 
  ungroup() %>% 
  arrange(desc(n_prop_4))
saveRDS(histo_nova, "website/content/homepage/data/country_prop_plot_nova.RDS")

titre <- "Nova principaux pays"
caption_txt <- paste(c(
  paste0(paste0(unique(sprintf("%s : %s produits",histo_nova$country,formatC(histo_nova$tot,big.mark = " ", small.mark = " "))),
                collapse = " ; "),"."),
  "On se restreint aux 10 principaux pays en terme de nombre de produits recensés dans la base d'OpenFoodFacts pour lesquels le NOVA est disponible."),
  collapse = "<br>")

hchart(
  histo_nova, 
  type="bar",
  hcaes(x = country, y = n_prop, group = nova_group,
        index = nova_group),
  color = color_nova
) %>%
  hc_title(
    text = titre
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
    text = caption_txt
  )  %>%
  #hc_add_theme(my_own_theme) %>% 
  hc_add_theme(hc_theme(chart=list(backgroundColor="#f2efe8")))



bubble_plot <- sub_data_nutri %>% 
  count(country) %>% select(country,n) %>% unique() %>% 
  filter(n>1500) %>% arrange(-n) %>% mutate(group = "Tous les produits")
bubble_plot_nutri <-  sub_data_nutri[!is.na(sub_data_nutri$nutriscore_grade),] %>% 
  filter(nutriscore_grade != "", country %in% bubble_plot$country)  %>% 
  count(country) %>% select(country,n) %>% unique() %>% arrange(-n) %>% 
  mutate(group = "Nutri-score connu")
bubble_plot_nova <-  sub_data_nova[!is.na(sub_data_nova$nova_group),] %>% 
  filter(nova_group != "", country %in% bubble_plot$country)  %>% 
  count(country) %>% select(country,n) %>% unique() %>% arrange(-n) %>% 
  mutate(group = "NOVA connu")

bubble_plot_all <- rbind(bubble_plot,
                         bubble_plot_nutri,
                         bubble_plot_nova)
saveRDS(bubble_plot, "website/content/homepage/data/bubble_plot.RDS")
saveRDS(bubble_plot_all, "website/content/homepage/data/bubble_plot_all.RDS")
bubble_plot_all <- readRDS("content/homepage/data/bubble_plot_all.RDS")

titre <- "Nombre de produits par vendus par pays et en fonction de la disponibilité du Nutri-Score et du score Nova"
caption_txt <- sprintf(paste(c("On ne garde que les pays où il y a plus de 1 500 produits vendus recensés dans la base d'OpenFoodFaces (%i pays).",
                               "NB : certains produits peuvent être vendus dans plusieurs pays"),
                             collapse = "<br>"),
                       nrow(bubble_plot_all)/3)
p <- hchart(
  bubble_plot_all,
  type="packedbubble",
  hcaes(name = country, value = n,group = group)
) %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = "<b>{point.name} :</b> {point.value}"
  ) %>% 
  hc_plotOptions(
    packedbubble = list(
      maxSize = "150%",
      layoutAlgorithm = list(
        gravitationalConstant =  0.1,
        splitSeries =  TRUE, # TRUE to group points
        seriesInteraction = TRUE,
        dragBetweenSeries = TRUE,
        parentNodeLimit = TRUE
      ),
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
    series = list(showInLegend = c(TRUE,FALSE,FALSE))
  )%>% 
  hc_title(
    text = titre
  )%>%
  hc_caption(
    text = caption_txt
  )
hide <- c("NOVA connu", "Nutri-score connu")
for (i in seq_along(p$x$hc_opts$series)){
  p$x$hc_opts$series[[i]]$visible <- ! (p$x$hc_opts$series[[i]]$name %in% hide)
}
p



titre <- "Nombre de produits par vendus par pays"
caption_txt <- sprintf(paste(c("On ne garde que les pays où il y a plus de 100 produits vendus (%i pays).",
                         "Le plus gros pays vendeur (%s) vend %.2f plus de produits que le second plus gros pays (%s).",
                         "NB : certains produits peuvent être vendus dans plusieurs pays"),
                         collapse = "<br>"),
        nrow(bubble_plot),
        bubble_plot$country[1],
        bubble_plot$n[1]/bubble_plot$n[2],
        bubble_plot$country[2])
hchart(
  bubble_plot,
  type="packedbubble",
  hcaes(name = country, value = n),
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


