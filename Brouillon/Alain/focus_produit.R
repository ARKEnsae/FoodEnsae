library(ggplot2)
library(dplyr)
library(highcharter)


# donnees <- data.table::fread("data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
# data <- donnees[grep("Salades composées",donnees$categories_fr),]
## Peut-etre regarder catégorie "main_category_fr"
# write.table(data,file = "data/salades.csv",fileEncoding="UTF-8", sep="\t")
data <- data.table::fread("data/salades.csv", encoding = "UTF-8")

grep("France",unique(data$countries_fr),invert = TRUE,value=TRUE)
data$generic_name
data <- data[grep(paste0("France", collapse = "|"),
     data$countries_fr),]

data_plot <- data[,c("code", "product_name","image_small_url",
                     "nutriscore_grade", "nova_group","fat_100g","sugars_100g")]
data_plot <- data_plot[data_plot$sugars_100g<30,]
data_plot$nutriscore_grade <- toupper(data_plot$nutriscore_grade)
data_plot$nova_group[is.na(data_plot$nova_group)] = "Non-Renseigné"
data_plot$nutriscore_grade[data_plot$nutriscore_grade ==""] = "Non-Renseigné"
saveRDS(data_plot,"content/homepage/data/focus_group_data.RDS")

colnames(data)
unique(data_plot$nova_group)
colors_nutri = c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                 E = "#D54C29","Non-Renseigné"= "gray")
# saveRDS(colors_nutri,"content/homepage/data/colors.RDS")
colors_grade <- readRDS("content/homepage/data/colors.RDS")
color_nova = c(G1 = "#FCBBA1",G2="#FB6A4A",G3="#CB181D",G4="#67000D")
names(color_nova) <- substr(names(color_nova),2,2)
color_nova <- c(color_nova, "Non-Renseigné"= "gray")

color_var = colors_nutri
titre <- "Salades vendues en Frances par nutriscore et taux de matières grasses et de sucres"
sous_titre <- "Qu'on arrête de nous prendre pour des salades !"
caption_txt <- paste("Cherchez plus d'informations sur les salades à partir du code-bar en utilisant le lien",
                       "https://fr.openfoodfacts.org/produit/<b>code_bar</b>.",
                        "<br>Par exemple pour la Sinappinen (salade de concombres) :",
                       "<a href=\"https://fr.openfoodfacts.org/produit/6424908462502\">https://fr.openfoodfacts.org/produit/6424908462502</a>.")
p <- hchart(data_plot, "scatter",
       hcaes(x = `sugars_100g`, y = fat_100g,
             group = nutriscore_grade))
for(i in seq_along(p$x$hc_opts$series)){
  p$x$hc_opts$series[[i]]$color = as.character(color_var[p$x$hc_opts$series[[i]]$name])
  p$x$hc_opts$series[[i]]$index = which(p$x$hc_opts$series[[i]]$name==names(color_var))
}
p %>% 
  hc_tooltip(formatter = JS(JStl),
             useHTML = TRUE)%>% 
  hc_title(
    text = titre
  )%>% 
  hc_subtitle(
    text = sous_titre
  )%>% 
  hc_yAxis(
    title = list(text="Matières grasse (g pour 100g)"))%>% 
  hc_xAxis(
    title = list(text="Sucres (g pour 100g)")) %>% 
  hc_caption(
    text = caption_txt,
    useHTML = TRUE
  )
hc_subtitle()
p$x$hc_opts$series[[1]]$color = "red"

data[data$code=='6424908462502',]
data$
'3083681006650'
JStl <- "function(){
  if (this.point.image_small_url==''){
    var img = ''
    //'<img src =\"https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/600px-No_image_available.svg.png\" width=\"82\"\"/>'
  } else{https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/600px-No_image_available.svg.png
    var img = '<br><img src =\"'+this.point.image_small_url+'\" height=\"82\"/>'
  }
  img = '<br>'+img
  var nova_score = this.point.nova_group
  if (nova_score == '1'){
    nova_score = nova_score+ ' (Aliments non transformés)'
  }else if (nova_score == '2'){
    nova_score = nova_score+ ' (Ingrédients culinaires transformés)'
  }else if (nova_score == '3'){
    nova_score = nova_score+ ' (Aliments transformés)'
  }else if (nova_score == '4'){
    nova_score = nova_score+ ' (Ultra-transformés)'
  }else {
    nova_score = 'NR'
  }
  var nutri_score ='<br>Nutri-score : ' 
  if(this.point.nutriscore_grade == ''){
    nutri_score = nutri_score + 'NR'
  }else{
    nutri_score = nutri_score + this.point.nutriscore_grade
  }
  nova_score = '<br>NOVA : '+nova_score
  var nom_prod = '<b>' + this.point.product_name+ '</b>'
  nom_prod = nom_prod + '<br>(code : '+this.point.code+')'
  return (nom_prod+ nutri_score + nova_score  + img)
}"
p %>% 
  hc_tooltip(formatter = JS(JStl),
             useHTML = TRUE)
hc() %>% 
  hchart(data_plot, "scatter",
         hcaes(x = `nutrition-score-fr_100g`, y = fat_100g,
               group = nova_group))
%>% hc_tooltip(useHTML = TRUE,
                 formatter = JS(
                   "function() {
                   var img = '{series.name}<img src = \"this.y\" height=\"82\" width=\"122\"/>'
                   return '<b>this.point.x</b>'
                 }"           
                 )
)

hc_tooltip(pointFormat = '{series.name} : <b>{point.y:.1f} %</b><br/>')
formatter= JS("function() {
                   var img = '{series.name}<img src = \"this.y\" height=\"82\" width=\"122\"/>'
                   return '<b>{point.x}</b>'
                 }")
formatter: "function() {
  var point = this.point,
  series = this.series,
  pointIndex = point.index,
  text,
  additionalValue;
  
  text = '<span style=\"color:' +
    this.color + '\">\u25CF</span> ' +
    series.name +
    ': <b>' +
    this.y +
    '</b><br/>';
  
  additionalValue =
    series.userOptions.data[pointIndex][2];
  
  text += '<br> Additional value: ' +
    additionalValue;
  
  return text;
}"

