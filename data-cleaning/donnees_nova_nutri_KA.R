library(ggplot2)
library(dplyr)
#devtools::install_github("rstudio/r2d3")
library(r2d3)
library(htmlwidgets)
library(networkD3)
library(highcharter)

### Pre-processing
#donnees <- data.table::fread("data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
#colnames(donnees)
#donnees_nutri_nova <- donnees %>% select(nutriscore_grade,nova_group) %>% group_by(nutriscore_grade,nova_group) %>%  summarise(n=n())
#saveRDS(donnees_nutri_nova,"data/donnees_nutri_nova.RDS")

#Color
colors_nutri = c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                 E = "#D54C29")
#color_nova <- brewer.pal(9,"Reds")[c(3,5,7,9)]
#display.brewer.pal(9,"Reds")
color_nova = c(G1 = "#FCBBA1",G2="#FB6A4A",G3="#CB181D",G4="#67000D")

#https://cran.r-project.org/web/packages/vegalite/vignettes/intro_to_vegalite.html
#https://observablehq.com/@d3/marimekko-chart
donnees_nutri_nova <- readRDS("data/donnees_nutri_nova.RDS")

donnees_nutri <- donnees_nutri_nova %>%
  group_by(nutriscore_grade) %>%
  summarise(n=sum(n)) %>% 
  filter(nutriscore_grade!="") %>% 
  mutate(nutriscore_grade = toupper(nutriscore_grade)) %>% 
  setNames(c("Country","Value")) %>% 
  mutate(pourc = round(100*Value/sum(Value),1)) %>% 
  mutate(color = colors_nutri)

p1 <- r2d3(data = donnees_nutri, 
		height=340,
           container = "cont",
           script = "Brouillon/Kim/d3/lollichart.js",
           options = list(borne_max=400000,
                          titre = "Nombre de produits selon le nutri-score",
                          note1 = paste0("Note : Sur les 1 635 635 produits de la base de données, seuls figurent"),
                          note2 = paste0("ici les 630 663 pour lesquels un nutri-score est indiqué.")
           ))

p1

#saveWidget(p1, file="lolli1.html")

donnees_nova <- donnees_nutri_nova %>%
  group_by(nova_group) %>%
  summarise(n=sum(n)) %>% 
  filter(nova_group!="") %>% 
  setNames(c("Country","Value")) %>% 
  mutate(pourc = round(100*Value/sum(Value),1)) %>% 
  mutate(color = color_nova) 

p2 <- r2d3(data = donnees_nova, 
     height=340,
     container = "cont",
     script = "Brouillon/Kim/d3/lollichart.js",
     options = list(borne_max=400000,
                    titre = "Nombre de produits selon le groupe Nova",
                    note1 = paste0("Note : Sur les 1 635 635 produits de la base de données, seuls figurent"),
                    note2 = paste0("ici les 588 403 pour lesquels un groupe Nova est indiqué.")
     ))
                    
p2
#saveWidget(p2, file="lolli2.html")



####################################

# index <- c(unique(donnees_sankey$target),unique(donnees_sankey$source))
# names(index)<-0:(length(index)-1)
# 
# donnees_sankey$target <- c(rep(0,4),rep(1,4),rep(2,4),rep(3,4),rep(4,4))
# donnees_sankey$source <- rep(5:8,5)
# 
# Energy <- list()
# Energy$nodes <- data.frame(name=index)
# Energy$links <- as.data.frame(donnees_sankey)
# Energy$links$group <- as.factor(paste0("g",Energy$links$target+1))
# Energy$nodes$group <- as.factor(Energy$nodes$name)
# 
# my_color <- 'd3.scaleOrdinal() .domain(["A","B","C","D","E", "1", "2", "3","4","g1", "g2", "g3","g4","g5"]) .range(["#387E48", "#91B849","#F7CB46","#E08531", "#D34C29", "#FCBBA1","#FB6A4A","#CB181D","#67000D","#387E48", "#91B849","#F7CB46","#E08531", "#D34C29"])'
# 
# 
# p3 <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#                    Target = "target", Value = "value", NodeID = "name",
#                    units = "produits", fontSize = 20, nodeWidth = 30,
#                    sinksRight = FALSE, colourScale = my_color,
#                   LinkGroup = "group", NodeGroup = "group", iterations=0)
# 
# p3

donnees_sankey <- donnees_nutri_nova %>%
  filter(nova_group!="" & nutriscore_grade!="") %>%
  mutate(nutriscore_grade = toupper(nutriscore_grade)) %>%
  group_by(nutriscore_grade) %>% 
  mutate(nutriscore_grade_freq = 100* n / sum(n)) %>% 
  ungroup() %>% 
  group_by(nova_group) %>% 
  mutate(nova_group_freq = 100* n / sum(n)) %>% 
  ungroup()


data_hc <- lapply(1:nrow(donnees_sankey), function(i){
  list(from = donnees_sankey[i,2], to = donnees_sankey[i,1],
       weight = donnees_sankey[i,3],
       color = as.character(colors_nutri[donnees_sankey[i,1]]))
})
colors <- c(color_nova, colors_nutri)


# Pour modifier les couleurs
nodes_color = c(lapply(seq_along(color_nova), function(i){
  list(id = i,
       color = as.character(color_nova[i]))
}),
lapply(seq_along(colors_nutri), function(i){
  list(id = as.character(names(colors_nutri)[i]),
       color = as.character(colors_nutri[i]))
}))

p3 <- highchart() %>%
  hc_chart(type = 'sankey') %>%
  hc_add_series(
    data = data_hc,
    nodes = nodes_color
  ) %>% hc_size(
    width = 500
  ) 

p3


########################################################

drawGauge <- function(chiffre){
  
  col.stops<-list_parse2(data.frame(q = c(0.15,0.8,1), c = c('#DF5353','#DDDF0D','#55BF3B'),stringsAsFactors = FALSE))
  
  
  chart <- highchart() %>%
    hc_chart(type = "solidgauge", plotBackgroundColor = '#f2efe8',backgroundColor = '#f2efe8')%>%
    hc_pane(center = JS("['50%','85%']"),
            size = "140%",
            startAngle = -90,
            endAngle = 90,background= list(
              outerRadius = '100%',
              innerRadius = '60%',
              shape="arc"
              #,backgroundColor= '#000000'
            )) %>% 
   # hc_plotOptions(center = JS("['50%','115%']")) %>% 
    hc_yAxis(
      stops=col.stops,
      lineWidth=0,
      minorTickWidth=0,
      tickAmount=2,
      min = 0,
      max = 100,
      labels=list(y=26,style = list(fontSize = "20px"), format='{value} %' )
    ) %>%
    hc_add_series(data = chiffre,dataLabels=list(y=-50,borderWidth=0, useHTML=TRUE,style = list(fontSize = "2rem"),format='{y} %'), enableMouseTracking = FALSE ) %>% 
    hc_size(
      height = 200
    )
  
  return(chart)
  
}

p1 <- drawGauge(81)
p1
p2 <- drawGauge(86)
p3 <- drawGauge(8)
p4 <- drawGauge(87)

p1

############################################
# data <- data.table::fread("data/salades.csv", encoding = "UTF-8")
# data <- data[grep(paste0("France", collapse = "|"),
#                   data$countries_fr),]
# data_plot <- data[,c("code", "product_name","image_small_url",
#                      "nutriscore_grade", "nova_group","fat_100g","sugars_100g", "proteins_100g")]
# data_plot <- data_plot[data_plot$sugars_100g<30,]
# data_plot$nutriscore_grade <- toupper(data_plot$nutriscore_grade)
# data_plot$nova_group[is.na(data_plot$nova_group)] = "Non-Renseigné"
# data_plot$nutriscore_grade[data_plot$nutriscore_grade ==""] = "Non-Renseigné"
# saveRDS(data_plot,"data/focus_group_data.RDS")

library(highcharter)
library(widgetframe)
library(dplyr)
color_var <- readRDS("website/content/homepage/data/colors.RDS")
data <- readRDS("data/focus_group_data.RDS")
my_own_theme <- readRDS("website/content/homepage/data/hc_theme.RDS")
data$ratio_fat_proteins <- data$fat_100g/data$proteins_100g
data$ratio_sugars_proteins <- data$sugars_100g/data$proteins_100g
data$log_ratio_fat_proteins <- log(data$fat_100g/data$proteins_100g)
data$log_ratio_sugars_proteins <- log(data$sugars_100g/data$proteins_100g)

hcoptslang_nouv <- getOption("highcharter.lang")
hcoptslang_nouv$decimalPoint <- ","
options(highcharter.lang = hcoptslang_nouv)
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

titre <- "Indices de sucre et de matière grasse des salades composées vendues en France"
#sous_titre <- "Qu'on arrête de nous prendre pour des salades !"
caption_txt <- paste("Cherchez plus d'informations sur les salades à partir du code-barres en utilisant le lien",
                     "https://fr.openfoodfacts.org/produit/<b>code_barre</b>.",
                     "<br>Par exemple pour la Sinappinen (salade de concombres) :",
                     "<a href=\"https://fr.openfoodfacts.org/produit/6424908462502\">https://fr.openfoodfacts.org/produit/6424908462502</a>.<br>")

p <- hchart(data, "scatter",
            #hcaes(x = `sugars_100g`, y = fat_100g,
            #hcaes(x = `ratio_sugars_proteins`, y = `ratio_fat_proteins`,
            hcaes(x = `log_ratio_sugars_proteins`, y = `log_ratio_fat_proteins`,
                        group = nutriscore_grade))
for(i in seq_along(p$x$hc_opts$series)){
  p$x$hc_opts$series[[i]]$color = as.character(color_var[p$x$hc_opts$series[[i]]$name])
  p$x$hc_opts$series[[i]]$marker$symbol = "diamond"
  p$x$hc_opts$series[[i]]$index = which(p$x$hc_opts$series[[i]]$name==names(color_var))
}
p %>% 
  hc_tooltip(formatter = JS(JStl),
             useHTML = TRUE)%>% 
  hc_title(
    text = titre
  )%>% 
  # hc_subtitle(
  #   text = sous_titre
  # )%>% 
  hc_yAxis(
    #type = 'logarithmic',
    title = list(text="Indice de matière grasse"),
    plotBands = list(
      list(
        label = list(text = "",
                     rotation=0, style=list(fontStyle="italic")),
        color = "rgba(237,28,36,.2)",
        from = 0,
        to = 1000
      )
    ),
    plotLines = list(
      list(
        label = list(text = "Aliment gras",
                     rotation=0, style=list(fontStyle="italic")),
        color = "black",
        dashStyle= "dot",
        width = 2,
        value = 0,
        zIndex=5
      )
    )
    )%>% 
  hc_xAxis(
    title = list(text="Indice de sucre"),
    plotBands = list(
      list(
        label = list(text = "", x=1,
                     rotation=0, style=list(fontStyle="italic")),
        color = "rgba(237,28,36,.2)",
        from = 0,
        to = 1000
      )
    ),
    plotLines = list(
      list(
        label = list(text = "Aliment sucré",
                     rotation=0, style=list(fontStyle="italic")),
        color = "black",
        dashStyle= "dot",
        width = 2,
        value = 0,
        zIndex=5
      )
    )
    ) %>% 
  hc_caption(
    text = caption_txt,
    useHTML = TRUE
  ) %>%
  hc_add_theme(my_own_theme) %>% 
  frameWidget(elementId = "focusProduit")

