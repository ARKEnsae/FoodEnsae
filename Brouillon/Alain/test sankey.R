library(highcharter)
library(dplyr)
colors_nutri = c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                 E = "#D54C29")
#color_nova <- brewer.pal(9,"Reds")[c(3,5,7,9)]
#display.brewer.pal(9,"Reds")
color_nova = c(G1 = "#FCBBA1",G2="#FB6A4A",G3="#CB181D",G4="#67000D")
names(color_nova) <- substr(names(color_nova),2,2)
#https://cran.r-project.org/web/packages/vegalite/vignettes/intro_to_vegalite.html
#https://observablehq.com/@d3/marimekko-chart
donnees_nutri_nova <- readRDS("content/homepage/data/donnees_nutri_nova.RDS")

donnees_sankey <- donnees_nutri_nova %>%
  filter(nova_group!="" & nutriscore_grade!="") %>% 
  mutate(nutriscore_grade = toupper(nutriscore_grade)) %>% 
  setNames(c("target","source","value")) 
donnees_sankey <- data.frame(donnees_sankey)
data_hc <- lapply(1:nrow(donnees_sankey), function(i){
  list(from = donnees_sankey[i,2], to = donnees_sankey[i,1],
       weight = donnees_sankey[i,3],
       color = as.character(colors_nutri[donnees_sankey[i,1]]))
})
colors <- c(color_nova, colors_nutri)

nodes_color = c(lapply(seq_along(color_nova), function(i){
  list(id = i,
       color = as.character(color_nova[i]))
}),
lapply(seq_along(colors_nutri), function(i){
  list(id = as.character(names(colors_nutri)[i]),
       color = as.character(colors_nutri[i]))
}))


highchart() %>%
  hc_chart(type = 'sankey') %>%
  hc_add_series(
    data = data_hc,
    nodes = nodes_color
  ) %>%
  hc_tooltip(useHTML = TRUE,
             formatter = JS('function() {
        var img = \'<img src = "https://static.pexels.com/photos/6606/flowers-garden-orange-tulips.jpg" height="82" width="122"/>\'
        return img
      }'))

JS

index <- c(unique(donnees_sankey$target),unique(donnees_sankey$source))
names(index)<-0:(length(index)-1)

donnees_sankey$target <- c(rep(0,4),rep(1,4),rep(2,4),rep(3,4),rep(4,4))
donnees_sankey$source <- rep(5:8,5)

Energy <- list()
Energy$nodes <- data.frame(name=index)
Energy$links <- as.data.frame(donnees_sankey)
Energy$links$group <- as.factor(paste0("g",Energy$links$target+1))
Energy$nodes$group <- as.factor(Energy$nodes$name)

my_color <- 'd3.scaleOrdinal() .domain(["A","B","C","D","E", "1", "2", "3","4","g1", "g2", "g3","g4","g5"]) .range(["#387E48", "#91B849","#F7CB46","#E08531", "#D34C29", "#FCBBA1","#FB6A4A","#CB181D","#67000D","#387E48", "#91B849","#F7CB46","#E08531", "#D34C29"])'


p3 <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name",
                    units = "produits", fontSize = 20, nodeWidth = 30,
                    sinksRight = FALSE, colourScale = my_color,
                    LinkGroup = "group", NodeGroup = "group", iterations=0)