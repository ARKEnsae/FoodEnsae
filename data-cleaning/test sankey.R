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

hcoptslang_nouv <- getOption("highcharter.lang")
hcoptslang_nouv$decimalPoint <- ","
options(highcharter.lang = hcoptslang_nouv)

donnees_sankey <- donnees_nutri_nova %>%
  filter(nova_group!="" & nutriscore_grade!="") %>%
  mutate(nutriscore_grade = toupper(nutriscore_grade)) %>%
  group_by(nutriscore_grade) %>% 
  mutate(nutriscore_grade_freq = 100* n / sum(n),
         nutri_tot = sum(n)) %>% 
  ungroup() %>% 
  group_by(nova_group) %>% 
  mutate(nova_group_freq = 100* n / sum(n),
         nova_tot = sum(n)) %>% 
  ungroup() %>% 
  mutate(nutri_prop_tot = nutri_tot/sum(n)*100,
         nova_prop_tot = nova_tot/sum(n)*100)%>% data.frame()

mf_prop <- function(x){
  paste0("<b>",formatC(x,
                decimal.mark = ",",
                digits = 1, format = "f"),
        " %</b>")
}
mf_tot <- function(x){
  paste0("<b>",formatC(x,big.mark = " ", small.mark = " "),
        "</b>")
}
data_hc <- lapply(1:nrow(donnees_sankey), function(i){
  comment0 = sprintf("%s produits sont <b>Nova %s</b> et <b>Nutri-score %s</b>",
                     mf_tot(donnees_sankey[i,"n"]),
                     donnees_sankey[i,"nova_group"],
                     donnees_sankey[i,"nutriscore_grade"])
  comment1 = sprintf("%s des Nova %s sont Nutri-score %s",
                     mf_prop(donnees_sankey[i,"nova_group_freq"]),
                     donnees_sankey[i,"nova_group"],
                     donnees_sankey[i,"nutriscore_grade"])
  comment2 = sprintf("%s des Nutri-score %s sont Nova %s",
                     mf_prop(donnees_sankey[i,"nutriscore_grade_freq"]),
                     donnees_sankey[i,"nutriscore_grade"],
                     donnees_sankey[i,"nova_group"])
  list(from = donnees_sankey[i,"nova_group"],
       to = donnees_sankey[i,"nutriscore_grade"],
       weight = donnees_sankey[i,"n"],
       color = as.character(colors_nutri[donnees_sankey[i,1]]),
       comment = paste(comment0, comment1, comment2, sep="<br>")
       )
})
colors <- c(color_nova, colors_nutri)

nodes_color = c(lapply(seq_along(color_nova), function(i){
  ligne <- which(donnees_sankey$nova_group==names(color_nova)[i])[1]
  list(id = i,
       color = as.character(color_nova[i]),
       comment = sprintf("%s produits sont Nova %s (%s)",
                        mf_tot(donnees_sankey[ligne,
                                               "nova_tot"]),
                        donnees_sankey[ligne, "nova_group"],
                        mf_prop(donnees_sankey[ligne,
                                       "nova_prop_tot"])))
}),
lapply(seq_along(colors_nutri), function(i){
  ligne <- which(donnees_sankey$nutriscore_grade==names(colors_nutri)[i])[1]
  list(id = as.character(names(colors_nutri)[i]),
       color = as.character(colors_nutri[i]),
       comment = sprintf("%s produits sont Nutri-score %s (%s)",
                         mf_tot(donnees_sankey[ligne,
                                               "nutri_tot"]),
                         donnees_sankey[ligne, "nutriscore_grade"],
                         mf_prop(donnees_sankey[ligne,
                                                "nutri_prop_tot"])))
}))

p <- highchart() %>%
  hc_chart(type = 'sankey') %>%
  hc_add_series(
    data = data_hc,
    nodes = nodes_color
  ) 
p%>% 
  hc_plotOptions(headerFormat="",
                 series = list(
                   events = list(
                     legendItemClick = JS("function(event) {
                    return false;
                }")
                   )
                 ))%>% 
  hc_tooltip(headerFormat = "",
             pointFormat = "{point.comment}",
             nodeFormat = "{point.comment}",
             useHTML = TRUE) %>% 
  add_legend(colors_nutri, "Nutri-score ")
?hc_add_series
add_legend = function(p, colors, prefix="", symbol = "square"){
  for(i in names(colors)){
    p <- p %>%  
      hc_add_series(type = "scatter",
                    name = paste0(prefix,i),
                    color = as.character(colors[i]),
                    marker = list(symbol = symbol))
    p$x$hc_opts$series
  }
  p
}
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