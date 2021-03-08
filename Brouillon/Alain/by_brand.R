library(highcharter)
library(widgetframe)
library(dplyr)
colors_grade <- c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
                  E = "#D54C29")
my_own_theme <- structure(list(chart = list(backgroundColor = NULL), caption = list(
  style = list(fontSize = "10px"))), class = "hc_theme")

# donnees <- data.table::fread("../data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
# data = donnees[!is.na(donnees$nutriscore_grade)& !is.na(donnees$brands),]
# data = data[(data$brands!="")&(data$nutriscore_grade!=""),]
# data = data[,c("brands","nutriscore_grade","countries_fr","nova_group")]
# write.table(data,file = "../data/data_brand.csv",fileEncoding="UTF-8", sep="\t",row.names=FALSE)
data <- data.table::fread("../data/data_brand.csv", encoding = "UTF-8")
data[grep(paste0("France", collapse = "|"),
     data$countries_fr),]
brand_grade <- data[grep(paste0("France", collapse = "|"),
                         data$countries_fr),] %>% 
  count(brands,`nutriscore_grade`) %>% 
  arrange(-n) %>% group_by(brands) %>% 
  mutate(tot = sum(n)) %>% 
  ungroup() %>% mutate(prop = n/tot*100,
                       id = tolower(gsub(" ","_", brands)),
                       nutriscore_grade = toupper(nutriscore_grade))
ent_retenues <- brand_grade %>% arrange(-tot) %>% distinct(brands,tot) %>% head(15)
brand_grade_t15 <- brand_grade %>% 
  filter(brands %in% ent_retenues$brands)

saveRDS(brand_grade_t15, file = "content/homepage/data/top15_brands.RDS")

max_prop <- sapply(unique(brand_grade_t15$id),function(b){
  brand_grade_t15 %>% filter(id == b) %>% 
    arrange(-n) %>% .[1,"nutriscore_grade"] %>% as.character()
})
data_plot <- data_to_hierarchical(brand_grade_t15, c(brands,nutriscore_grade),c(n))
data_plot2 <- lapply(data_plot,function(x){
  if(x$level==1){
    x$pct = ""
    return(x)
  }else{
    x$color = as.character(colors_grade[x$name])
    x$pct = brand_grade_t15 %>% 
      filter(id == x$parent, nutriscore_grade == x$name) %>% select(prop) %>% 
      as.numeric %>% 
      formatC(decimal.mark = ",",
              digits = 1, format = "f") %>% 
      sprintf(" (%s %%)",.)
    if(x$name == max_prop[x$parent]){
      x$borderWidth=3
      x$dataLabels$enabled = TRUE
    }
    return(x)
  }
})
lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 5,
    borderColor = "black",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(fontSize = "12px", textOutline = FALSE, color = "white")
    )
  ),
  list(
    level = 2,
    # borderWidth = 0,
    borderColor = "gray",
    # colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE), # pour ne pas afficher legend sur graph
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)
p <- hchart(
  data_plot2,
  type = "treemap",
  # levelIsConstant = FALSE,
  # allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE),
  legendType= 'point',
  showInLegend = FALSE
) %>% 
  hc_title(
    text = "Nutriscore des produits des principales marques"
  ) %>% 
  hc_tooltip(pointFormat = "<b>{point.name}</b> : {point.value}{point.pct}<br/>") 
p
p$x$hc_opts$series[[1]][-1]
# p$x$hc_opts$series = lapply(p$x$hc_opts$series[[1]]$data, function(x){
#   c(list(data = list(x)),
#     p$x$hc_opts$series[[1]][-1])
# })
x=(p$x$hc_opts$series[[1]]$data[[1]])
length(p$x$hc_opts$series)
p$x$hc_opts$series[[1]]$data[[16]]$showInLegend = F
names(p$x$hc_opts$series[[1]])
p$x$hc_opts$series[[30]]$data
for (i in seq_along(p$x$hc_opts$series[[1]]$data)){
  p$x$hc_opts$series[[1]]$data[[i]]$hiddenInLegend = TRUE
}
p$x$hc_opts

p 
hc_
?hc_add_event_point
p$x$
Jsf = "function() {
  var name = this.name;
  var _i = this._i;
  Highcharts.each(this.chart.series, function(p, i) {
    if (name === p.name && _i !== p._i) {
      (!p.visible) ? p.show(): p.hide()
    }
  })
}"
Jsf = "function () {
                    var visibility = this.visible ? 'visible' : 'hidden';
                    if (!confirm('The series is currently ' +
                                 visibility + '. Do you want to change that?')) {
                        return false;
                    }
                }"
