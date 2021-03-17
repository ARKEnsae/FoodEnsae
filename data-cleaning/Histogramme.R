library(ggplot2)
library(dplyr)
library(highcharter)

# print(Sys.time())
# donnees <- data.table::fread("data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
# print(Sys.time())
# # rm(donnees)
# # print(Sys.time())
# # donnees <- arrow::read_tsv_arrow("data/fr.openfoodfacts.org.products.csv")
# # print(Sys.time())
# donnees <- data.table::fread("data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")
# data = donnees[!is.na(donnees$additives_n)&!is.na(donnees$nutriscore_grade)&!is.na(donnees$`energy-kcal_100g`)&!is.na(donnees$brands),]
# quantile(data$`energy-kcal_100g`,probs = seq(0,1,0.05))
# data=data[data$`energy-kcal_100g`<1000,]
# data = data[(data$brands!="")&(data$nutriscore_grade!=""),]
# write.table(data,file = "data/sub_data.csv",fileEncoding="UTF-8", sep="\t")
data <- data.table::fread("data/sub_data.csv", encoding = "UTF-8")
head(data$generic_name)
unique(data$generic_name)
head(data$product_name)
unique(data$nutriscore_grade)
colnames(donnees)
donnees$brands_tags
sum(is.na(donnees$brands_tags))
sum(is.na(donnees$brands))
head(unique(donnees$additives_n))
is.na(head(donnees$additives_n))
donnees$nutriscore_grade
# ggplot(donnees, aes(x=`nutriscore_grade`)) + 
#   geom_bar()

#Color
dput(c(rgb(56, 126, 72, names = "A", maxColorValue = 255),
       rgb(145, 184, 73, names = "B", maxColorValue = 255),
       rgb(247, 203, 70, names = "C", maxColorValue = 255),
       rgb(224, 133, 49, names = "D", maxColorValue = 255),
       rgb(213, 76, 41, names = "E", maxColorValue = 255)))
colors_grade = c(A = "#387E48", B = "#91B849", C = "#F7CB46", D = "#E08531", 
  E = "#D54C29")

ggplot(data, aes(x=`nutriscore_grade`)) + 
  geom_bar()
ggplot(data, aes(x=additives_n,fill=`nutriscore_grade`)) + 
  geom_histogram(alpha=0.5, position="identity",bins = 20) +
  facet_wrap(vars(`nutriscore_grade`))
ggplot(data, aes(x=`energy-kcal_100g`,fill=`nutriscore_grade`)) + 
  geom_histogram(alpha=0.5, position="identity") +
  facet_wrap(vars(`nutriscore_grade`))
ggplot(data, aes(x=`energy-kcal_100g`,y = additives_n,color=`nutriscore_grade`)) + 
  geom_point() +
  facet_wrap(vars(`nutriscore_grade`))

hchart(data, "scatter", hcaes(x=`energy-kcal_100g`,y = additives_n,color=`nutriscore_grade`))

brand_grade <- data %>% 
  count(brands,`nutriscore_grade`) %>% 
  arrange(-n)
brand_grade_t15 <- brand_grade[brand_grade$brands%in%head(unique(brand_grade$brands),15),]
brand_grade_t15$nutriscore_grade <- toupper(brand_grade_t15$nutriscore_grade)
unique(brand_grade_t15$brands)
unique(brand_grade_t15$nutriscore_grade)
c(56,66)

hchart(
  brand_grade_t15, 
  "bar",
  hcaes(x = brands, y = n, group = nutriscore_grade),
  color = c(colors_grade)
) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")
  ) %>% 
  hc_title(
    text = "Nombre de produits par nutriscore des principales marques",
    style = list(fontFamily = "Gloria Hallelujah")
  ) %>% 
  hc_yAxis(
    title = list(text="Nombre de produits"))%>% 
  hc_xAxis(
    title = list(text=""))

hchart(brand_grade_t15, "treemap",
       hcaes(x = brands, value = n, color = nutriscore_grade))%>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")
  ) 


data_plot <- data_to_hierarchical(brand_grade_t15, c(brands,nutriscore_grade),n)
x = data_plot[[1]]
data_plot2 <- lapply(data_plot,function(x){
  if(x$level==1){
    x$showInLegend = FALSE
    return(x)
  }else{
    x$color = as.character(colors_grade[x$name])
    return(x)
  }
})
lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 4,
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
    borderWidth = 0,
    borderColor = "white",
    # colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE), # pour ne pas afficher legend sur graph
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)
hchart(
  data_plot2,
  type = "treemap",
  # levelIsConstant = FALSE,
  # allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE),
  showInLegend = FALSE,
  legendType= 'point'
) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")
  ) %>% 
  hc_title(
    text = "Nutriscore des produits des principales marques",
    style = list(fontFamily = "Gloria Hallelujah")
  )
hchart(brand_grade_t15, "treemap",
       hcaes(x = brands, value = n, color = nutriscore_grade))%>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")
  ) 
