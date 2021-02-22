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
unique(brand_grade_t15$brands)
unique(brand_grade_t15$nutriscore_grade)

hchart(
  brand_grade_t15, 
  "bar",
  hcaes(x = brands, y = n, group = nutriscore_grade)
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


lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
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
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = FALSE),
    style = list(fontSize = "8px", textOutline = FALSE, color = "white")
  )
)
hchart(
  data_to_hierarchical(brand_grade_t15, c(brands,nutriscore_grade), n),
  type = "treemap",
  # levelIsConstant = FALSE,
  # allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
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

structure(list(brands = c("Carrefour", "Auchan", "U", "Carrefour", 
                         "Auchan", "U", "U", "Auchan", "Carrefour", "Carrefour", "Casino", 
                         "Auchan", "Casino", "U", "Auchan", "Casino", "Carrefour", "U", 
                         "Meijer", "Picard", "Lindt", "Meijer", "Great Value", "Leader Price", 
                         "Kroger", "Casino", "Casino", "Spartan", "Picard", "Leader Price", 
                         "Haribo", "Hacendado", "Kroger", "Picard", "Great Value", "Hacendado", 
                         "Ahold", "Bonduelle", "Leader Price", "Meijer", "Spartan", "Leader Price", 
                         "Picard", "Hacendado", "Kroger", "Ahold", "Leader Price", "Ahold", 
                         "Hacendado", "Great Value", "Meijer", "Spartan", "Kroger", "Ahold", 
                         "Ahold", "Hacendado", "Meijer", "Spartan", "Great Value", "Great Value", 
                         "Kroger", "Spartan", "Picard", "Bonduelle", "Lindt", "Bonduelle", 
                         "Haribo", "Haribo", "Haribo", "Lindt", "Bonduelle", "Lindt"), 
              nutriscore_grade = c("d", "d", "d", "c", "c", "a", "c", "a", 
                                   "a", "e", "d", "b", "a", "b", "e", "c", "b", "e", "d", "a", 
                                   "e", "a", "d", "d", "d", "b", "e", "d", "d", "c", "d", "c", 
                                   "a", "c", "a", "d", "d", "a", "a", "c", "a", "e", "b", "a", 
                                   "c", "c", "b", "a", "b", "c", "e", "c", "e", "b", "e", "e", 
                                   "b", "e", "b", "e", "b", "b", "e", "b", "d", "c", "e", "b", 
                                   "c", "c", "d", "b"), n = c(1015L, 967L, 959L, 894L, 791L, 
                                                              768L, 743L, 729L, 668L, 604L, 594L, 549L, 531L, 521L, 520L, 
                                                              515L, 507L, 502L, 475L, 440L, 429L, 423L, 407L, 407L, 383L, 
                                                              376L, 337L, 332L, 331L, 327L, 322L, 306L, 306L, 299L, 296L, 
                                                              295L, 289L, 284L, 280L, 277L, 262L, 259L, 258L, 241L, 228L, 
                                                              219L, 218L, 217L, 212L, 199L, 186L, 168L, 156L, 148L, 147L, 
                                                              142L, 141L, 140L, 137L, 119L, 111L, 93L, 84L, 53L, 24L, 13L, 
                                                              12L, 4L, 3L, 2L, 1L, 1L)), row.names = c(NA, -72L), class = c("data.table"))
