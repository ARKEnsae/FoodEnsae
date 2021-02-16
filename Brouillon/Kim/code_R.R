library(data.table)
library(stringr)
library(ggwordcloud)
library(wordcloud)
library(dplyr)
library(RColorBrewer)

#devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
donnees <- fread("C:/Users/Kim Antunez/Desktop/3A/data_storytelling/Data_Storytelling/data/fr.openfoodfacts.org.products.csv", encoding = "UTF-8")


#WordCloud des catégories
#categories <- unlist(str_split(donnees$categories_fr, ","))
#categories <- as.data.frame(table(categories), stringsAsFactors = FALSE)

setwd("C:/Users/Kim Antunez/Desktop/3A/data_storytelling/Data_Storytelling/Brouillon/Kim")
#saveRDS(categories,"../../data/categories.RDS")
categories <- readRDS("../../data/categories.RDS")

#Petites données de catégories
categories2 <- categories[which(categories$Freq>8000),]
categories2$Freq <- sqrt(sqrt(categories2$Freq))
categories2$categories[which(categories2$categories=="Aliments et boissons à base de végétaux")] <- "Origine végétale"
categories2$categories[which(categories2$categories=="Aliments d'origine végétale")] <- "Origine végétale"
categories2 <- categories2[which(!categories2$categories%in%c("")),] #,"Origine végétale"
categories2 <- categories2 %>%
  group_by(categories) %>%
  summarise(Freq = sum(Freq)) %>% 
  ungroup() %>% 
  arrange(desc(Freq)) %>% 
  mutate(couleur=c("#41AB5D",sample(brewer.pal(9,"Oranges")[-c(1:4)],(nrow(.)-1), replace = TRUE)))

wordcloud2(data=categories2, size=0.5,shuffle=FALSE, color = categories2$couleur)

# wordcloud(categories$categories,categories$Freq,
#            min.freq=50,random.order=FALSE, rot.per=0.35)

#figPath = "C:/Users/Kim Antunez/Desktop/3A/data_storytelling/Data_Storytelling/Brouillon/Kim/plate2.png"


# set.seed(42)
# ggplot(
#   categories %>%
#     mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))),
#   aes(
#     label = as.character(categories), size = Freq)
# ) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 25) +
#   theme_minimal() +
#   scale_color_gradient(low = "darkred", high = "red")



#https://grace.rbind.io/post/2019-02-22-emoji-mds/

#https://buzzrbeeline.blog/2018/06/13/fun-and-easy-r-graphs-with-images/

#grepl("Trop","Tropl",fixed=TRUE)
chaine <- "Yaourts à boire sans sucres"
bool <- sapply(donnees$categories_fr, function (k) grepl(chaine, k, fixed = TRUE))
yaourts <- donnees[bool,]
saveRDS(yaourts,"../../data/yaourts.RDS")
yaourts <- readRDS("../../data/yaourts.RDS")

library(ggimage)
ggplot(data = yaourts, aes(x = energy_100g, y = fat_100g)) +
  geom_point() +
  geom_image(aes(image = image_small_url), size = 0.05) +
  xlab("energy_100g") +
  ylab("fat_100g") 


chaine <- "Bière"
bool <- sapply(donnees$categories_fr, function (k) grepl(chaine, k, fixed = TRUE))
biere <- donnees[bool,]
colnames(biere) <- grep()
biere <- biere[which(!is.na(biere[,"energy-kcal_100g"])),]
biere <- biere[which(biere[,"image_small_url"]!=""),]
saveRDS(biere,"../../data/biere.RDS")
biere <- readRDS("../../data/biere.RDS")

ggplot(data = biere[1:10,], aes(x = `energy-kcal_100g`,y=1)) +
  geom_point() +
  geom_image(aes(image = image_small_url), by="height", nudge_x = 0, size = 0.10) +
  xlab("energy_100g") 

# Bières à la cerise
# Crèmes Chantilly
# Thés minceur
# Yaourts à boire sans sucres
??geom_image
