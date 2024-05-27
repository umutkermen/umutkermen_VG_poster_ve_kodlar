# Veri Gorsellestirme Poster #

install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggcorrplot")
install.packages("corrplot")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library (corrplot)
# Calisma surecinde veri manipulasyonu ve gorsellestirme icin dplyr, ggplot2, gridExtra, ggcorrplot ve corrplot paketlerine ihtiyacim olacaktir
# grafikleri yerlestirmek icin gridExtra paketine ihtiyacim olabilir

risk_var <- bozulmus_yeme_data %>% filter(risk == "Var")
risk_yok <- bozulmus_yeme_data %>% filter(risk == "Yok")
# yeme bozuklugu riski olanlar ve lmayanlar ici iki ayrı data frame olusturuldu

str(bozulmus_yeme_data)
str(risk_var)
str(risk_yok)
# verilerin yapisi kontrol edildi


# cinsiyete göre kernel yogunluk tahminleri

a <- ggplot(bozulmus_yeme_data, aes(x = bozulmus_yeme, fill = cinsiyet)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Bozulmuş Yeme", 
       y = "Yoğunluk",
       title = "Bozulmuş Yeme Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Cinsiyet") + 
  scale_fill_discrete(labels = c("Erkek", "Kız")) +
  lims(x = c(0, 80), y = c(0, 0.15)) + 
  theme_bw()


b <- ggplot(bozulmus_yeme_data, aes(x = depresyon, fill = cinsiyet)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Depresyon", 
       y = "Yoğunluk",
       title = "Depresyon Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Cinsiyet") + 
  scale_fill_discrete(labels = c("Erkek", "Kız")) +
  lims(x = c(0, 80), y = c(0, 0.08)) +
  theme_bw()


c <- ggplot(bozulmus_yeme_data, aes(x = gorunus_kaygisi, fill = cinsiyet)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Görünüş Kaygısı", 
       y = "Yoğunluk",
       title = "Görünüş Kaygısı Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Cinsiyet") + 
  scale_fill_discrete(labels = c("Erkek", "Kız")) +
  lims(x = c(0, 90), y = c(0, 0.05)) + 
  theme_bw()


d <- ggplot(bozulmus_yeme_data, aes(x = bozulmus_yeme_data$aleksitimi, fill = cinsiyet)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Aleksitimi", 
       y = "Yoğunluk",
       title = "Aleksitimi Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Cinsiyet") + 
  scale_fill_discrete(labels = c("Erkek", "Kız")) +
  lims(x = c(0, 90), y = c(0, 0.05)) +
  theme_bw()


e<- ggplot(bozulmus_yeme_data, aes(x = bozulmus_yeme_data$bastirma, fill = cinsiyet)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Bastırma", 
       y = "Yoğunluk",
       title = "Bastırma Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Cinsiyet") + 
  scale_fill_discrete(labels = c("Erkek", "Kız")) +
  lims(x = c(0, 30), y = c(0, 0.10)) +
  theme_bw()


f <- ggplot(bozulmus_yeme_data, aes(x = bozulmus_yeme_data$yeniden_degerlendirme, fill = cinsiyet)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Yeniden Değerlendirme", 
       y = "Yoğunluk",
       title = "Yeniden Değerlendirme Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Cinsiyet") + 
  scale_fill_discrete(labels = c("Erkek", "Kız")) +
  lims(x = c(0, 40), y = c(0, 0.07)) +
  theme_bw()

grid.arrange(a, b, c, d, e, f, nrow = 2, ncol = 3)

# riske gore kernel yoğunluk tahmini dagılımları

g<- ggplot(bozulmus_yeme_data, aes(x = bozulmus_yeme, fill = risk)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Bozulmuş Yeme", 
       y = "Yoğunluk",
       title = "Bozulmuş Yeme Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Yeme Bozukluğu Riski") + 
  scale_fill_discrete(labels = c("Var", "Yok")) +
  lims(x = c(0, 80), y = c(0, 0.13)) +
  theme_bw()


h <-ggplot(bozulmus_yeme_data, aes(x = depresyon, fill = risk)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Depresyon", 
       y = "Yoğunluk",
       title = "Depresyon Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Yeme Bozukluğu Riski") + 
  scale_fill_discrete(labels = c("Var", "Yok")) +
  lims(x = c(0, 60), y = c(0, 0.06)) +
  theme_bw()


i <- ggplot(bozulmus_yeme_data, aes(x = gorunus_kaygisi, fill = risk)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Görünüş Kaygısı", 
       y = "Yoğunluk",
       title = "Görünüş Kaygısı Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Yeme Bozukluğu Riski") + 
  scale_fill_discrete(labels = c("Var", "Yok")) +
  lims(x = c(0, 90), y = c(0, 0.05)) +
  theme_bw()


j <- ggplot(bozulmus_yeme_data, aes(x = aleksitimi, fill = risk)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Aleksitimi", 
       y = "Yoğunluk",
       title = "Aleksitimi Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Yeme Bozukluğu Riski") + 
  scale_fill_discrete(labels = c("Var", "Yok")) +
  lims(x = c(0, 90), y = c(0, 0.05)) +
  theme_bw()


k<- ggplot(bozulmus_yeme_data, aes(x = bastirma, fill = risk)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Bastırma", 
       y = "Yoğunluk",
       title = "Bastırma Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Yeme Bozukluğu Riski") + 
  scale_fill_discrete(labels = c("Var", "Yok")) +
  lims(x = c(0, 40), y = c(0, 0.10)) +
  theme_bw()


l <- ggplot(bozulmus_yeme_data, aes(x = yeniden_degerlendirme, fill = risk)) + 
  geom_density(alpha = 0.5) +
  labs(x = "Yeniden Değerlendirme", 
       y = "Yoğunluk",
       title = "Yeniden Değerlendirme Skorlarının Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Yeme Bozukluğu Riski") + 
  scale_fill_discrete(labels = c("Var", "Yok")) +
  lims(x = c(0, 50), y = c(0, 0.08)) +
  theme_bw()

grid.arrange(g, h, i, j, k, l, nrow = 2, ncol = 3)

#Korelasyonlarin Gorsellestirilmesi

cor_risk_var <- select(risk_var, 4:9)
cor_risk_yok <- select(risk_yok, 4:9)
cor_tum_grup <- select(bozulmus_yeme_data, 4:9)

# korelasyon analizleri icin kullanilacak datalar olusturuldu

cor_risk_var <- rename(cor_risk_var,
                       YDE ="yeniden_degerlendirme",
                       BAS = "bastirma",
                       GKY = "gorunus_kaygisi",
                       BYE = "bozulmus_yeme",
                       ALE = "aleksitimi",
                       DEP = "depresyon")

cor_risk_yok <- rename(cor_risk_yok,
                       YDE ="yeniden_degerlendirme",
                       BAS = "bastirma",
                       GKY = "gorunus_kaygisi",
                       BYE = "bozulmus_yeme",
                       ALE = "aleksitimi",
                       DEP = "depresyon")

cor_tum_grup <- rename(cor_tum_grup,
                       YDE ="yeniden_degerlendirme",
                       BAS = "bastirma",
                       GKY = "gorunus_kaygisi",
                       BYE = "bozulmus_yeme",
                       ALE = "aleksitimi",
                       DEP = "depresyon")

# Tabloda optimal degisken adlari icin adlari degistirildi

# ggcorrplot veya corrplot paketi korelasyonlarin gorsellestilmesi icin kullanilacaktir

corr_var <- round(cor(cor_risk_var), 1)
corr_yok <- round(cor(cor_risk_yok), 1)
corr_tum <- round(cor(cor_tum_grup), 1)

# yeme bozuklugu riski tasiyan, tasimayan ve tum grup icin korelasyon matrisleri olusturuldu

corrplot.mixed (corr_var, order = "AOE")
corrplot.mixed( corr_yok, order = "AOE")
corrplot.mixed(corr_tum, order = "AOE")

corrplot(corr_var, order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('PiYG'))

corrplot(corr_yok, order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('PiYG'))

corrplot(corr_tum, order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('PiYG'))

# iki farkli korelayon görsellestirmesi yapildi, bir tanesi secilecek ve postere konulacaktir
# yeme bozuklugu riski tasiyan, tasimayan ve tum grup icin korelasyonlar gorsellestirildi

table(bozulmus_yeme_data$cinsiyet)
table(bozulmus_yeme_data$risk)
mean(bozulmus_yeme_data$yas)

# grubun ozelliklerini tanitmak için frekans ve ortalama alindi


