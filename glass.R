library(tidyverse)
.<-read_csv("F:/IDS FINAL/glass.csv")
print(g)
head(base_glass)

names(base_glass)<-c("refractive index","sodium","magnesium","aluminium","silicon","potassium","calcium","barium","iron","type")
head(base_glass,5)

base_glass$`type`<-as_factor(base_glass$`type`)
base_glass$`type`

sum(is.na(base_glass))

base_glass <- na.omit(base_glass)

summary(base_glass)

base_glass[, 1:9] <- scale(base_glass[, 1:9])


set.seed(100)
dividir <- sample.split(Y = base_glass$type, SplitRatio = 0.70)
base_treinamento <- subset(x = base_glass, subset = dividir == TRUE)
base_teste <- subset(x = base_glass, subset = dividir == FALSE)

cor_base_glass<-cor(base_glass[,1:9])
cor_base_glass

library(class)
previsaoKNN <- knn(train = base_treinamento, test = base_teste, cl = base_treinamento$type, k = 1)
head(previsaoKNN,20)


library(caret)

matriz_confusao <- table(previsaoKNN, base_teste$type)
confusionMatrix(matriz_confusao)

accuracy <- trainControl(method = "cv", number = 10)
model <- train(type ~ ., data = base_glass, method = "knn", trControl = accuracy)
model$results 


library(cowplot)
graf_base_teste <- base_teste %>%
  ggplot() +
  geom_bar(mapping = aes(x = type, fill = type), color = "white") +
  ylim(0, 30) +
  ggtitle(label = "BASE TESTE")

previsaoKNN <- as_tibble(previsaoKNN)
names(previsaoKNN) <- c("type")

graf_previsaoKNN <- previsaoKNN %>%
  ggplot() +
  geom_bar(mapping = aes(x = type, fill = type), color = "white") +
  ylim(0, 30) +
  ggtitle(label = "PREVISAO KNN -98.4%")

plot_grid(graf_base_teste, graf_previsaoKNN, labels = "AUTO")

library(corrplot)

corrplot(cor_base_glass,method = "color",type = "upper",tl.srt = 45,tl.col = "black",addCoef.col = "#8B6969",col=c("#00688B","#00868B","#00CDCD","#BBFFFF","#E0FFFF","#FFFAFA","#B0E2FF","#87CEFF","#1C86EE","#104E8B","#00008B"))

library(ggplot2)
library(dplyr)
library(gridExtra)

maior_cor <- base_glass %>%
  ggplot() +
  geom_point(mapping = aes(x = `refractive index`, y = calcium), color = "red") +
  ggtitle("Highest correlation found - refractive index X calcium")

menor_cor <- base_glass %>%
  ggplot() +
  geom_point(mapping = aes(x = `refractive index`, y = barium), color = "blue") +
  ggtitle("Lowest correlation found - refractive index X barium")

grid.arrange(maior_cor, menor_cor)


base_glass %>% 
  ggplot()+
  geom_bar(mapping = aes(x = `type`,fill=`type`),color="white")


library(gridExtra)
grid.arrange(graf_base_teste,graf_previsaoKNN)

TP <- sum(previsaoKNN == "1" & base_teste$type == "1")
FP <- sum(previsaoKNN == "1" & base_teste$type == "2")
TN <- sum(previsaoKNN == "2" & base_teste$type == "2")
FN <- sum(previsaoKNN == "2" & base_teste$type == "1")


recall <- TP / (TP + FN)
precision <- TP / (TP + FP)

cat("Recall (Sensitivity):", recall, "\n")
cat("Precision:", precision, "\n")

