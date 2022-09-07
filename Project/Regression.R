# benötigte pakete
library("readxl")
library("tidyverse")
library("dplyr")
library(PerformanceAnalytics)
library(rpart)
library(rpart.plot)
library(car)
library(rattle)
library(ggplot2)
library(corrr)
library(tidyverse)
library(fpc)
library(cluster)


# house data
HouseData <- read_xlsx(path =  'House_Data.xlsx', col_names =  TRUE,sheet = 1 )

# löschen von Datum, id, zipcode und Position da unwichtig
HouseData <-select(HouseData, -id, -date, -zipcode, -lat,-long)
# umwndeln von Strings in Numerische Daten
# fehler beim einlesen, eine eigenart von R
HouseData$price <- as.numeric(as.character(HouseData$price))
HouseData$bedrooms <- as.numeric(as.character(HouseData$bedrooms))
HouseData$bathrooms <- as.numeric(as.character(HouseData$bathrooms))
HouseData$sqft_living <- as.numeric(as.character(HouseData$sqft_living))
HouseData$sqft_lot<- as.numeric(as.character(HouseData$sqft_lot))
HouseData$sqft_above <- as.numeric(as.character(HouseData$sqft_above ))
HouseData$sqft_basement<- as.numeric(as.character(HouseData$sqft_basement ))
HouseData$sqft_living15<- as.numeric(as.character(HouseData$sqft_living15 ))
HouseData$sqft_lot15<- as.numeric(as.character(HouseData$sqft_lot15 ))
HouseData$floors<- as.numeric(as.character(HouseData$floors))
HouseData$waterfront<- as.numeric(as.character(HouseData$waterfront))
HouseData$view<- as.numeric(as.character(HouseData$view))
HouseData$condition<- as.numeric(as.character(HouseData$condition))
HouseData$grade<- as.numeric(as.character(HouseData$grade))
HouseData$yr_built<- as.numeric(as.character(HouseData$yr_built))
HouseData$yr_renovated<- as.numeric(as.character(HouseData$yr_renovated))


# mittelwerte 
mean(HouseData$price)
sd(HouseData$price)
mean(HouseData$sqft_living)
sd(HouseData$sqft_living)
mean(HouseData$bedrooms)
sd(HouseData$bedrooms)
mean(HouseData$yr_built)
sd(HouseData$yr_built)
#  correlation house data
HouseData.cor<- correlate(HouseData)
# nur preis
HouseData.cor %>% 
  focus(price) %>% 
  arrange(price)

HouseData.cor %>%
  rearrange(method = "MDS", absolute = FALSE) %>%
  shave() %>% 
  rplot(shape = 15, colours = c("red", "green"))


trainingRowIndex <- sample(1:nrow(HouseData ), 0.75*nrow(HouseData ))  # 75 Prozent der Daten als Trainigsdaten
dtrain <- HouseData [trainingRowIndex, ] 


dtest  <- HouseData [-trainingRowIndex, ]   # Testdaten als restliche Daten
save(dtrain, dtest, file = "TrainTestData.RData")

#versuche für die erstellung eines Regressionsmodells
model <- lm(formula=price ~ bedrooms, data=dtrain)
summary(model)

bedrooms<- dtrain$bedrooms
price<- dtrain$price

ggplot(data = dtrain, aes(x = bedrooms, y =price) ) +
  geom_point()

ggplot(data = HouseData, aes(x = bedrooms, y =price )) +
geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

  
preds <- predict(model,dtest )
modelEval <- cbind(dtest$price, preds)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
modelEval
mse <- mean((as.numeric(as.character(modelEval$Actual)) - as.numeric(as.character(modelEval$Predicted)))^2)
rmse <- sqrt(mse)
rmse


fit_3 <- lm(price ~ bedrooms+ bathrooms, data = dtest)
summary(fit_3)
preds <- predict(fit_3,dtest )
modelEval <- cbind(dtest$price, preds)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
modelEval
mse <- mean((as.numeric(as.character(modelEval$Actual)) - as.numeric(as.character(modelEval$Predicted)))^2)
rmse <- sqrt(mse)
rmse

# regression für alle Parameter mit dem Preis durch price~.
fit_3 <- lm(price~. , data = dtest)
summary(fit_3)
preds <- predict(fit_3,dtest )
modelEval <- cbind(dtest$price, preds)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
modelEval
# Fehler
mse <- mean((as.numeric(as.character(modelEval$Actual)) - as.numeric(as.character(modelEval$Predicted)))^2)
rmse <- sqrt(mse)
rmse

# akuteller preis und prädiktion
Actual<-modelEval$Actual
Pred<-modelEval$Predicted

boxplot(Actual,Pred,
        main = "Vergleich Geschätzter und Tatsächlicher Preis",
        at = c(1,2),
        names = c("Realer Preis","Prädiktion"),
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

# Zeit progression
HouseData_Date <- read_xlsx(path =  'House_Data.xlsx', col_names =  TRUE,sheet = 1 )

HouseData_Date <-select(HouseData_Date, -id, -zipcode, -lat,-long)

HouseData_Date$price <- as.numeric(as.character(HouseData_Date$price))
HouseData_Date$bedrooms <- as.numeric(as.character(HouseData_Date$bedrooms))
HouseData_Date$bathrooms <- as.numeric(as.character(HouseData_Date$bathrooms))
HouseData_Date$sqft_living <- as.numeric(as.character(HouseData_Date$sqft_living))
HouseData_Date$sqft_lot<- as.numeric(as.character(HouseData_Date$sqft_lot))
HouseData_Date$sqft_above <- as.numeric(as.character(HouseData_Date$sqft_above ))
HouseData_Date$sqft_basement<- as.numeric(as.character(HouseData_Date$sqft_basement ))
HouseData_Date$sqft_living15<- as.numeric(as.character(HouseData_Date$sqft_living15 ))
HouseData_Date$sqft_lot15<- as.numeric(as.character(HouseData_Date$sqft_lot15 ))
HouseData_Date$floors<- as.numeric(as.character(HouseData_Date$floors))
HouseData_Date$waterfront<- as.numeric(as.character(HouseData_Date$waterfront))
HouseData_Date$view<- as.numeric(as.character(HouseData_Date$view))
HouseData_Date$condition<- as.numeric(as.character(HouseData_Date$condition))
HouseData_Date$grade<- as.numeric(as.character(HouseData_Date$grade))
HouseData_Date$yr_built<- as.numeric(as.character(HouseData_Date$yr_built))
HouseData_Date$yr_renovated<- as.numeric(as.character(HouseData_Date$yr_renovated))

HouseData_Date$date <- as.Date(as.character(HouseData_Date$date), "%Y%m%d")


# umwandeln in Monat
m <- months(seq.Date(as.Date("2013-06-01"), as.Date("2014-05-31"), "month"))
HouseData_Date$months <- factor(months(HouseData_Date$date), levels = m)

# plot x axis as ordered
boxplot(HouseData_Date$price ~ HouseData_Date$months, xlab="Monate",ylab="Preis",outline = FALSE)

# Neuskalierung
rescale_df <- HouseData %>%
  mutate(price_scale = scale(HouseData$price)) %>%
  select(-c(price))


# clusterertellung
# erst seed anwenden für Random Number Generator
# dann cluster erstellen
set.seed(2345)



kmeans_cluster<-kmeans(x = cbind(as.numeric(as.character(rescale_df$price_scale)),
                 as.numeric(as.character(rescale_df$bathrooms))), 
       5, iter.max = 20, nstart = 10) 



clusplot(rescale_df, kmeans_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
