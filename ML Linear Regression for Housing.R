options(scipen = 999)

set.seed(1)


library(readxl)
myData <- read_excel("House Price.xlsx")



Ames <- subset(myData, myData$Town == 'Ames, IA' &
                 myData$Type == 'Single Family')

View(Ames)

Ames$New <- ifelse(Ames$Build_year >= 2000, 1, 0)


Complete <- lm(Sale_amount ~ Beds + Baths + Sqft_home + Sqft_lot + New,  data=Ames)
summary(Complete)


Restricted <- lm(Sale_amount ~ Baths + Sqft_home + Sqft_lot + New,  data=Ames)
summary(Restricted)


summary(Complete)$adj.r.squared
summary(Restricted)$adj.r.squared


Restricted_predict <- predict(Restricted, data.frame(Baths = 2, Sqft_home = 1600, Sqft_lot = 15000, New = 1)) #lm(Sale_amount ~ 2*(Baths) + 1600*(Sqft_home) + 15000*(Sqft_lot) + 1*(New),  data=Ames))


Ames_T <- Ames[1:140,]; Ames_D <- Ames[141:209,]



Model1_Complete <- lm(Sale_amount ~ Beds + Baths + Sqft_home + Sqft_lot + New,  data=Ames_T)
summary(Model1_Complete)

Pred1 <- predict(Model1_Complete, Ames_D)
sqrt(mean((Ames_D$Sale_amount-Pred1)^2))


Model2_Restricted <- lm(Sale_amount ~ Baths + Sqft_home + Sqft_lot + New,  data=Ames_T)
summary(Model2_Restricted)


Pred2 <- predict(Model2_Restricted, Ames_D)
sqrt(mean((Ames_D$Sale_amount-Pred2)^2))


install.packages("caret")

library(caret)
myControl <- trainControl(method="cv", number = 5)

Result1 <- train(Sale_amount ~ Beds + Baths + Sqft_home + Sqft_lot + New,  data=Ames, method='lm', trControl = myControl)
Result1

Result2 <- train(Sale_amount ~ Baths + Sqft_home + Sqft_lot + New,  data=Ames, method='lm', trControl = myControl)
Result2
summary(Result2)


#mean(Ames2$Sqft_lot)
#Ames2 <- Ames2[ -c(13) ]

Ames2$New1 <- ifelse(Ames$Build_year >= 2000, 1, 0)
Ames2$New2 <- ifelse(Ames$Sqft_lot >= 12000, 1, 0)


myControl <- trainControl(method="cv", number = 5)
Result3 <- train(Sale_amount ~ Baths + Sqft_home + Sqft_lot + New1 + New2,  data=Ames2, method='lm', trControl = myControl)
Result3
summary(Result3)

#summary(Result1)$adj.r.squared
#summary(Result2)$adj.r.squared
#summary(Result3)$adj.r.squared
