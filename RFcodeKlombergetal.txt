#### RANDOMFOREST analysis for Primary and secondary pollinators for the whole mountain and per season and elevation ####
## Trait paper Klomberg et al. Seasonal and altitudinal changes in floral trait importance in tropical pollination systems##
# yannickklomberg@gmail.com #

## packages to load
if(!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(cluster)) {install.packages("cluster"); require(cluster)}
if(!require(magrittr)) {install.packages("magrittr"); require(magrittr)}
if(!require(ape)) {install.packages("ape"); require(ape)}
if(!require(vegan)) {install.packages("vegan"); require(vegan)}
if(!require(randomForest)) {install.packages("randomForest"); require(randomForest)}
if(!require(caret)) {install.packages("caret"); require(caret)}
if(!require(e1071)) {install.packages("e1071"); require(e1071)}
if(!require(readxl)) {install.packages("readxl"); require(readxl)}
if(!require(xlsx)) {install.packages("xlsx"); require(xlsx)}

## Load data 
data <- read_excel("./Input/Suppl.Material-5_Data.xlsx", 
                     +     sheet = "RF - Individual")
PriPoll <- read_excel("./Input/Suppl.Material-5_Data.xlsx", 
                      sheet = "RF- Primary Pollinator", na = "")
SecPoll <- read_excel("./Input/Suppl.Material-5_Data.xlsx", 
                      sheet = "RF- Secondary Pollinator", na = "")

# prepare to set categorical variables as factor#
set.seed(123)
cols <- c("Shape", "Symmetry",	"FL_Position",	"Ant_Position",	"Od_Strength",	"Colour",	"Nect_Guides")

###### ANALYSES PRIMARY AND SECONDARY POLLINATOR ######

#### Primary pollinators ####  
PriPoll3 <- as.data.frame(PriPoll) # create a new file to delete columns in as a safety
row.names(PriPoll3) <- PriPoll$SpCode # rename row names
PriPoll3$SpCode <- NULL # delete columns
PriPoll3 %<>% mutate_at(cols, funs(factor(.))) # set as factor
PriPoll3$PriPoll <- as.factor(PriPoll3$PriPoll) # set as factor

RF.PriPoll <- randomForest(PriPoll ~.,data = PriPoll3, proximity= TRUE, importance= TRUE, nPerm=100) #running RF with 100 permutations#
plot(RF.PriPoll) #Plot RF to see how many trees are needed to reduce variation#
update(RF.PriPoll, ntree = 500) #updating the model with the number of trees#

## find optimal MTRY (number of predictors tried at each split) using train function from the Caret package, however with our limited factors most often mtry will be 2 ##
set.seed(234)
train(PriPoll ~., data = PriPoll3, method = "rf")
update(RF.PriPoll, mtry=2)

##results RandomForest. Are written directly in an excel file in the Output folder, with the results rounded to 2 decimals.)
write.xlsx(round(importance(RF.PriPoll),2), file = "Output/Imp_PriPoll.xlsx")
## Partial dependance plot to see the importance of specific traits in the RF, only left here to get an idea of direction, not displayed in the paper ##
par(mfrow=c(4,3))
partialPlot(RF.PriPoll, PriPoll3, "Shape")
partialPlot(RF.PriPoll, PriPoll3, "Symmetry")
partialPlot(RF.PriPoll, PriPoll3, "FL_Position")
partialPlot(RF.PriPoll, PriPoll3, "Ant_Position")
partialPlot(RF.PriPoll, PriPoll3, "Od_Strength")
partialPlot(RF.PriPoll, PriPoll3, "Colour")
partialPlot(RF.PriPoll, PriPoll3, "Nect_Guides")
partialPlot(RF.PriPoll, PriPoll3, "Size")
partialPlot(RF.PriPoll, PriPoll3, "Tube_Length")
partialPlot(RF.PriPoll, PriPoll3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#### Secondary pollinators. Repeat the same as above for secondary pollinators ####
SecPoll$SecPoll = factor(SecPoll$SecPoll)
SecPoll3 <- as.data.frame(SecPoll)
row.names(SecPoll3) <- SecPoll$SpCode
SecPoll3$SpCode <- NULL
SecPoll3 %<>% mutate_at(cols, funs(factor(.)))

RF.SecPoll <- randomForest(SecPoll ~.,data = SecPoll3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.SecPoll)
update(RF.SecPoll, ntree = 500)

# find optimal MTRY (number of predictors tried at each split)
set.seed(223)
train(SecPoll ~., data = SecPoll3, method = "rf")
update(RF.SecPoll, mtry=2)

##results RF
write.xlsx(round(importance(RF.SecPoll),2), file = "Output/Imp_SecPoll.xlsx")
## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.SecPoll, SecPoll3, "Shape")
partialPlot(RF.SecPoll, SecPoll3, "Symmetry")
partialPlot(RF.SecPoll, SecPoll3, "FL_Position")
partialPlot(RF.SecPoll, SecPoll3, "Ant_Position")
partialPlot(RF.SecPoll, SecPoll3, "Od_Strength")
partialPlot(RF.SecPoll, SecPoll3, "Colour")
partialPlot(RF.SecPoll, SecPoll3, "Nect_Guides")
partialPlot(RF.SecPoll, SecPoll3, "Size")
partialPlot(RF.SecPoll, SecPoll3, "Tube_Length")
partialPlot(RF.SecPoll, SecPoll3, "SugarFlower")
#reset mfrow  
par(mfrow=c(1,1))

###### INDIVIDUAL ANALYSES PER SEASON AND ELEVATION ######
## Data preparation for indididual Randomforest analyses ##
dataN <- data #depending on which group you are running delete either Primary (PriPoll) or Secondary (SecPoll) pollinators #
dataN$PriPoll <- NULL
dataN$SecPoll <- NULL
names(dataN)[4] <-"Poll" #change the name of the pripoll or secpoll column to Poll for easier code reproduction

library(dplyr) # split data into separate files
MSDRY <- filter(dataN, Elev == "MS", Exp == "DRY")
MSWET <- filter(dataN, Elev == "MS", Exp == "WET")
CLDRY <- filter(dataN, Elev == "CL", Exp == "DRY")
CLWET <- filter(dataN, Elev == "CL", Exp == "WET")
PCDRY <- filter(dataN, Elev == "PC", Exp == "DRY")
PCWET <- filter(dataN, Elev == "PC", Exp == "WET")
DGDRY <- filter(dataN, Elev == "DG", Exp == "DRY")
DGWET <- filter(dataN, Elev == "DG", Exp == "WET")

# Secondary pollinator: remove empty rows without a SecPoll present before starting analyses#
MSDRY <- MSDRY[complete.cases(MSDRY[ , 4]),]
MSWET <- MSWET[complete.cases(MSWET[ , 4]),]
CLDRY <- CLDRY[complete.cases(CLDRY[ , 4]),]
CLWET <- CLWET[complete.cases(CLWET[ , 4]),]
PCDRY <- PCDRY[complete.cases(PCDRY[ , 4]),]
PCWET <- PCWET[complete.cases(PCWET[ , 4]),]
DGDRY <- DGDRY[complete.cases(DGDRY[ , 4]),]
DGWET <- DGWET[complete.cases(DGWET[ , 4]),]

##Random forest ##
#This has to be done separately for each elevation and season since the model has to be trained (ntree and MTRY) #
# Note: partial dependance plots were left in the code, but in the end not used in the paper. They help to show the direction of certain traits #

#MSDRY
set.seed(221)
MSDRY3 <- as.data.frame(MSDRY[,4:14])
row.names(MSDRY3) <- MSDRY$SpCode
MSDRY3 %<>% mutate_at(cols, funs(factor(.)))
RF.MSDRY <- randomForest(Poll ~.,data = MSDRY3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.MSDRY)
update(RF.MSDRY, ntree = 500)

        ## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = MSDRY3, method = "rf")
update(RF.MSDRY, mtry=2)

        ##results RF
write.xlsx(round(importance(RF.MSDRY),2), file = "importanceRF_ALL.xlsx", sheetName = "MSDRY", append =FALSE)

        ## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.MSDRY, MSDRY3, "Shape")
partialPlot(RF.MSDRY, MSDRY3, "Symmetry")
partialPlot(RF.MSDRY, MSDRY3, "FL_Position")
partialPlot(RF.MSDRY, MSDRY3, "Ant_Position")
partialPlot(RF.MSDRY, MSDRY3, "Od_Strength")
partialPlot(RF.MSDRY, MSDRY3, "Colour")
partialPlot(RF.MSDRY, MSDRY3, "Nect_Guides")
partialPlot(RF.MSDRY, MSDRY3, "Size")
partialPlot(RF.MSDRY, MSDRY3, "Tube_Length")
partialPlot(RF.MSDRY, MSDRY3, "SugarFlower")

          #reset mfrow  
par(mfrow=c(1,1))
    
#MSWET
set.seed(222)
MSWET3 <- as.data.frame(MSWET[,4:14])
row.names(MSWET3) <- MSWET$SpCode
MSWET3 %<>% mutate_at(cols, funs(factor(.)))
RF.MSWET <- randomForest(Poll ~.,data = MSWET3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.MSWET)
update(RF.MSWET, ntree = 500)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = MSWET3, method = "rf")
update(RF.MSWET, mtry=2)

##results RF
write.xlsx(round(importance(RF.MSWET),2), file = "importanceRF_ALL.xlsx", sheetName = "MSWET", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.MSWET, MSWET3, "Shape")
partialPlot(RF.MSWET, MSWET3, "Symmetry")
partialPlot(RF.MSWET, MSWET3, "FL_Position")
partialPlot(RF.MSWET, MSWET3, "Ant_Position")
partialPlot(RF.MSWET, MSWET3, "Od_Strength")
partialPlot(RF.MSWET, MSWET3, "Colour")
partialPlot(RF.MSWET, MSWET3, "Nect_Guides")
partialPlot(RF.MSWET, MSWET3, "Size")
partialPlot(RF.MSWET, MSWET3, "Tube_Length")
partialPlot(RF.MSWET, MSWET3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#CLDRY
set.seed(223)
CLDRY3 <- as.data.frame(CLDRY[,4:14])
row.names(CLDRY3) <- CLDRY$SpCode
CLDRY3 %<>% mutate_at(cols, funs(factor(.)))
RF.CLDRY <- randomForest(Poll ~.,data = CLDRY3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.CLDRY)
update(RF.CLDRY, ntree = 500)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = CLDRY3, method = "rf")
update(RF.CLDRY, mtry=2)

##results RF
write.xlsx(round(importance(RF.CLDRY),2), file = "importanceRF_ALL.xlsx", sheetName = "CLDRY", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.CLDRY, CLDRY3, "Shape")
partialPlot(RF.CLDRY, CLDRY3, "Symmetry")
partialPlot(RF.CLDRY, CLDRY3, "FL_Position")
partialPlot(RF.CLDRY, CLDRY3, "Ant_Position")
partialPlot(RF.CLDRY, CLDRY3, "Od_Strength")
partialPlot(RF.CLDRY, CLDRY3, "Colour")
partialPlot(RF.CLDRY, CLDRY3, "Nect_Guides")
partialPlot(RF.CLDRY, CLDRY3, "Size")
partialPlot(RF.CLDRY, CLDRY3, "Tube_Length")
partialPlot(RF.CLDRY, CLDRY3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#CLWET
set.seed(224)
CLWET3 <- as.data.frame(CLWET[,4:14])
row.names(CLWET3) <- CLWET$SpCode
CLWET3 %<>% mutate_at(cols, funs(factor(.)))
RF.CLWET <- randomForest(Poll ~.,data = CLWET3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.CLWET)
update(RF.CLWET, ntree = 300)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = CLWET3, method = "rf")
update(RF.CLWET, mtry=2)

##results RF
write.xlsx(round(importance(RF.CLWET),2), file = "importanceRF_ALL.xlsx", sheetName = "CLWET", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.CLWET, CLWET3, "Shape")
partialPlot(RF.CLWET, CLWET3, "Symmetry")
partialPlot(RF.CLWET, CLWET3, "FL_Position")
partialPlot(RF.CLWET, CLWET3, "Ant_Position")
partialPlot(RF.CLWET, CLWET3, "Od_Strength")
partialPlot(RF.CLWET, CLWET3, "Colour")
partialPlot(RF.CLWET, CLWET3, "Nect_Guides")
partialPlot(RF.CLWET, CLWET3, "Size")
partialPlot(RF.CLWET, CLWET3, "Tube_Length")
partialPlot(RF.CLWET, CLWET3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#PCDRY
set.seed(225)
PCDRY3 <- as.data.frame(PCDRY[,4:14])
row.names(PCDRY3) <- PCDRY$SpCode
PCDRY3 %<>% mutate_at(cols, funs(factor(.)))
RF.PCDRY <- randomForest(Poll ~.,data = PCDRY3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.PCDRY)
update(RF.PCDRY, ntree = 400)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = PCDRY3, method = "rf")
update(RF.PCDRY, mtry=2)

##results RF
write.xlsx(round(importance(RF.PCDRY),2), file = "importanceRF_ALL.xlsx", sheetName = "PCDRY", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.PCDRY, PCDRY3, "Shape")
partialPlot(RF.PCDRY, PCDRY3, "Symmetry")
partialPlot(RF.PCDRY, PCDRY3, "FL_Position")
partialPlot(RF.PCDRY, PCDRY3, "Ant_Position")
partialPlot(RF.PCDRY, PCDRY3, "Od_Strength")
partialPlot(RF.PCDRY, PCDRY3, "Colour")
partialPlot(RF.PCDRY, PCDRY3, "Nect_Guides")
partialPlot(RF.PCDRY, PCDRY3, "Size")
partialPlot(RF.PCDRY, PCDRY3, "Tube_Length")
partialPlot(RF.PCDRY, PCDRY3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#PCWET
set.seed(226)
PCWET3 <- as.data.frame(PCWET[,4:14])
row.names(PCWET3) <- PCWET$SpCode
PCWET3 %<>% mutate_at(cols, funs(factor(.)))
RF.PCWET <- randomForest(Poll ~.,data = PCWET3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.PCWET)
update(RF.PCWET, ntree = 400)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = PCWET3, method = "rf")
update(RF.PCWET, mtry=2)

##results RF
write.xlsx(round(importance(RF.PCWET),2), file = "importanceRF_ALL.xlsx", sheetName = "PCWET", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.PCWET, PCWET3, "Shape")
partialPlot(RF.PCWET, PCWET3, "Symmetry")
partialPlot(RF.PCWET, PCWET3, "FL_Position")
partialPlot(RF.PCWET, PCWET3, "Ant_Position")
partialPlot(RF.PCWET, PCWET3, "Od_Strength")
partialPlot(RF.PCWET, PCWET3, "Colour")
partialPlot(RF.PCWET, PCWET3, "Nect_Guides")
partialPlot(RF.PCWET, PCWET3, "Size")
partialPlot(RF.PCWET, PCWET3, "Tube_Length")
partialPlot(RF.PCWET, PCWET3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#DGDRY
set.seed(227)
DGDRY3 <- as.data.frame(DGDRY[,4:14])
row.names(DGDRY3) <- DGDRY$SpCode
DGDRY3 %<>% mutate_at(cols, funs(factor(.)))
RF.DGDRY <- randomForest(Poll ~.,data = DGDRY3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.DGDRY)
update(RF.DGDRY, ntree = 300)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = DGDRY3, method = "rf")
update(RF.DGDRY, mtry=2)

##results RF
write.xlsx(round(importance(RF.DGDRY),2), file = "importanceRF_ALL.xlsx", sheetName = "DGDRY", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.DGDRY, DGDRY3, "Shape")
partialPlot(RF.DGDRY, DGDRY3, "Symmetry")
partialPlot(RF.DGDRY, DGDRY3, "FL_Position")
partialPlot(RF.DGDRY, DGDRY3, "Ant_Position")
partialPlot(RF.DGDRY, DGDRY3, "Od_Strength")
partialPlot(RF.DGDRY, DGDRY3, "Colour")
partialPlot(RF.DGDRY, DGDRY3, "Nect_Guides")
partialPlot(RF.DGDRY, DGDRY3, "Size")
partialPlot(RF.DGDRY, DGDRY3, "Tube_Length")
partialPlot(RF.DGDRY, DGDRY3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

#DGWET
set.seed(228)
DGWET3 <- as.data.frame(DGWET[,4:14])
row.names(DGWET3) <- DGWET$SpCode
DGWET3 %<>% mutate_at(cols, funs(factor(.)))
RF.DGWET <- randomForest(Poll ~.,data = DGWET3, proximity= TRUE, importance= TRUE, nPerm=100)
plot(RF.DGWET)
update(RF.DGWET, ntree = 300)


## find optimal MTRY (number of predictors tried at each split)
set.seed(123)
train(Poll ~ ., data = DGWET3, method = "rf")
update(RF.DGWET, mtry=2)

##results RF
write.xlsx(round(importance(RF.DGWET),2), file = "importanceRF_ALL.xlsx", sheetName = "DGWET", append =TRUE)

## Partial dependance plot
par(mfrow=c(4,3))
partialPlot(RF.DGWET, DGWET3, "Shape")
partialPlot(RF.DGWET, DGWET3, "Symmetry")
partialPlot(RF.DGWET, DGWET3, "FL_Position")
partialPlot(RF.DGWET, DGWET3, "Ant_Position")
partialPlot(RF.DGWET, DGWET3, "Od_Strength")
partialPlot(RF.DGWET, DGWET3, "Colour")
partialPlot(RF.DGWET, DGWET3, "Nect_Guides")
partialPlot(RF.DGWET, DGWET3, "Size")
partialPlot(RF.DGWET, DGWET3, "Tube_Length")
partialPlot(RF.DGWET, DGWET3, "SugarFlower")

#reset mfrow  
par(mfrow=c(1,1))

###### PREDICTION ######
# You will first have to run the full network code for primary or secondary pollinators before being able to run this#

#### Primary pollinator prediction ####
# (Note: you need to match the levels of the variables, otherwise it will not work)#
# prediction MSDRY 
MSDRY4 <- MSDRY3[c(2:11)]
row.names(MSDRY4) <- MSDRY$SpCode
MSDRY4 %<>% mutate_at(cols2, funs(factor(.)))
MSDRY4$Shape <- factor(MSDRY4$Shape, levels = levels(PriPoll4$Shape))
MSDRY4$Colour <- factor(MSDRY4$Colour , levels = levels(PriPoll4$Colour))
MSDRY4$FL_Position <- factor(MSDRY4$FL_Position  , levels = levels(PriPoll4$FL_Position))
MSDRY4$Od_Strength <- factor(MSDRY4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
MSDRYPrediction <- predict(RF.PriPoll, MSDRY4, type="response")

#prediction MSWET
MSWET4 <- MSWET3[c(2:11)]
row.names(MSWET4) <- MSWET$SpCode
MSWET4 %<>% mutate_at(cols2, funs(factor(.)))
MSWET4$Shape <- factor(MSWET4$Shape, levels = levels(PriPoll4$Shape))
MSWET4$Colour <- factor(MSWET4$Colour , levels = levels(PriPoll4$Colour))
MSWET4$FL_Position <- factor(MSWET4$FL_Position  , levels = levels(PriPoll4$FL_Position))
MSWET4$Od_Strength <- factor(MSWET4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
MSWETPrediction <- predict(RF.PriPoll, MSWET4, type="response")

#prediction CLDRY
CLDRY4 <- CLDRY3[c(2:11)]
row.names(CLDRY4) <- CLDRY$SpCode
CLDRY4 %<>% mutate_at(cols2, funs(factor(.)))
CLDRY4$Shape <- factor(CLDRY4$Shape, levels = levels(PriPoll4$Shape))
CLDRY4$Colour <- factor(CLDRY4$Colour , levels = levels(PriPoll4$Colour))
CLDRY4$FL_Position <- factor(CLDRY4$FL_Position  , levels = levels(PriPoll4$FL_Position))
CLDRY4$Od_Strength <- factor(CLDRY4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
CLDRYPrediction <- predict(RF.PriPoll, CLDRY4, type="response")

#prediction CLWET
CLWET4 <- CLWET3[c(2:11)]
row.names(CLWET4) <- CLWET$SpCode
CLWET4 %<>% mutate_at(cols2, funs(factor(.)))
CLWET4$Shape <- factor(CLWET4$Shape, levels = levels(PriPoll4$Shape))
CLWET4$Colour <- factor(CLWET4$Colour , levels = levels(PriPoll4$Colour))
CLWET4$FL_Position <- factor(CLWET4$FL_Position  , levels = levels(PriPoll4$FL_Position))
CLWET4$Od_Strength <- factor(CLWET4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
CLWETPrediction <- predict(RF.PriPoll, CLWET4, type="response")

#prediction PCDRY
PCDRY4 <- PCDRY3[c(2:11)]
row.names(PCDRY4) <- PCDRY$SpCode
PCDRY4 %<>% mutate_at(cols2, funs(factor(.)))
PCDRY4$Shape <- factor(PCDRY4$Shape, levels = levels(PriPoll4$Shape))
PCDRY4$Colour <- factor(PCDRY4$Colour , levels = levels(PriPoll4$Colour))
PCDRY4$FL_Position <- factor(PCDRY4$FL_Position  , levels = levels(PriPoll4$FL_Position))
PCDRY4$Od_Strength <- factor(PCDRY4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
PCDRYPrediction <- predict(RF.PriPoll, PCDRY4, type="response")

#prediction PCWET
PCWET4 <- PCWET3[c(2:11)]
row.names(PCWET4) <- PCWET$SpCode
PCWET4 %<>% mutate_at(cols2, funs(factor(.)))
PCWET4$Shape <- factor(PCWET4$Shape, levels = levels(PriPoll4$Shape))
PCWET4$Colour <- factor(PCWET4$Colour , levels = levels(PriPoll4$Colour))
PCWET4$FL_Position <- factor(PCWET4$FL_Position  , levels = levels(PriPoll4$FL_Position))
PCWET4$Od_Strength <- factor(PCWET4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
PCWETPrediction <- predict(RF.PriPoll, PCWET4, type="response")

#prediction DGDRY
DGDRY4 <- DGDRY3[c(2:11)]
row.names(DGDRY4) <- DGDRY$SpCode
DGDRY4 %<>% mutate_at(cols2, funs(factor(.)))
DGDRY4$Shape <- factor(DGDRY4$Shape, levels = levels(PriPoll4$Shape))
DGDRY4$Colour <- factor(DGDRY4$Colour , levels = levels(PriPoll4$Colour))
DGDRY4$FL_Position <- factor(DGDRY4$FL_Position  , levels = levels(PriPoll4$FL_Position))
DGDRY4$Od_Strength <- factor(DGDRY4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
DGDRYPrediction <- predict(RF.PriPoll, DGDRY4, type="response")

#prediction DGWET
DGWET4 <- DGWET3[c(2:11)]
row.names(DGWET4) <- DGWET$SpCode
DGWET4 %<>% mutate_at(cols2, funs(factor(.)))
DGWET4$Shape <- factor(DGWET4$Shape, levels = levels(PriPoll4$Shape))
DGWET4$Colour <- factor(DGWET4$Colour , levels = levels(PriPoll4$Colour))
DGWET4$FL_Position <- factor(DGWET4$FL_Position  , levels = levels(PriPoll4$FL_Position))
DGWET4$Od_Strength <- factor(DGWET4$Od_Strength  , levels = levels(PriPoll4$Od_Strength))
DGWETPrediction <- predict(RF.PriPoll, DGWET4, type="response")

##write all to excel
write.xlsx(as.data.frame(cbind(MSDRY[,c(1:5)],MSDRYPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "MSDRY", append =FALSE)
write.xlsx(as.data.frame(cbind(MSWET[,c(1:5)],MSWETPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "MSWET", append =TRUE)
write.xlsx(as.data.frame(cbind(CLDRY[,c(1:5)],CLDRYPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "CLDRY", append =TRUE)
write.xlsx(as.data.frame(cbind(CLWET[,c(1:5)],CLWETPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "CLWET", append =TRUE)
write.xlsx(as.data.frame(cbind(PCDRY[,c(1:5)],PCDRYPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "PCDRY", append =TRUE)
write.xlsx(as.data.frame(cbind(PCWET[,c(1:5)],PCWETPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "PCWET", append =TRUE)
write.xlsx(as.data.frame(cbind(DGDRY[,c(1:5)],DGDRYPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "DGDRY", append =TRUE)
write.xlsx(as.data.frame(cbind(DGWET[,c(1:5)],DGWETPrediction)), file = "PriPoll_ALL.xlsx", sheetName = "DGWET", append =TRUE)

#### Secondary pollinator prediction ####
#(Note: you need to match the levels of the variables, otherwise it will not work)#
#prediction MSDRY #
MSDRY4 <- MSDRY3[c(2:11)]
row.names(MSDRY4) <- MSDRY$SpCode
MSDRY4 %<>% mutate_at(cols2, funs(factor(.)))
MSDRY4$Shape <- factor(MSDRY4$Shape, levels = levels(SecPoll4$Shape))
MSDRY4$Colour <- factor(MSDRY4$Colour , levels = levels(SecPoll4$Colour))
MSDRY4$FL_Position <- factor(MSDRY4$FL_Position  , levels = levels(SecPoll4$FL_Position))
MSDRY4$Od_Strength <- factor(MSDRY4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
MSDRYPrediction <- predict(RF.SecPoll, MSDRY4, type="response")

#prediction MSWET
MSWET4 <- MSWET3[c(2:11)]
row.names(MSWET4) <- MSWET$SpCode
MSWET4 %<>% mutate_at(cols2, funs(factor(.)))
MSWET4$Shape <- factor(MSWET4$Shape, levels = levels(SecPoll4$Shape))
MSWET4$Colour <- factor(MSWET4$Colour , levels = levels(SecPoll4$Colour))
MSWET4$FL_Position <- factor(MSWET4$FL_Position  , levels = levels(SecPoll4$FL_Position))
MSWET4$Od_Strength <- factor(MSWET4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
MSWETPrediction <- predict(RF.SecPoll, MSWET4, type="response")

#prediction CLDRY
CLDRY4 <- CLDRY3[c(2:11)]
row.names(CLDRY4) <- CLDRY$SpCode
CLDRY4 %<>% mutate_at(cols2, funs(factor(.)))
CLDRY4$Shape <- factor(CLDRY4$Shape, levels = levels(SecPoll4$Shape))
CLDRY4$Colour <- factor(CLDRY4$Colour , levels = levels(SecPoll4$Colour))
CLDRY4$FL_Position <- factor(CLDRY4$FL_Position  , levels = levels(SecPoll4$FL_Position))
CLDRY4$Od_Strength <- factor(CLDRY4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
CLDRYPrediction <- predict(RF.SecPoll, CLDRY4, type="response")

#prediction CLWET
CLWET4 <- CLWET3[c(2:11)]
row.names(CLWET4) <- CLWET$SpCode
CLWET4 %<>% mutate_at(cols2, funs(factor(.)))
CLWET4$Shape <- factor(CLWET4$Shape, levels = levels(SecPoll4$Shape))
CLWET4$Colour <- factor(CLWET4$Colour , levels = levels(SecPoll4$Colour))
CLWET4$FL_Position <- factor(CLWET4$FL_Position  , levels = levels(SecPoll4$FL_Position))
CLWET4$Od_Strength <- factor(CLWET4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
CLWETPrediction <- predict(RF.SecPoll, CLWET4, type="response")

#prediction PCDRY
PCDRY4 <- PCDRY3[c(2:11)]
row.names(PCDRY4) <- PCDRY$SpCode
PCDRY4 %<>% mutate_at(cols2, funs(factor(.)))
PCDRY4$Shape <- factor(PCDRY4$Shape, levels = levels(SecPoll4$Shape))
PCDRY4$Colour <- factor(PCDRY4$Colour , levels = levels(SecPoll4$Colour))
PCDRY4$FL_Position <- factor(PCDRY4$FL_Position  , levels = levels(SecPoll4$FL_Position))
PCDRY4$Od_Strength <- factor(PCDRY4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
PCDRYPrediction <- predict(RF.SecPoll, PCDRY4, type="response")

#prediction PCWET
PCWET4 <- PCWET3[c(2:11)]
row.names(PCWET4) <- PCWET$SpCode
PCWET4 %<>% mutate_at(cols2, funs(factor(.)))
PCWET4$Shape <- factor(PCWET4$Shape, levels = levels(SecPoll4$Shape))
PCWET4$Colour <- factor(PCWET4$Colour , levels = levels(SecPoll4$Colour))
PCWET4$FL_Position <- factor(PCWET4$FL_Position  , levels = levels(SecPoll4$FL_Position))
PCWET4$Od_Strength <- factor(PCWET4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
PCWETPrediction <- predict(RF.SecPoll, PCWET4, type="response")

#prediction DGDRY
DGDRY4 <- DGDRY3[c(2:11)]
row.names(DGDRY4) <- DGDRY$SpCode
DGDRY4 %<>% mutate_at(cols2, funs(factor(.)))
DGDRY4$Shape <- factor(DGDRY4$Shape, levels = levels(SecPoll4$Shape))
DGDRY4$Colour <- factor(DGDRY4$Colour , levels = levels(SecPoll4$Colour))
DGDRY4$FL_Position <- factor(DGDRY4$FL_Position  , levels = levels(SecPoll4$FL_Position))
DGDRY4$Od_Strength <- factor(DGDRY4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
DGDRYPrediction <- predict(RF.SecPoll, DGDRY4, type="response")

#prediction DGWET
DGWET4 <- DGWET3[c(2:11)]
row.names(DGWET4) <- DGWET$SpCode
DGWET4 %<>% mutate_at(cols2, funs(factor(.)))
DGWET4$Shape <- factor(DGWET4$Shape, levels = levels(SecPoll4$Shape))
DGWET4$Colour <- factor(DGWET4$Colour , levels = levels(SecPoll4$Colour))
DGWET4$FL_Position <- factor(DGWET4$FL_Position  , levels = levels(SecPoll4$FL_Position))
DGWET4$Od_Strength <- factor(DGWET4$Od_Strength  , levels = levels(SecPoll4$Od_Strength))
DGWET4$Ant_Position <- factor(DGWET4$Ant_Position  , levels = levels(SecPoll4$Ant_Position))
DGWETPrediction <- predict(RF.SecPoll, DGWET4, type="response")

##write all to excel
write.xlsx(as.data.frame(cbind(MSDRY[,c(1:5)],MSDRYPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "MSDRY", append =FALSE)
write.xlsx(as.data.frame(cbind(MSWET[,c(1:5)],MSWETPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "MSWET", append =TRUE)
write.xlsx(as.data.frame(cbind(CLDRY[,c(1:5)],CLDRYPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "CLDRY", append =TRUE)
write.xlsx(as.data.frame(cbind(CLWET[,c(1:5)],CLWETPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "CLWET", append =TRUE)
write.xlsx(as.data.frame(cbind(PCDRY[,c(1:5)],PCDRYPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "PCDRY", append =TRUE)
write.xlsx(as.data.frame(cbind(PCWET[,c(1:5)],PCWETPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "PCWET", append =TRUE)
write.xlsx(as.data.frame(cbind(DGDRY[,c(1:5)],DGDRYPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "DGDRY", append =TRUE)
write.xlsx(as.data.frame(cbind(DGWET[,c(1:5)],DGWETPrediction)), file = "SecPoll_ALL.xlsx", sheetName = "DGWET", append =TRUE)
