#### RANDOMFOREST analysis for Primary and secondary pollinators for the whole mountain and per season and elevation ####
## Trait paper Klomberg et al. Spatiotemporal shifts in the role of floral traits in shaping tropical plant-pollinator interactions. 
##
# yannickklomberg@gmail.com #

## packages to load
if(!require(readxl)) {install.packages("readxl"); require(readxl)}
if(!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(magrittr)) {install.packages("magrittr"); require(magrittr)}
if(!require(randomForest)) {install.packages("randomForest"); require(randomForest)}
if(!require(randomForestExplainer)) {install.packages("randomForestExplainer"); require(randomForestExplainer)}
if(!require(xlsx)) {install.packages("xlsx"); require(xlsx)}

## Load data 
data <- read_excel("./Input/Suppl.Material-5_Data.xlsx", 
                     +     sheet = "RF - Individual")
PriPoll <- read_excel("./Input/Suppl.Material-5_Data.xlsx", 
                      sheet = "RF- Primary Pollinator", na = "")
SecPoll <- read_excel("./Input/Suppl.Material-5_Data.xlsx", 
                      sheet = "RF- Secondary Pollinator", na = "")

# prepare to set categorical variables as factor#
cols <- c("Shape", "Symmetry",	"FL_Position",	"Ant_Position",	"Od_Strength",	"Colour",	"Nect_Guides")
###### ANALYSES PRIMARY AND SECONDARY POLLINATOR ######

#### Primary pollinators ####  
PriPoll3 <- as.data.frame(PriPoll) # create a new file to delete columns in as a safety
row.names(PriPoll3) <- PriPoll3$SpCode # rename row names
PriPoll3$SpCode <- NULL # delete columns
PriPoll3 %<>% mutate_at(cols, funs(factor(.))) # set as factor
PriPoll3$PriPoll <- as.factor(PriPoll3$PriPoll) # set as factor

set.seed(123)
RF.PriPoll <- randomForest(PriPoll ~.,data = PriPoll3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10)) #running RF with 100 permutations and mtry as the square root of the number of variables#
plot(RF.PriPoll) #Plot RF to see how many trees are needed to reduce variation#
update(RF.PriPoll, ntree = 500) #updating the model with the number of trees#

##results RandomForest. Are written directly in an excel file in the Output folder)
write.xlsx(importance(RF.PriPoll, scale = FALSE), file = "Imp_PriPoll.xlsx")


#### Secondary pollinators. Repeat the same as above for secondary pollinators ####
SecPoll$SecPoll = factor(SecPoll$SecPoll)
SecPoll3 <- as.data.frame(SecPoll)
row.names(SecPoll3) <- SecPoll$SpCode
SecPoll3$SpCode <- NULL
SecPoll3 %<>% mutate_at(cols, funs(factor(.)))

set.seed(1234)
RF.SecPoll <- randomForest(SecPoll ~.,data = SecPoll3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.SecPoll)
update(RF.SecPoll, ntree = 500)

##results RF
write.xlsx(importance(RF.SecPoll, scale = FALSE), file = "Imp_SecPoll.xlsx")

###### INDIVIDUAL ANALYSES PER SEASON AND ELEVATION ######
## Data preparation for indididual Randomforest analyses ##
dataN <- data #depending on which group you are running delete either Primary (PriPoll) or Secondary (SecPoll) pollinators #
dataN$PriPoll <- NULL
dataN$SecPoll <- NULL
names(dataN)[4] <-"Poll" #change the name of the pripoll or secpoll column to Poll for easier code reproduction
cols2 <- c("Poll","Shape", "Symmetry",	"FL_Position",	"Ant_Position",	"Od_Strength",	"Colour",	"Nect_Guides") #Include Poll in Factor transformation

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
#This has to be done separately for each elevation and season since the model has to be trained (ntree) #

#MSDRY
set.seed(221)
MSDRY3 <- as.data.frame(MSDRY[,4:14])
row.names(MSDRY3) <- MSDRY$SpCode
MSDRY3 %<>% mutate_at(cols2, funs(factor(.)))
RF.MSDRY <- randomForest(Poll ~.,data = MSDRY3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.MSDRY)
update(RF.MSDRY, ntree = 400)

        ##results RF
write.xlsx(importance(RF.MSDRY, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "MSDRY", append =FALSE)

#MSWET
set.seed(222)
MSWET3 <- as.data.frame(MSWET[,4:14])
row.names(MSWET3) <- MSWET$SpCode
MSWET3 %<>% mutate_at(cols2, funs(factor(.)))
RF.MSWET <- randomForest(Poll ~.,data = MSWET3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.MSWET)
update(RF.MSWET, ntree = 500)

##results RF
write.xlsx(importance(RF.MSWET, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "MSWET", append =TRUE)

#CLDRY
set.seed(223)
CLDRY3 <- as.data.frame(CLDRY[,4:14])
row.names(CLDRY3) <- CLDRY$SpCode
CLDRY3 %<>% mutate_at(cols2, funs(factor(.)))
RF.CLDRY <- randomForest(Poll ~.,data = CLDRY3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.CLDRY)
update(RF.CLDRY, ntree = 500)

##results RF
write.xlsx(importance(RF.CLDRY, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "CLDRY", append =TRUE)

#CLWET
set.seed(224)
CLWET3 <- as.data.frame(CLWET[,4:14])
row.names(CLWET3) <- CLWET$SpCode
CLWET3 %<>% mutate_at(cols2, funs(factor(.)))
RF.CLWET <- randomForest(Poll ~.,data = CLWET3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.CLWET)
update(RF.CLWET, ntree = 400)

##results RF
write.xlsx(importance(RF.CLWET, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "CLWET", append =TRUE)

#PCDRY
set.seed(225)
PCDRY3 <- as.data.frame(PCDRY[,4:14])
row.names(PCDRY3) <- PCDRY$SpCode
PCDRY3 %<>% mutate_at(cols2, funs(factor(.)))
RF.PCDRY <- randomForest(Poll ~.,data = PCDRY3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.PCDRY)
update(RF.PCDRY, ntree = 500)

##results RF
write.xlsx(importance(RF.PCDRY, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "PCDRY", append =TRUE)

#PCWET
set.seed(226)
PCWET3 <- as.data.frame(PCWET[,4:14])
row.names(PCWET3) <- PCWET$SpCode
PCWET3 %<>% mutate_at(cols2, funs(factor(.)))
RF.PCWET <- randomForest(Poll ~.,data = PCWET3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.PCWET)
update(RF.PCWET, ntree = 500)

##results RF
write.xlsx(importance(RF.PCWET, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "PCWET", append =TRUE)

#DGDRY
set.seed(227)
DGDRY3 <- as.data.frame(DGDRY[,4:14])
row.names(DGDRY3) <- DGDRY$SpCode
DGDRY3 %<>% mutate_at(cols2, funs(factor(.)))
RF.DGDRY <- randomForest(Poll ~.,data = DGDRY3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.DGDRY)
update(RF.DGDRY, ntree = 500)

##results RF
write.xlsx(importance(RF.DGDRY, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "DGDRY", append =TRUE)

#DGWET
set.seed(228)
DGWET3 <- as.data.frame(DGWET[,4:14])
row.names(DGWET3) <- DGWET$SpCode
DGWET3 %<>% mutate_at(cols2, funs(factor(.)))
RF.DGWET <- randomForest(Poll ~.,data = DGWET3, proximity= TRUE, importance= TRUE, nPerm=100, mtry=sqrt(10))
plot(RF.DGWET)
update(RF.DGWET, ntree = 300)

##results RF
write.xlsx(importance(RF.DGWET, scale = FALSE), file = "importanceRF_ALL.xlsx", sheetName = "DGWET", append =TRUE)
