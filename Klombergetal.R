#### RANDOMFOREST analysis for Primary and secondary pollinators for the whole mountain and per season and elevation ####
## Trait paper Klomberg et al. Spatiotemporal variation in the role of floral traits in shaping tropical plant-pollinator interactions. 
##
# yannickklomberg@gmail.com #

## packages to load
if(!require(readxl)) {install.packages("readxl"); require(readxl)}
if(!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(magrittr)) {install.packages("magrittr"); require(magrittr)}
if(!require(randomForest)) {install.packages("randomForest"); require(randomForest)}
if(!require(caret)) {install.packages("caret"); require(caret)}
if(!require(xlsx)) {install.packages("xlsx"); require(xlsx)}
if(!require(xlsx2dfs)) {install.packages("xlsx2dfs"); require(xlsx2dfs)}
if(!require(MASS)) {install.packages("MASS"); require(MASS)}
if(!require(rfUtilities)) {install.packages("rfUtilities"); require(rfUtilities)}

setwd("C:/Users/taborp00/YannickClanek")

## Load data 

data <- read_excel("C:/Users/taborp00/YannickClanek/Data-Klomberg-et-al-2021.xlsx", sheet = "RF - all together")

# prepare to set categorical variables as factor#
cols <- c("PriPoll","Shape", "Symmetry",	"FL_Position",	"Ant_Position",	"Od_Strength",	"Colour",	"Nect_Guides")
PriPoll3 <- as.data.frame(data) # create a new file to delete columns in as a safety
PriPoll3$SpCode <- NULL # delete columns
PriPoll3$SecPoll <- NULL # delete columns
PriPoll3$Incl_PriPoll<-NULL# delete columns
PriPoll3[cols] <- lapply(PriPoll3[cols] , factor)# convert cols in factor

#### Whole model with all available traits ####  
# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
#tuning mtry
set.seed(222)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = PriPoll3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300,500,1000,1500,2000,5000)) {
  set.seed(999)
  rf_maxtrees <- train(PriPoll~.,
                       data = PriPoll3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

############fit the final model
set.seed(226)
RF.PriPoll <- randomForest(PriPoll~ .,data = PriPoll3,metric = "Accuracy",importance= TRUE,localImp=TRUE,
                            mtry=2, ntree=500)
RF.PriPoll
varImpPlot(RF.PriPoll,type=1)
RF.PriPoll$importance
RF.PriPoll$importanceSD

set.seed(222)
rf.perm <- rf.significance(RF.PriPoll, PriPoll3[,2:11], nperm=499, ntree=501)
rf.perm


###########################Whole model including only the 3 best traits##################
PriPoll3[cols] <- lapply(PriPoll3[cols] , factor)# convert cols in factor
PriPoll2<-PriPoll3[,c(	"PriPoll","Colour",	"SugarFlower", "Size" )]
# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
#tuning mtry
set.seed(1118)
tuneGrid <- expand.grid(.mtry = c(1: 3))
rf_mtry <- train(PriPoll~.,
                 data = PriPoll2,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
)

print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
set.seed(224)
store_maxtrees <- list()
for (ntree in c(300, 500,1000,  1500, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(PriPoll~.,
                       data = PriPoll2,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit the final model
set.seed(7538)# 
RF.PriPoll <- randomForest(PriPoll~ .,data = PriPoll2,  metric = "Accuracy",importance= TRUE,
                           mtry=1, ntree=500)

RF.PriPoll
importance(RF.PriPoll, Type=1, scale=FALSE)
varImpPlot(RF.PriPoll,type=1)
RF.PriPoll$importanceSD


set.seed(100)
rf.perm <- rf.significance(RF.PriPoll, PriPoll2[,2:4], nperm=499, ntree=501)
rf.perm
plot(rf.perm)


###### INDIVIDUAL ANALYSES PER SEASON AND ELEVATION ######
## Data preparation for indididual Randomforest analyses ##
data <- read_excel("C:/Users/taborp00/YannickClanek/Data-Klomberg-et-al-2021.xlsx", sheet = "RF - Individual networks")
# prepare to set categorical variables as factor#
cols3 <- c("Elevation","Season", "Shape", "Symmetry",	"FL_Position",	"Ant_Position",	"Od_Strength",	"Colour",	"Nect_Guides") #Include Poll in Factor transformation
data[cols3] <- lapply(data[cols3] , factor)

dataN <- data 

 # split data into separate files
MSDRY <- filter(dataN, Elevation == "2200", Season == "DRY")
MSWET <- filter(dataN, Elevation == "2200", Season == "WET")
CLDRY <- filter(dataN, Elevation == "1450", Season == "DRY")
CLWET <- filter(dataN, Elevation == "1450", Season == "WET")
PCDRY <- filter(dataN, Elevation == "1100", Season == "DRY")
PCWET <- filter(dataN, Elevation == "1100", Season == "WET")
DGDRY <- filter(dataN, Elevation == "650", Season == "DRY")
DGWET <- filter(dataN, Elevation == "650", Season == "WET")
WET<-filter(dataN, Season =="WET")
DRY<-filter(dataN, Season =="DRY")
MS <- filter(dataN, Elevation == "2200")
CL <- filter(dataN, Elevation == "1450")
PC <- filter(dataN, Elevation == "1100")
DG <- filter(dataN, Elevation == "650")

##Random forest ##
#This has to be done separately for each elevation and season 

######################2200m DRY###################
set.seed(222)
MSDRY3 <- as.data.frame(MSDRY[,4:14])
# prepare to set categorical variables as factor#
cols3 <- c("PriPoll","Shape", "Symmetry",	"FL_Position",	"Ant_Position",	"Od_Strength",	"Colour",	"Nect_Guides")
MSDRY3[cols3] <- lapply(MSDRY3[cols3] , factor)
row.names(MSDRY3) <- MSDRY3$SpCode
MSDRY3$SpCode <- NULL # delete column
MSDRY3 <- MSDRY3[complete.cases(MSDRY3),]

############tune RF.MSDRY##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")

set.seed(333)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = MSDRY3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300,500,1000, 2000,5000,10000)) {
  set.seed(220)
  rf_maxtrees <- train(PriPoll~.,
                       data = MSDRY3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed((222))
RF.MSDRY <- randomForest(PriPoll ~.,data = MSDRY3, proximity= TRUE, importance= TRUE, mtry=4, ntree=300)
RF.MSDRY
RF.MSDRY$importance
RF.MSDRY$importanceSD
plot(RF.MSDRY)
varImpPlot(RF.MSDRY,type=1)

set.seed(468)
rf.perm <- rf.significance(RF.MSDRY, MSDRY3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)
RF.MSDRY$importance
partialPlot(RF.MSDRY,MSDRY3,Ant_Position,"Bees")
partialPlot(RF.MSDRY,MSDRY3,SugarFlower,"Flies")

###############################2200m WET###################
set.seed(12)
MSWET3 <- as.data.frame(MSWET[,4:14])
row.names(MSWET3) <- MSWET$SpCode
MSWET3[cols3] <- lapply(MSWET3[cols3] , factor)
row.names(MSWET3) <- MSWET3$SpCode
MSWET3$SpCode <- NULL # delete column
MSWET3 <- MSWET3[complete.cases(MSWET3[ , 4]),]
MSWET3 <- MSWET3[complete.cases(MSWET3),]

set.seed(500)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = MSWET3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300,500,1000,2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = MSWET3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed((500))
RF.MSWET <- randomForest(PriPoll ~.,data = MSWET3, proximity= TRUE, importance= TRUE, mtry=1, ntree=300)
RF.MSWET
RF.MSWET$importance
RF.MSWET$importanceSD
plot(RF.MSWET)
varImpPlot(RF.MSWET,type=1)

set.seed(300)
rf.perm <- rf.significance(RF.MSWET, MSWET3[-1], nperm=199, ntree=301)
rf.perm

partialPlot(RF.MSWET,MSWET3,SugarFlower,"Flies")
partialPlot(RF.MSWET,MSWET3,SugarFlower,"Birds")
partialPlot(RF.MSWET,MSWET3,Colour,"Moths")


#####################1450m DRY############################
set.seed(224)
CLDRY3 <- as.data.frame(CLDRY[,4:14])
row.names(CLDRY3) <- CLDRY$SpCode
CLDRY3[cols3] <- lapply(CLDRY3[cols3] , factor)
row.names(CLDRY3) <- CLDRY3$SpCode
CLDRY3$SpCode <- NULL # delete column
CLDRY3 <- CLDRY3[complete.cases(CLDRY3[ , 4]),]
CLDRY3 <- CLDRY3[complete.cases(CLDRY3),]

############tune RF.CLDRY##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = CLDRY3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE
                )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000,15000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = CLDRY3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(600)
RF.CLDRY <- randomForest(PriPoll ~.,data = CLDRY3, proximity= TRUE, importance= TRUE,  mtry=2, ntree=300)
RF.CLDRY
RF.CLDRY$importance
RF.CLDRY$importanceSD
varImpPlot(RF.CLDRY)

set.seed(789)
rf.perm <- rf.significance(RF.CLDRY, CLDRY3[-1], nperm=199, ntree=301)
rf.perm
plot(rf.perm)
partialPlot(RF.CLDRY,CLDRY3,Tube_Length,"Flies")
partialPlot(RF.CLDRY,CLDRY3,Tube_Length,"Bees")
partialPlot(RF.CLDRY,CLDRY3,Symmetry,"Moths")
#####################1450m WET####################
set.seed(224)
CLWET3 <- as.data.frame(CLWET[,4:14])
row.names(CLWET3) <- CLWET$SpCode
CLWET3[cols3] <- lapply(CLWET3[cols3] , factor)
row.names(CLWET3) <- CLWET3$SpCode
CLWET3$SpCode <- NULL # delete column
CLWET3 <- CLWET3[complete.cases(CLWET3[ , 4]),]
CLWET3 <- CLWET3[complete.cases(CLWET3),]

############tune RF.CLWET##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = CLWET3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                                  )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = CLWET3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(465)
RF.CLWET <- randomForest(PriPoll ~.,data = CLWET3, proximity= TRUE, importance= TRUE, mtry=1, ntree=300)
RF.CLWET
RF.CLWET$importance
RF.CLWET$importanceSD
varImpPlot(RF.CLWET)

set.seed(222)
rf.perm <- rf.significance(RF.CLWET, CLWET3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)
RF.CLWET$importance
partialPlot(RF.CLWET,CLWET3,Od_Strength,"Flies")
partialPlot(RF.CLWET,CLWET3,Od_Strength,"Moths")
partialPlot(RF.CLWET,CLWET3,Colour,"Bees")

#####################1100m DRY####################
set.seed(225)
PCDRY3 <- as.data.frame(PCDRY[,4:14])
row.names(PCDRY3) <- PCDRY$SpCode
PCDRY3[cols3] <- lapply(PCDRY3[cols3] , factor)
row.names(PCDRY3) <- PCDRY3$SpCode
PCDRY3$SpCode <- NULL # delete column
PCDRY3 <- PCDRY3[complete.cases(PCDRY3),]

############tune RF.PCDRY##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(220)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = PCDRY3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = PCDRY3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(600)
RF.PCDRY <- randomForest(PriPoll ~.,data = PCDRY3, proximity= TRUE, importance= TRUE, mtry=5, ntree=300)
RF.PCDRY
RF.PCDRY$importance
RF.PCDRY$importanceSD

set.seed((467))
rf.perm <- rf.significance(RF.PCDRY, PCDRY3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)

partialPlot(RF.PCDRY,PCDRY3,Size,"Bees")
partialPlot(RF.PCDRY,PCDRY3,Size,"Flies")
partialPlot(RF.PCDRY,PCDRY3,Shape,"Butterflies")
partialPlot(RF.PCDRY,PCDRY3,Colour,"Moths")
#################1100m WET###################
set.seed(226)
PCWET3 <- as.data.frame(PCWET[,4:14])
PCWET3[cols3] <- lapply(PCWET3[cols3] , factor)
row.names(PCWET3) <- PCWET3$SpCode
PCWET3$SpCode <- NULL # delete column
PCWET3 <- PCWET3[complete.cases(PCWET3),]

############tune RF.PCWET##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = PCWET3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = PCWET3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(333)
RF.PCWET <- randomForest(PriPoll ~.,data = PCWET3, proximity= TRUE, importance= TRUE, mtry=1, ntree=300)
RF.PCWET
RF.PCWET$importance
RF.PCWET$importanceSD
varImpPlot(RF.PCWET)

set.seed((467))
rf.perm <- rf.significance(RF.PCWET, PCWET3[-1], nperm=299, ntree=301)
rf.perm 
plot(rf.perm)
RF.PCWET$importance
partialPlot(RF.PCWET,PCWET3,Nect_Guides,"Bees")
partialPlot(RF.PCWET,PCWET3,SugarFlower,"Flies")
partialPlot(RF.PCWET,PCWET3,SugarFlower,"Birds")

########################650m DRY#################
set.seed(227)
DGDRY3 <- as.data.frame(DGDRY[,4:14])
DGDRY3[cols3] <- lapply(DGDRY3[cols3] , factor)
row.names(DGDRY3) <- DGDRY3$SpCode
DGDRY3$SpCode <- NULL # delete column
DGDRY3 <- DGDRY3[complete.cases(DGDRY3),]

############tune RF.DGDRY##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = DGDRY3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = DGDRY3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                      ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(467)
RF.DGDRY <- randomForest(PriPoll ~.,data = DGDRY3, proximity= TRUE, importance= TRUE, mtry=2, ntree=300)
RF.DGDRY
RF.DGDRY$importance
RF.DGDRY$importanceSD
varImpPlot(RF.DGDRY)

set.seed((467))
rf.perm <- rf.significance(RF.DGDRY, DGDRY3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)

partialPlot(RF.DGDRY,DGDRY3,SugarFlower,"Bees")
partialPlot(RF.DGDRY,DGDRY3,Size,"Butterflies")
partialPlot(RF.DGDRY,DGDRY3,Shape,"Flies")
partialPlot(RF.DGDRY,DGDRY3,Colour,"Moths")
#################650m WET#############################
DGWET3 <- as.data.frame(DGWET[,4:14])
DGWET3[cols3] <- lapply(DGWET3[cols3] , factor)
row.names(DGWET3) <- DGWET3$SpCode
DGWET3$SpCode <- NULL # delete column
DGWET3 <- DGWET3[complete.cases(DGWET3),]

############tune RF.DGWET##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data = DGWET3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = DGWET3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF

set.seed(782)
RF.DGWET <- randomForest(PriPoll ~.,data = DGWET3, proximity= TRUE, importance= TRUE, mtry=1, ntree=300)
RF.DGWET
RF.DGWET$importance
RF.DGWET$importanceSD
importance(RF.DGWET, Type=1, scale=TRUE)
varImpPlot(RF.DGWET)

set.seed((478))
rf.perm <- rf.significance(RF.DGWET, DGWET3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)
RF.DGWET$importance
partialPlot(RF.DGWET,DGWET3,Size,"Bees")
partialPlot(RF.DGWET,DGWET3,Tube_Length,"Moths")
##########################DRY################
DRY3<- as.data.frame(DRY[,4:14])
DRY3[cols3] <- lapply(DRY3[cols3] , factor)
row.names(DRY3) <- DRY3$SpCode
DRY3$SpCode <- NULL # delete column
DRY3 <- DRY3[complete.cases(DRY3),]


############tune RF.DRY##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data =DRY3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500, 1000, 2000,5000,10000)) {
  set.seed(226)
  rf_maxtrees <- train(PriPoll~.,
                       data = DRY3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(782)
RF.DRY <- randomForest(PriPoll ~.,data = DRY3, proximity= TRUE, importance= TRUE, mtry=2, ntree=500)
RF.DRY
RF.DRY$importance
RF.DRY$importanceSD
importance(RF.DRY, Type=1, scale=TRUE)
varImpPlot(RF.DRY)

set.seed(400)
rf.perm <- rf.significance(RF.DRY, DRY3[-1], nperm=199, ntree=501)
rf.perm 
plot(rf.perm)
RF.DRY$importance
partialPlot(RF.DRY,DRY3,Colour,"Bees")
partialPlot(RF.DRY,DRY3,SugarFlower,"Birds")
partialPlot(RF.DRY,DRY3,Ant_Position,"Flies")
partialPlot(RF.DRY,DRY3,Size,"Butterflies")
partialPlot(RF.DRY,DRY3,Colour,"Moths")
##########################WET################
WET3<- as.data.frame(WET[,4:14])
WET3[cols3] <- lapply(WET3[cols3] , factor)
row.names(WET3) <- WET3$SpCode
WET3$SpCode <- NULL # delete column
WET3 <- WET3[complete.cases(WET3),]

############tune RF.WET##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")

set.seed(4)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data =WET3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500, 1000, 2000,5000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = WET3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(782)
RF.WET <- randomForest(PriPoll ~.,data = WET3, proximity= TRUE, importance= TRUE, mtry=8, ntree=1000)
RF.WET
RF.WET$importance
RF.WET$importanceSD
importance(RF.WET, Type=1, scale=TRUE)
varImpPlot(RF.WET)
accuracy(RF.WET$confusion[1:4,1:4])
set.seed((467))
rf.perm <- rf.significance(RF.WET, WET3[-1], nperm=199, ntree=1001)
rf.perm 
plot(rf.perm)

partialPlot(RF.WET,WET3,SugarFlower,"Bees")
partialPlot(RF.WET,WET3,SugarFlower,"Flies")
partialPlot(RF.WET,WET3,SugarFlower,"Birds")
partialPlot(RF.WET,WET3,Size,"Moths")

################################ 2200m #####################
MS3<- as.data.frame(MS[,4:14])
MS3[cols3] <- lapply(MS3[cols3] , factor)
row.names(MS3) <- MS3$SpCode
MS3$SpCode <- NULL # delete column
MS3 <- MS3[complete.cases(MS3),]

############tune RF.MS##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(220)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data =MS3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = MS3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(107)
RF.MS <- randomForest(PriPoll ~.,data = MS3, proximity= TRUE, importance= TRUE, mtry=5, ntree=300)
RF.MS
RF.MS$importance
RF.MS$importanceSD
importance(RF.MS, Type=1, scale=TRUE)
varImpPlot(RF.MS)
set.seed(467)
rf.perm <- rf.significance(RF.MS, MS3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)
partialPlot(RF.MS,MS3,SugarFlower,"Bees")
partialPlot(RF.MS,MS3,SugarFlower,"Flies")
partialPlot(RF.MS,MS3,SugarFlower,"Birds")
partialPlot(RF.MS,MS3,Colour,"Moths")

############################### 1450m ###########################

CL3<- as.data.frame(CL[,4:14])
CL3[cols3] <- lapply(CL3[cols3] , factor)
row.names(CL3) <- CL3$SpCode
CL3$SpCode <- NULL # delete column
CL3 <- CL3[complete.cases(CL3),]

############tune RF.CL##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(222)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data =CL3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,5000,10000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = CL3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(226)
RF.CL <- randomForest(PriPoll ~.,data = CL3, proximity= TRUE, importance= TRUE, mtry=2, ntree=500)
RF.CL
RF.CL$importance
RF.CL$importanceSD
importance(RF.CL, Type=1, scale=TRUE)
varImpPlot(RF.CL)

set.seed(467)
rf.perm <- rf.significance(RF.CL, CL3[-1], nperm=199, ntree=501)
rf.perm 
plot(rf.perm)
partialPlot(RF.CL,CL3,Colour,"Bees")
partialPlot(RF.CL,CL3,Size,"Flies")
partialPlot(RF.CL,CL3,SugarFlower,"Birds")
partialPlot(RF.CL,CL3,Tube_Length,"Moths")

########################### 1100m ################
PC3<- as.data.frame(PC[,4:14])
PC3[cols3] <- lapply(PC3[cols3] , factor)
row.names(PC3) <- PC3$SpCode
PC3$SpCode <- NULL # delete column
PC3 <- PC3[complete.cases(PC3),]

############tune RF.PC##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(987)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data =PC3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,3000,5000)) {
  set.seed(220)
   rf_maxtrees <- train(PriPoll~.,
                       data = PC3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(782)
RF.PC <- randomForest(PriPoll ~.,data = PC3, proximity= TRUE, importance= TRUE, mtry=6, ntree=300)
RF.PC
RF.PC$importance
RF.PC$importanceSD
importance(RF.PC, Type=1, scale=TRUE)
varImpPlot(RF.PC)

set.seed(467)
rf.perm <- rf.significance(RF.PC, PC3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)
partialPlot(RF.PC,PC3,Size,"Bees")
partialPlot(RF.PC,PC3,Size,"Flies")
partialPlot(RF.PC,PC3,SugarFlower,"Birds")
partialPlot(RF.PC,PC3,Size,"Butterflies")
partialPlot(RF.PC,PC3,Size,"Moths")

############################## 650m ################
DG3<- as.data.frame(DG[,4:14])
DG3[cols3] <- lapply(DG3[cols3] , factor)
row.names(DG3) <- DG3$SpCode
DG3$SpCode <- NULL # delete column
DG3 <- DG3[complete.cases(DG3),]

############tune RF.DG##############
# Define the control
trControl <- trainControl(method = "cv",
                          number = 5,
                          search = "grid")
set.seed(321)
#tuning mtry
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(PriPoll~.,
                 data =DG3,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 )
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

#tuning ntree
store_maxtrees <- list()
for (ntree in c(300, 500,  1000, 2000,3000,5000)) {
  set.seed(2200)
  rf_maxtrees <- train(PriPoll~.,
                       data = DG3,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#fit final RF
set.seed(782)
RF.DG <- randomForest(PriPoll ~.,data = DG3, proximity= TRUE, importance= TRUE, mtry=10, ntree=300)
RF.DG
RF.DG$importance
RF.DG$importanceSD
importance(RF.DG, Type=1, scale=TRUE)
varImpPlot(RF.DG)
set.seed((467))
rf.perm <- rf.significance(RF.DG, DG3[-1], nperm=199, ntree=301)
rf.perm 
plot(rf.perm)

