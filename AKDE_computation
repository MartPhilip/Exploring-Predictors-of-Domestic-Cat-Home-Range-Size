################Title################
'Exploring Predictors of Domestic Cat 
Home Range Size Variability and 
Implications for Wildlife Management'

'PHILIPPE-LESAFFRE Martin'

'05/2023'
################Title################

################Section################
'AKDE Computation'
################Section################

################Package################
require(ggplot2)
require(readr)  
require(dplyr)  
require(tidyr)  
require(purrr)  
require(ctmm)
require(stringr)
require(lme4)
require(rgdal)
require(sp)
require(MuMIn)
require(stargazer)
require(sjPlot)
require(sjlabelled)
require(sjmisc)
require(ggplot2)
require(scales)
require(corrplot)
require(DHARMa)
require(ggpubr)
require(simr)
require(plyr)
scale2 <- function(x){(x - mean(x)) / (2*sd(x))}
################Package################



################AKDE_computing################
files <- dir(pattern = "*ssPtsAb.csv") #contain all the names of the csv corresponding to GPS data
data <- files %>%
  map(read_csv) #list containing all the csv in R format

AKDE.list <- list()
for(i in 1:length(data)){
  data.index <- data[[i]]
  data.index$DateTime_UTC <- gsub("[()]","",data.index$DateTime_UTC)
  w.datim <- paste(data.index$Date_UTC,"",data.index$Time_UTC,sep=" ")
  w.strptim <- as.POSIXct(strptime(w.datim,"%Y-%m-%d %H:%M:%S"))
  extraction <- data.frame("timestamp"=w.strptim,
                           "location.long"=data.index$Longitude,
                           "location.lat"=data.index$Latitude)
  extraction <- extraction %>% 
    distinct(timestamp, .keep_all= TRUE)%>%
    filter(location.lat!=0|location.long!=0)
  
  telemetry <- as.telemetry(extraction,
                            timeformat = '%YY-%M-%D %H:%m:%s',
                            projection=CRS("+init=epsg:2154"))
  w.vario <- variogram(telemetry)
  GUESS <- ctmm.guess(telemetry,interactive = F)
  fitted.mods <- ctmm.select(telemetry,
                             CTMM = GUESS,
                             verbose=T,
                             level=1)
  w.sumfit <- summary(fitted.mods)
  w.mod1 <- fitted.mods[[1]]
  
  w.akde <- akde(telemetry,CTMM=w.mod1)
  akde.95 <- summary(w.akde,level.UD=0.95)
  akde.50 <-summary(w.akde,level.UD=0.50)
  info <- c(data[[i]]$idChat[i],data[[i]]$saison[i],data[[i]]$annee[i])
  summary.akde <- list(w.sumfit,w.mod1,w.akde,akde.95,akde.50,info,w.vario)#ajouter akde.50 et w.vario pour analyse finale
  AKDE.list[[i]] <- summary.akde
}

################AKDE_computing################

################AKDE_data_available################
AKDE.list <- readRDS('AKDE_list.RDS')
################AKDE_data_available################

################database_construction###############
n=length(AKDE.list)
summary.AKDE.list.95 <- data.frame()
for(i in 1:n){
  AKDE.95 <- AKDE.list[[i]][[4]][["CI"]]
  method <- rownames(AKDE.95)
  AKDE.95 <- cbind(AKDE.95,method)
  summary.AKDE.list.95 <- rbind(summary.AKDE.list.95,
                                AKDE.95)
}

summary.AKDE.list.50 <- data.frame()
for(i in 1:n){
  AKDE.50 <- AKDE.list[[i]][[5]][["CI"]]
  method <- rownames(AKDE.50)
  AKDE.50 <- cbind(AKDE.50,method)
  summary.AKDE.list.50 <- rbind(summary.AKDE.list.50,
                                AKDE.50)
}

summary.cat.info <- data.frame()
for(i in 1:163){
  cat.info <- AKDE.list[[i]][[7]]
  summary.cat.info <- rbind(summary.cat.info,
                            cat.info)
}
for(i in 164:168){
  cat.info <- AKDE.list[[i]][[6]]
  summary.cat.info <- rbind(summary.cat.info,
                            cat.info)
}

colnames(summary.cat.info) <- c('ID_chat','Saison','Annee')

summary.AKDE.list.95 <- cbind(summary.AKDE.list.95,summary.cat.info)
summary.AKDE.list.50 <- cbind(summary.AKDE.list.50,summary.cat.info)
rownames(summary.AKDE.list.95) <- c(1:n)
rownames(summary.AKDE.list.50) <- c(1:n)


summary.AKDE.list.95$low <- as.numeric(summary.AKDE.list.95$low)
summary.AKDE.list.95$est <- as.numeric(summary.AKDE.list.95$est)
summary.AKDE.list.95$high <- as.numeric(summary.AKDE.list.95$high)


summary.AKDE.list.50$low <- as.numeric(summary.AKDE.list.50$low)
summary.AKDE.list.50$est <- as.numeric(summary.AKDE.list.50$est)
summary.AKDE.list.50$high <- as.numeric(summary.AKDE.list.50$high)


summary.AKDE.list.95 <- summary.AKDE.list.95 %>%
  mutate(akde = case_when(method=='area (hectares)'~ est*10000,
                          method=='area (square kilometers)'~est*1000000,
                          method=='area (square meters)'~est))

summary.AKDE.list.50 <- summary.AKDE.list.50 %>%
  mutate(akde = case_when(method=='area (hectares)'~ est*10000,
                          method=='area (square kilometers)'~est*1000000,
                          method=='area (square meters)'~est))
################database_construction###############


################extracting_cat_characteristics_info###############
table.cat.info <- readxl::read_xlsx("table_info_cat_final.xlsx")#session 2020-2021
table.cat.info.2 <- readxl::read_xlsx("table_info_cat_final_2.xlsx")#session 2017
################extracting_cat_characteristics_info###############





################database_cleaning###############
summary.AKDE.list.95$ID_chat[is.na(summary.AKDE.list.95$ID_chat)] <- '60'#failed to extract the ID so did it manually 
summary.AKDE.list.95$ID_chat[summary.AKDE.list.95$ID_chat=='0_'] <- '100'#indiv to remove because error when ID assignation 
summary.AKDE.list.50$ID_chat[is.na(summary.AKDE.list.50$ID_chat)] <- '60'
summary.AKDE.list.50$ID_chat[summary.AKDE.list.50$ID_chat=='0_'] <- '100'

summary.AKDE.list.50$Saison[is.na(summary.AKDE.list.50$Saison)] <- 'automne'
summary.AKDE.list.50$Annee[is.na(summary.AKDE.list.50$Annee)] <- '2021'
summary.AKDE.list.95$Saison[is.na(summary.AKDE.list.95$Saison)] <- 'automne'
summary.AKDE.list.95$Annee[is.na(summary.AKDE.list.95$Annee)] <- '2021'


summary.AKDE.list.95$ID_chat <- as.numeric(summary.AKDE.list.95$ID_chat)
summary.AKDE.list.50$ID_chat <- as.numeric(summary.AKDE.list.50$ID_chat)


summary.AKDE.list.95 <- summary.AKDE.list.95 %>% mutate(position=c(1:n))
summary.AKDE.list.50 <- summary.AKDE.list.50 %>% mutate(position=c(1:n))
summary.AKDE.list.95 <- summary.AKDE.list.95 %>% distinct(ID_chat,Saison,Annee,.keep_all = T)
summary.AKDE.list.50 <- summary.AKDE.list.50 %>% distinct(ID_chat,Saison,Annee,.keep_all = T)

summary.AKDE.list.95 <- summary.AKDE.list.95 %>% filter(position!=1 &
                                                          position!=10 &
                                                          position!=21 &
                                                          position!=24 &
                                                          position!=27 &
                                                          position!=39 &
                                                          position!=47 &
                                                          position!=48 &
                                                          position!=49 &
                                                          position!=64 &
                                                          position!=70 &
                                                          position!=75 &
                                                          position!=80 &
                                                          position!=82 &
                                                          position!=116 &
                                                          position!=128 &
                                                          position!=145 &
                                                          position!=148 &
                                                          position!=156 &
                                                          position!=160)#removed the aKDE which not converge 
summary.AKDE.list.50 <- summary.AKDE.list.50 %>%  filter(position!=1 &
                                                           position!=10 &
                                                           position!=21 &
                                                           position!=24 &
                                                           position!=27 &
                                                           position!=39 &
                                                           position!=47 &
                                                           position!=48 &
                                                           position!=49 &
                                                           position!=64 &
                                                           position!=70 &
                                                           position!=75 &
                                                           position!=80 &
                                                           position!=82 &
                                                           position!=116 &
                                                           position!=128 &
                                                           position!=145 &
                                                           position!=148 &
                                                           position!=156 &
                                                           position!=160)#removed the aKDE which not converge  



table.cat.info$Saison <- tolower(table.cat.info$Saison)
all.cats.akde.95 <- merge(summary.AKDE.list.95,table.cat.info,by=c('ID_chat',"Saison","Annee"),all.x=T)#merging akde and cat characteristics  
all.cats.akde.50 <- merge(summary.AKDE.list.50,table.cat.info,by=c('ID_chat',"Saison","Annee"),all.x=T)#merging akde and cat characteristics   
all.cats.akde.95 <- all.cats.akde.95 %>% distinct(ID_chat,Saison,Annee,.keep_all = T)
all.cats.akde.50 <- all.cats.akde.50 %>% distinct(ID_chat,Saison,Annee,.keep_all = T)

all.cats.akde.95 <- all.cats.akde.95 %>% dplyr::select(ID_chat,Saison,Annee,akde,Session,Age,Sex,Type_GPS,Outdoor_access,sum.DRR,sum.RR,mean.TM,mean.FXY,Monitoring_time)
all.cats.akde.50 <- all.cats.akde.50 %>% dplyr::select(ID_chat,Saison,Annee,akde,Session,Age,Sex,Type_GPS,Outdoor_access,sum.DRR,sum.RR,mean.TM,mean.FXY,Monitoring_time)

all.cats.akde.95.2 <- table.cat.info.2 %>% dplyr::select(ID_chat,Saison,Annee,aKDE95,Age,Sex,Type_GPS,Outdoor_access,sum.DRR,sum.RR,mean.TM,mean.FXY,Monitoring_time)
all.cats.akde.95.2 <- all.cats.akde.95.2 %>% mutate(Session=0) %>% dplyr::rename(akde=aKDE95)
all.cats.akde.50.2 <- table.cat.info.2 %>% dplyr::select(ID_chat,Saison,Annee,aKDE50,Age,Sex,Type_GPS,Outdoor_access,sum.DRR,sum.RR,mean.TM,mean.FXY,Monitoring_time)
all.cats.akde.50.2 <- all.cats.akde.50.2 %>% mutate(Session=0) %>% dplyr::rename(akde=aKDE50)

all.cats.akde.95 <- rbind(all.cats.akde.95,all.cats.akde.95.2)
all.cats.akde.50 <- rbind(all.cats.akde.50,all.cats.akde.50.2)

all.cats.akde.95$Saison <- tolower(all.cats.akde.95$Saison)
all.cats.akde.50$Saison <- tolower(all.cats.akde.50$Saison)


all.cats.akde.95 <- all.cats.akde.95 %>% mutate(Outdoor_access2=case_when(Outdoor_access=='demande'~'restricted',
                                                                          Outdoor_access=='libreJour'~'restricted',
                                                                          Outdoor_access=='libre'~'unrestricted',
                                                                          Outdoor_access=='demandeJour'~'restricted',
                                                                          Outdoor_access=='demande_jour_nuit'~'restricted',
                                                                          Outdoor_access=='libre_jour'~'restricted',
))
all.cats.akde.50 <- all.cats.akde.50 %>% mutate(Outdoor_access2=case_when(Outdoor_access=='demande'~'restricted',
                                                                          Outdoor_access=='libreJour'~'restricted',
                                                                          Outdoor_access=='libre'~'unrestricted',
                                                                          Outdoor_access=='demandeJour'~'restricted',
                                                                          Outdoor_access=='demande_jour_nuit'~'restricted',
                                                                          Outdoor_access=='libre_jour'~'restricted',
))

all.cats.akde.95 <- all.cats.akde.95 %>% mutate(Saison2=case_when(Saison=='hiver'~'winter',
                                                                  Saison=='printemps'~'spring',
                                                                  Saison=='ete'~'summer',
                                                                  Saison=='automne'~'fall'))

all.cats.akde.50 <- all.cats.akde.50 %>% mutate(Saison2=case_when(Saison=='hiver'~'winter',
                                                                  Saison=='printemps'~'spring',
                                                                  Saison=='ete'~'summer',
                                                                  Saison=='automne'~'fall'))

#correct all the predictors in term of English and class

################database_cleaning###############


all.cats.akde.95 <- all.cats.akde.95 %>% filter(Monitoring_time<38) %>% filter(Age>0) %>% mutate(daily.RR=sum.RR/Monitoring_time)
all.cats.akde.50 <- all.cats.akde.50 %>% filter(Monitoring_time<38) %>% filter(Age>0) %>% mutate(daily.RR=sum.RR/Monitoring_time)
buffer <- readRDS("BUFFER.RDS") 
buffer <- buffer %>% select(ID_chat,prop100,prop250,prop500) %>% dplyr::rename(buffer100=prop100) %>% dplyr::rename(buffer250=prop250) %>% dplyr::rename(buffer500=prop500)  #this contain the data about the artificialisaion in a 500m buffer around the house
buffer$ID_chat <- as.numeric(buffer$ID_chat)
id_owners <- readxl::read_xlsx('ID_owners.xlsx')
id_owners <- id_owners %>% select(ID_chat,id_proprio)%>% dplyr::rename(id_owner=id_proprio)#this contain the data about the identity of the owner
id_owners$ID_chat <- as.numeric(id_owners$ID_chat)
id_owners <- id_owners %>% distinct(ID_chat,.keep_all = T)
all.cats.akde.95 <- merge(all.cats.akde.95,buffer,by='ID_chat',all.x=T)
all.cats.akde.95 <- merge(all.cats.akde.95,id_owners,by='ID_chat',all.x=T)
all.cats.akde.50 <- merge(all.cats.akde.50,buffer,by='ID_chat',all.x=T)
all.cats.akde.50 <- merge(all.cats.akde.50,id_owners,by='ID_chat',all.x=T)



write.csv(all.cats.akde.95,'all.cats.akde.95.csv')
write.csv(all.cats.akde.50,'all.cats.akde.50.csv')
