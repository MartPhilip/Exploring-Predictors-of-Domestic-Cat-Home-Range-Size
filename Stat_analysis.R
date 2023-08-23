################Title################
'Exploring Predictors of Domestic Cat 
Home Range Size Variability and 
Implications for Wildlife Management'

'PHILIPPE-LESAFFRE Martin'

'05/2023'
################Title################

################Section################
'Statistical analysis'
################Section################
################Package################
require(vtable)
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
require(splines)
require(ggeffects)
scale2 <- function(x){(x - mean(x)) / (2*sd(x))}
################Package################


################Loading_data################
all.cats.akde.95 <- read.csv('all.cats.akde.95.csv')
all.cats.akde.50 <- read.csv('all.cats.akde.50.csv')
################Loading_data################




################aKDE95%_analysis################
all.cats.akde.95 <- all.cats.akde.95 %>% 
  mutate(log.akde=log(akde))%>%
  mutate(log.sum.RR=log(sum.RR))%>%
  mutate(log.Monitoring_time=log(Monitoring_time))%>%
  mutate(log.Age=log(Age))%>%
  mutate(log.daily=log(daily.RR))
all.cats.akde.95 <- all.cats.akde.95 %>%
  mutate(akde.plot=akde/1000000)



all.cats.akde.95 <- all.cats.akde.95 %>% dplyr::select(-sum.DRR,-mean.FXY,-Saison,-Outdoor_access,-Session,-X)
all.cats.akde.95 <- all.cats.akde.95 %>% na.omit()
st(all.cats.akde.95, vars = c('Sex','akde','Age','sum.RR','Outdoor_access2','buffer100','buffer500','mean.TM','daily.RR',"Saison2","Type_GPS"), group.long = TRUE)



all.cats.akde.95 <- mutate_at(all.cats.akde.95, c("log.sum.RR","log.Age","mean.TM","log.Monitoring_time","buffer100","buffer250","buffer500","log.daily"), scale2)
hist(all.cats.akde.95$log.akde)
hist(all.cats.akde.95$log.Age)
hist(all.cats.akde.95$log.sum.RR)
hist(all.cats.akde.95$mean.TM)
hist(all.cats.akde.95$log.Monitoring_time)
hist(all.cats.akde.95$buffer100)
hist(all.cats.akde.95$buffer250)
hist(all.cats.akde.95$buffer500)
hist(all.cats.akde.95$log.daily)



all.cats.akde.95.bis <- all.cats.akde.95
all.cats.akde.95 <- all.cats.akde.95 %>% 
  dplyr::rename('log(aKDE95%)'=log.akde) %>%
  dplyr::rename(age=log.Age) %>%
  dplyr::rename(sex=Sex) %>%
  dplyr::rename('outdoor access' =Outdoor_access2) %>%
  dplyr::rename(rainfall=log.sum.RR) %>%
  dplyr::rename('mean temperature'=mean.TM) %>%
  dplyr::rename(monitoring=log.Monitoring_time) %>%
  dplyr::rename('proportion of vegetation 100m'=buffer100)%>% #translation in english
  dplyr::rename('proportion of vegetation 250m'=buffer250)%>% #translation in english
  dplyr::rename('proportion of vegetation 500m'=buffer500)%>% #translation in english
 dplyr::rename('daily rainfall'=log.daily) #translation in english

all.cats.akde.95$akde <- as.integer(all.cats.akde.95$akde)
all.cats.akde.95$id_owner <- as.factor(all.cats.akde.95$id_owner)


stat.model.full.95 <- lmer(`log(aKDE95%)` ~ age + sex +`proportion of vegetation 100m`+`proportion of vegetation 500m`+ `outdoor access` +  `daily rainfall` + `mean temperature` + Type_GPS  + (1|ID_chat),
                        data=all.cats.akde.95)



summary(stat.model.full.95)

r.squaredGLMM(stat.model.full.95)


plot95 <- plot_model(stat.model.full.95,vline.color = "black", show.values = TRUE, value.offset = .3,sort.est = TRUE)
plot95

simulationOutput <- simulateResiduals(fittedModel = stat.model.full.95, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput)
################aKDE95%_analysis################



################aKDE50%_analysis################
all.cats.akde.50 <- all.cats.akde.50 %>% 
  mutate(log.akde=log(akde))%>%
  mutate(log.sum.RR=log(sum.RR))%>%
  mutate(log.Monitoring_time=log(Monitoring_time))%>%
  mutate(log.Age=log(Age))%>%
  mutate(log.daily=log(daily.RR))
all.cats.akde.50 <- all.cats.akde.50 %>%
  mutate(akde.plot=akde/1000000)

all.cats.akde.50 <- all.cats.akde.50 %>% dplyr::select(-sum.DRR,-mean.FXY,-Saison,-Saison2,-Outdoor_access,-Session,-X)
all.cats.akde.50 <- all.cats.akde.50 %>% na.omit()

st(all.cats.akde.50, vars = c('Sex','akde','Age','sum.RR','Outdoor_access2','buffer100','buffer500','mean.TM','daily.RR'), group.long = TRUE)


all.cats.akde.50 <- mutate_at(all.cats.akde.50, c("log.sum.RR","log.Age","mean.TM","log.Monitoring_time","buffer100","buffer250","buffer500","log.daily"), scale2)
hist(all.cats.akde.50$log.akde)
hist(all.cats.akde.50$log.Age)
hist(all.cats.akde.50$log.sum.RR)
hist(all.cats.akde.50$mean.TM)
hist(all.cats.akde.50$log.Monitoring_time)
hist(all.cats.akde.50$buffer100)
hist(all.cats.akde.50$buffer250)
hist(all.cats.akde.50$buffer500)
hist(all.cats.akde.50$log.daily)



all.cats.akde.50.bis <- all.cats.akde.50
all.cats.akde.50 <- all.cats.akde.50 %>% 
  dplyr::rename('log(aKDE50%)'=log.akde) %>%
  dplyr::rename(age=log.Age) %>%
  dplyr::rename(sex=Sex) %>%
  dplyr::rename('outdoor access' =Outdoor_access2) %>%
  dplyr::rename(rainfall=log.sum.RR) %>%
  dplyr::rename('mean temperature'=mean.TM) %>%
  dplyr::rename(monitoring=log.Monitoring_time) %>%
  dplyr::rename('proportion of vegetation 100m'=buffer100)%>% #translation in english
  dplyr::rename('proportion of vegetation 250m'=buffer250)%>% #translation in english
  dplyr::rename('proportion of vegetation 500m'=buffer500)%>% #translation in english
  dplyr::rename('daily rainfall'=log.daily) #translation in english

all.cats.akde.50$akde <- as.integer(all.cats.akde.50$akde)
all.cats.akde.50$id_owner <- as.factor(all.cats.akde.50$id_owner)



stat.model.full.50 <- lmer(`log(aKDE50%)` ~ age + sex +`proportion of vegetation 100m`+`proportion of vegetation 500m`+ `outdoor access` +  `daily rainfall` + `mean temperature` + Type_GPS + (1|ID_chat),
                           data=all.cats.akde.50)


summary(stat.model.full.50)

r.squaredGLMM(stat.model.full.50)


plot50 <- plot_model(stat.model.full.50,vline.color = "black", show.values = TRUE, value.offset = .3,sort.est = TRUE)
plot50


simulationOutput <- simulateResiduals(fittedModel = stat.model.full.50, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput)

################data structure##############

ID.reparition <- all.cats.akde.95 %>% select(ID_chat) %>% mutate(occu=1) 
ID.reparition <- ddply(ID.reparition,.(ID_chat),summarize,'Monitoring number'=sum(occu))
ID.reparition$'Multiple observations' <- ifelse(ID.reparition$`Monitoring number`>1,"YES","NO")
ID.reparition$`Monitoring number` <- as.character(ID.reparition$`Monitoring number`)
ID.reparition <- ID.reparition %>%  pivot_longer(cols=c('Monitoring number','Multiple observations'))
ID.reparition <- ID.reparition %>% mutate(occur=1)
ID.reparition <- ddply(ID.reparition,.(name,value),summarize,'number'=sum(occur))

ID.reparition.plot <- ggplot(data = ID.reparition,aes(x = value,y=number,fill=value),color='black') + 
  geom_col(color='black')+
  geom_point(shape=23,fill='black',size=3)+
  facet_wrap(~name,scales='free')+
  theme_bw() +
  scale_fill_viridis_d()+
  theme(
    axis.text=element_text(size=10),
    panel.background = element_rect(fill = "lightcyan2",
                                    colour = "lightcyan2",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"),
  legend.position='none',strip.text = element_text(size=15),
  axis.title.y = element_text(size=13))+  
  labs(x='',y='Individual number')
ID.reparition.plot


session.reparition <- all.cats.akde.95 %>% select(Annee,Saison2) %>% mutate(occu=1) 
session.reparition <- ddply(session.reparition,.(Annee,Saison2),summarize,'Monitoring number'=sum(occu))
session.reparition$`Monitoring number` <- as.character(session.reparition$`Monitoring number`)
session.reparition <- session.reparition %>%  pivot_longer(cols=c('Monitoring number'))
session.reparition$value <- as.numeric(session.reparition$value)
session.reparition$Annee <- as.character(session.reparition$Annee)


session.reparition.plot <- ggplot(data = session.reparition,aes(x = Saison2,y=value,fill=Saison2),color='black') + 
  facet_grid(.~Annee)+
  geom_col(color='black',position=position_dodge2(width = 0.5))+
  geom_point(shape=23,fill='black',size=3,position=position_dodge2(width = 0.5))+
  theme_bw() +
  scale_fill_viridis_d()+
  theme(
    axis.text=element_text(size=10),
    legend.position='none',strip.text = element_text(size=15),
    axis.title.y = element_text(size=13))+  
  labs(x='',y='Individual number')
session.reparition.plot

################results_summary################
all.cats.akde.95.for.plot <- ddply(all.cats.akde.95,.(ID_chat),summarize,akde.final=mean(akde.plot))
all.cats.akde.50.for.plot <- ddply(all.cats.akde.50,.(ID_chat),summarize,akde.final=mean(akde.plot))

min(all.cats.akde.95.for.plot$akde.final)
max(all.cats.akde.95.for.plot$akde.final)
mean(all.cats.akde.95.for.plot$akde.final)
sd(all.cats.akde.95.for.plot$akde.final)

min(all.cats.akde.50.for.plot$akde.final)
max(all.cats.akde.50.for.plot$akde.final)
mean(all.cats.akde.50.for.plot$akde.final)
sd(all.cats.akde.50.for.plot$akde.final)

median(all.cats.akde.95.for.plot$akde.final)
median(all.cats.akde.50.for.plot$akde.final)

confint(stat.model.full.50,level=0.90)
confint(stat.model.full.95,level=0.90)
confint50 <- as.data.frame(confint(stat.model.full.50,level=0.95))
confint95 <- as.data.frame(confint(stat.model.full.95,level=0.95))
fixef50 <- as.data.frame(fixef(stat.model.full.50))
fixef95 <- as.data.frame(fixef(stat.model.full.95))
confint50$predictor <- rownames(confint50)
confint95$predictor <- rownames(confint95)
fixef50$predictor <- rownames(fixef50)
fixef95$predictor <- rownames(fixef95)
model95 <- merge(confint95,fixef95,by='predictor')
model50 <- merge(confint50,fixef50,by='predictor')
model95$predictor <- c("Intercept","Mean rainfall","Mean temperature","Unrestricted access to outdoor","Proportion of vegetation 100m", "Proportion of vegetation 500m","Age","Male cat","Grey GPS")
model50$predictor <- c("Intercept","Mean rainfall","Mean temperature","Unrestricted access to outdoor","Proportion of vegetation 100m", "Proportion of vegetation 500m","Age","Male cat","Grey GPS")
model95 <-model95 %>% dplyr::rename(estimate=`fixef(stat.model.full.95)`) %>% mutate(aKDE='95%')
model50 <-model50 %>% dplyr::rename(estimate=`fixef(stat.model.full.50)`)%>% mutate(aKDE='50%')
model <- rbind(model95,model50)

length(unique(all.cats.akde.50$ID_chat))
length(unique(all.cats.akde.95$ID_chat))

simsex1<- powerSim(stat.model.full.95, fixed("sex"),  nsim = 1000, alpha = .05)
simsex2 <- powerSim(stat.model.full.50, fixed("sex"),  nsim = 1000, alpha = .1)
simage1 <- powerSim(stat.model.full.95, fixed("age"),  nsim = 1000, alpha = .05)
simsage2 <- powerSim(stat.model.full.50, fixed("age"),  nsim = 1000, alpha = .05)
simgps1 <- powerSim(stat.model.full.95, fixed("Type_GPS"),  nsim = 1000, alpha = .05)
simsgps2 <- powerSim(stat.model.full.50, fixed("Type_GPS"),  nsim = 1000, alpha = .05)


simsex1
simsex2
simage1
simsage2
simgps1
simsgps2


plot.all.cats.akde.95 <- ggplot(all.cats.akde.95.for.plot, aes(x=akde.final)) + 
  geom_histogram( colour="black", fill="gold",bins =55)+
  geom_vline(aes(xintercept=mean(akde.final)), 
             color = "chartreuse4", size=1.5,linetype='longdash')+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(x="aKDE 95% (square kilometers)",y="Number of domestic cats")+
  theme(
    axis.text=element_text(size=10),
    panel.background = element_rect(fill = "lightcyan2",
                                    colour = "lightcyan2",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"),axis.title= element_text(size=13)
  )
plot.all.cats.akde.95

plot.all.cats.akde.50 <- ggplot(all.cats.akde.50.for.plot, aes(x=akde.final)) + 
  geom_histogram( colour="black", fill="gold",bins =55)+
  geom_vline(aes(xintercept=mean(akde.final)), 
             color = "chartreuse4", size=1.5,linetype='longdash')+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(x="aKDE 50% (square kilometers)",y="Number of domestic cats")+
  theme(
    axis.text=element_text(size=10),
    panel.background = element_rect(fill = "lightcyan2",
                                    colour = "lightcyan2",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"),
    axis.title= element_text(size=13)
  )
plot.all.cats.akde.50

ggarrange(plot.all.cats.akde.95,plot.all.cats.akde.50,labels = c("A", "B"),ncol = 1, nrow = 2)


model <- model %>% filter(predictor!='Intercept')
plot.est <- ggplot(data=model,aes(x=reorder(predictor,estimate),y=estimate,fill=aKDE,label=round(estimate,2)),color='black')+
 geom_hline(aes(yintercept=0), 
             color = "chartreuse4", size=1,linetype='longdash')+
  geom_pointrange(aes(ymin=`2.5 %`,ymax=`97.5 %`),shape=23,position = position_dodge(width = 0.7))+
  geom_text(position = position_dodge(width = 0.7),hjust=-1.5,size=3,parse=TRUE)+
  coord_flip()+
  ylim(-1,1)+
  scale_fill_viridis_d()+
  labs(y="Estimate",x='')+
  theme(
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=7),
    panel.background = element_rect(fill = "lightcyan2",
                                    colour = "lightcyan2",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey40"),
    axis.title= element_text(size=13)
  )

plot.est


all.cats.akde.95 <- read.csv('all.cats.akde.95.csv')
all.cats.akde.95$Saison2 <- factor(all.cats.akde.95$Saison2 , levels=c('fall', 'winter', 'spring', 'summer'))
all.cats.akde.95 <- all.cats.akde.95 %>% dplyr::rename(season=Saison2)
ggplot(data = all.cats.akde.95,aes(y=mean.TM,x=season,color=season,fill=season))+scale_color_viridis_d()+scale_fill_viridis_d()+geom_violin(alpha=0.3,color='black')+geom_boxplot(alpha=0.5,color='black')+geom_point(alpha=0.7,size=3)  + labs(x='',y='mean temperature')

