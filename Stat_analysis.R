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

all.cats.akde.95 <- all.cats.akde.95 %>% dplyr::select(-sum.DRR,-mean.FXY,-Saison,-Saison2,-Outdoor_access,-Session,-X)
all.cats.akde.95 <- all.cats.akde.95 %>% na.omit()

st(all.cats.akde.95, vars = c('Sex','akde','Age','sum.RR','Outdoor_access2','buffer100','buffer500','mean.TM','daily.RR'), group.long = TRUE)


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
  dplyr::rename('proportion of non-artificial 100m'=buffer100)%>% #translation in english
  dplyr::rename('proportion of non-artificial 250m'=buffer250)%>% #translation in english
  dplyr::rename('proportion of non-artificial 500m'=buffer500)%>% #translation in english
 dplyr::rename('daily rainfall'=log.daily) #translation in english

all.cats.akde.95$akde <- as.integer(all.cats.akde.95$akde)



stat.model.full.95 <- lmer(`log(aKDE95%)` ~ age + sex +`proportion of non-artificial 100m`+`proportion of non-artificial 500m`+ `outdoor access` +  `daily rainfall` + `mean temperature` + (1|Type_GPS) + (1|id_owner) + (1|ID_chat),
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
  dplyr::rename('proportion of non-artificial 100m'=buffer100)%>% #translation in english
  dplyr::rename('proportion of non-artificial 250m'=buffer250)%>% #translation in english
  dplyr::rename('proportion of non-artificial 500m'=buffer500)%>% #translation in english
  dplyr::rename('daily rainfall'=log.daily) #translation in english

all.cats.akde.50$akde <- as.integer(all.cats.akde.50$akde)



stat.model.full.50 <- lmer(`log(aKDE50%)` ~ age + sex +`proportion of non-artificial 100m`+`proportion of non-artificial 500m`+ `outdoor access` +  `daily rainfall` + `mean temperature` + (1|Type_GPS) + (1|id_owner) + (1|ID_chat),
                           data=all.cats.akde.50)


summary(stat.model.full.50)

r.squaredGLMM(stat.model.full.50)


plot50 <- plot_model(stat.model.full.50,vline.color = "black", show.values = TRUE, value.offset = .3,sort.est = TRUE)
plot50


simulationOutput <- simulateResiduals(fittedModel = stat.model.full.50, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput)
################aKDE50%_analysis################




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

confint(stat.model.full.50,level=0.95)
confint(stat.model.full.95,level=0.95)
confint(stat.model.full.50,level=0.90)
confint(stat.model.full.95,level=0.90)

simsex1<- powerSim(stat.model.full.95, fixed("sex"),  nsim = 1000, alpha = .05)
simsex2 <- powerSim(stat.model.full.50, fixed("sex"),  nsim = 1000, alpha = .1)
simage1 <- powerSim(stat.model.full.95, fixed("age"),  nsim = 1000, alpha = .05)
simsage2 <- powerSim(stat.model.full.50, fixed("age"),  nsim = 1000, alpha = .05)


simsex1
simsex2
simage1
simsage2


stargazer(stat.model.full, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

stargazer(stat.model.full.50, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


a <- plot_model(stat.model.full.95,vline.color = "black", show.values = TRUE, value.offset = .3,sort.est = TRUE)
summary(stat.model.full)
b<- plot_model(stat.model.full.50,vline.color = "black", show.values = TRUE, value.offset = .3,sort.est = TRUE)
summary(stat.model.full.50)

plot.all.cats.akde.95 <- ggplot(all.cats.akde.95.for.plot, aes(x=akde.final)) + 
  geom_histogram( colour="black", fill="black",bins =50)+
  geom_vline(aes(xintercept=mean(akde.final)), 
             color = "yellow", size=2)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(x="aKDE 95% (square kilometers)",y="Number of domestic cats")+
  theme(
    axis.text.x=element_text(size=10)
  )

plot.all.cats.akde.50 <- ggplot(all.cats.akde.50.for.plot, aes(x=akde.final)) + 
  geom_histogram(colour="black", fill="black",bins =50)+
  geom_vline(aes(xintercept=mean(akde.final)), 
             color = "yellow", size=2)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(x="aKDE 50% (square kilometers)",y="Number of domestic cats")+
  theme(
    axis.text.x=element_text(size=10)
  )


ggarrange(plot.all.cats.akde.95,plot.all.cats.akde.50,labels = c("A", "B"),ncol = 1, nrow = 2)
ggarrange(plot95,plot50,labels = c("A", "B"),ncol = 2, nrow = 1)


all.cats.akde.95 <- read.csv('all.cats.akde.95.csv')
ggplot(data = all.cats.akde.95,aes(y=mean.TM,x=Saison2))+geom_boxplot()

