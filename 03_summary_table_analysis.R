library(ggplot2)
library(sf)
library(dplyr)
library(geodata)
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(raster)
library(ggmap)
library(smoothr)
library(lubridate)
library(dplyr)
library(vctrs)

#read in the data
summary.final<-read.csv("./data/summary_table.csv")
removal.all<-read.csv("./data/removal.all.csv")

#clean the removal data and summarise it for graphing purposes
removal.all<-removal.all %>% filter(Year>=2013) #selecting just the dates since the scout program began
removal.summ<-removal.all %>% group_by(Method) %>% summarise(n=n())
removal.summ<-removal.summ[c(2:11),] #remove group with no method
removal.summ<-removal.summ %>% mutate(percent=round(100*(n/sum(n)),digits=2))  #create a new column of percentage

#make a pie chart of the removal data and save
ggplot(data=removal.summ,aes(x="", y=n, fill=Method))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(x=1.7, label = paste0(percent, "%")), position = position_stack(vjust=0.5),size=4) +
  scale_fill_viridis_d(option="turbo")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  labs(title="Python Captures in Southwest Florida 2013-2025 by Capture Method")
ggsave("./images/method_piechart.jpeg")  


summary.final$season<-as.factor(summary.final$season)
summary.final$search<-as.integer(summary.final$search)

## summary.final exploratory data
summ<-summary.final %>% group_by(season) %>%  
  summarise(n=n(),
            total.removal=sum(pythons.removed,na.rm=T),
            total.female=sum(females,na.rm=T),
            total.male=sum(males,na.rm=T),
            total.ground=sum(ground,na.rm=T),
            total.peak=sum(peak,na.rm=T),
            total.search = sum(search,na.rm=T),
            total.sample.size=sum(sample.size,na.rm=T),
            total.activity.season.trapdays=sum(trapdays.corrected.breeding.season,na.rm=T))

ggplot(data=summ)+
  geom_point(aes(x=season,y=total.female,group=NA,color="pink"))+
  geom_line(aes(x=season,y=total.female,group=NA,color="pink"))+
  geom_point(aes(x=season,y=total.male,group=NA,color="red"))+
  geom_line(aes(x=season,y=total.male,group=NA,color="red"))+
  geom_point(aes(x=season,y=total.removal,group=NA,color="green"))+
  geom_line(aes(x=season,y=total.removal,group=NA,color="green"))+
  scale_color_manual(values=c("green","pink","red"),
                     labels=c('total pythons','females','males'))+ 
  theme_bw()+
  theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  labs(x="Season",y="Count")












#investigating how trapdays relates to pythons removed
ggplot(summary.final)+
  geom_point(aes(x=trapdays.corrected.breeding.season,y=pythons.removed))

model<-lm(pythons.removed~trapdays.corrected.breeding.season,data=summary.final)
summary(model)

#create a column indicating the total number of seasons each snake has been tracked for
summary.final<-summary.final %>% group_by(Snake_ID) %>% 
  arrange(Snake_ID,season) %>% 
  mutate(seasons.tracked = if_else(row_number() == 1,1,NA))
summary.final <- summary.final %>% group_by(Snake_ID) %>% fill(seasons.tracked) %>% mutate(seasons.tracked = cumsum(seasons.tracked)) 

#investigating HR overlap over time (seasons)

ggplot(summary.final)+
  geom_point(aes(x=seasons.tracked,y=overlap_core.A))

model<-lm(overlap_core.A~seasons.tracked,data=summary.final)
summary(model)

#strongest relationship is between core A overlap and seasons tracked ie; as pythons are tracked for more seasons 
#(and presumably, as more females are removed from their home range), home range overlap decreases, 
#suggesting that snakes are changing their home ranges over time in response to python removal 
#(or to avoid python researchers I suppose)

#investigating hoe minimum distance traveled changes over time (seasons)

ggplot(summary.final)+
  geom_point(aes(x=seasons.tracked,y=min.dist.core.B.mi.corrected))

model<-lm(min.dist.core.B.mi.corrected~seasons.tracked,data=summary.final)
summary(model)

library(lme4)
library(MuMIn)
#creating a model to find the strongest predictors of pythons removed

summary.final$search<-as.integer(summary.final$search)
summary.final$search<-ifelse(is.na(summary.final$search),0,summary.final$search)
summary.final$MCP_sqmi<-ifelse(is.na(summary.final$MCP_sqmi),0,summary.final$MCP_sqmi)
summary.final$trapdays.corrected.breeding.season<-ifelse(is.na(summary.final$trapdays.corrected.breeding.season),0,summary.final$trapdays.corrected.breeding.season)
summary.final$Snake_ID<-as.factor(summary.final$Snake_ID)

#scaling and centering variables first
summary.final$search<-scale(summary.final$search,center=T,scale=T)
summary.final$sample.size<-scale(summary.final$sample.size,center=T,scale=T)
summary.final$MCP_sqmi<-scale(summary.final$MCP_sqmi,center=T,scale=T)
summary.final$trapdays.corrected.breeding.season<-scale(summary.final$trapdays.corrected.breeding.season,center=T,scale=T)
summary.final$min.dist.mi.corrected<-scale(summary.final$min.dist.mi.corrected,center=T,scale=T)
summary.final$seasons.tracked<-scale(summary.final$seasons.tracked,center=T,scale=T)

#creating different model types
mod.glm.ident<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=gaussian(link="identity"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.glm.inv<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=gaussian(link="inverse"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.glm.log<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=gaussian(link="log"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.bi.logit<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=binomial(link="logit"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.bi.probit<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=binomial(link="probit"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.bi.cauchit<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=binomial(link="cauchit"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.bi.log<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=binomial(link="log"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.bi.cloglog<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=binomial(link="cloglog"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.gamma.inv<-glmer(pythons.removed~
                 search+sample.size+
                 MCP_sqmi+
                 trapdays.corrected.breeding.season+
                 min.dist.mi.corrected+seasons.tracked+
                 (1|Snake_ID),
               data=summary.final,
               family=Gamma(link="inverse"),
               control=glmerControl(optimizer="nlminbwrap"))
mod.gamma.ident<-glmer(pythons.removed~
                    search+sample.size+
                    MCP_sqmi+
                    trapdays.corrected.breeding.season+
                    min.dist.mi.corrected+seasons.tracked+
                    (1|Snake_ID),
                  data=summary.final,
                  family=Gamma(link="identity"),
                  control=glmerControl(optimizer="nlminbwrap"))

mod.gamma.log<-glmer(pythons.removed~
                    search+sample.size+
                    MCP_sqmi+
                    trapdays.corrected.breeding.season+
                    min.dist.mi.corrected+seasons.tracked+
                    (1|Snake_ID),
                  data=summary.final,
                  family=Gamma(link="log"),
                  control=glmerControl(optimizer="nlminbwrap"))



mod.pois.log<-glmer(pythons.removed~
                    search+sample.size+
                    MCP_sqmi+
                    trapdays.corrected.breeding.season+
                    min.dist.mi.corrected+seasons.tracked+
                    (1|Snake_ID),
                  data=summary.final,
                  family=poisson(link="log"),
                  control=glmerControl(optimizer="nlminbwrap"))
mod.pois.ident<-glmer(pythons.removed~
                   search+sample.size+
                   MCP_sqmi+
                   trapdays.corrected.breeding.season+
                   min.dist.mi.corrected+seasons.tracked+
                   (1|Snake_ID),
                 data=summary.final,
                 family=poisson(link="identity"),
                 control=glmerControl(optimizer="nlminbwrap"))
mod.pois.sqrt<-glmer(pythons.removed~
                   search+sample.size+
                   MCP_sqmi+
                   trapdays.corrected.breeding.season+
                   min.dist.mi.corrected+seasons.tracked+
                   (1|Snake_ID),
                 data=summary.final,
                 family=poisson(link="sqrt"),
                 control=glmerControl(optimizer="nlminbwrap"))
mod.invG.mu<-glmer(pythons.removed~
                   search+sample.size+
                   MCP_sqmi+
                   trapdays.corrected.breeding.season+
                   min.dist.mi.corrected+seasons.tracked+
                   (1|Snake_ID),
                 data=summary.final,
                 family=inverse.gaussian(link="1/mu^2"),
                 control=glmerControl(optimizer="nlminbwrap"))
mod.invG.inv<-glmer(pythons.removed~
                   search+sample.size+
                   MCP_sqmi+
                   trapdays.corrected.breeding.season+
                   min.dist.mi.corrected+seasons.tracked+
                   (1|Snake_ID),
                 data=summary.final,
                 family=inverse.gaussian(link="inverse"),
                 control=glmerControl(optimizer="nlminbwrap"))
mod.invG.identity<-glmer(pythons.removed~
                   search+sample.size+
                   MCP_sqmi+
                   trapdays.corrected.breeding.season+
                   min.dist.mi.corrected+seasons.tracked+
                   (1|Snake_ID),
                 data=summary.final,
                 family=inverse.gaussian(link="identity"),
                 control=glmerControl(optimizer="nlminbwrap"))
mod.invG.log<-glmer(pythons.removed~
                   search+sample.size+
                   MCP_sqmi+
                   trapdays.corrected.breeding.season+
                   min.dist.mi.corrected+seasons.tracked+
                   (1|Snake_ID),
                 data=summary.final,
                 family=inverse.gaussian(link="log"),
                 control=glmerControl(optimizer="nlminbwrap"))



AIC(mod.pois.log,mod.pois.sqrt) #mod.pois.sqrt best fits the data



model<-glmer(pythons.removed~
               search+
               sample.size+
               MCP_sqmi+
               trapdays.corrected.breeding.season+
               min.dist.mi.corrected+seasons.tracked+
               (1|Snake_ID),
             data=summary.final,
             family=poisson(link="sqrt"))

#check collinearity of predictor variables

pairs(summary.final[,c(12,13,16,22,23,26)],lower.panel=NULL)
check.all<-performance::check_collinearity(model)
check.all<-check.all %>% arrange(desc(VIF)) #VIF>5 indicates problematic collinearity

options(na.action = "na.fail")   #  prevent fitting models to different datasets
combinations<-dredge(model)
options(na.action = "na.omit")   # reset options

#removing uninformative model subsets
combinations2<-combinations[-c(6)]

#extracting all models with a delta value less than 2
best.models<-subset(combinations2,delta<2)

#creating an average model of the two top competitive models
avg.mod<-model.avg(best.models)

#create a summary of the average model
summary.mod<-summary(avg.mod)

#convert summary to df
full<-as.data.frame(summary.mod[[9]])

full<-tibble::rownames_to_column(full,"Variable") %>% arrange(`Pr(>|z|)`)

#exclude intercept
full.subset<-full %>% dplyr::slice(2:6)

#add confidence intervals
full.subset$upper<-(full.subset$Estimate+(full.subset$`Adjusted SE` * 1.96))
full.subset$lower<-(full.subset$Estimate-(full.subset$`Adjusted SE` * 1.96))

#plot the data
full.subset %>% 
  dplyr::arrange(desc(`Pr(>|z|)`)) %>% 
  mutate(variable=factor(Variable,levels=Variable)) %>% 
  ggplot()+
  geom_vline(xintercept = 0,linetype="dashed",col="black",linewidth=1)+
  geom_pointrange(aes(x=Estimate,y=Variable,xmin=lower,xmax=upper),colour="darkorange",size=0.025)+
  geom_point(aes(x=Estimate,y=Variable),size=1.5,colour="black")+
  labs(x="Regression Coefficient")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=7),
        axis.title = element_text(size=9))+
  xlim(-0.55,1)

ggsave("./images/predictors_of_python_removal.jpeg")  

#exploring correlates of home range overlap
# #create a column to identify the first season tracked for each snake
# summary.final<-summary.final %>% group_by(Snake_ID) %>% mutate(n=NA,
#                                                                n = replace(n, 1, 1))
# #fill in NA values with sequentially increasing numbers
# summary.final<-summary.final %>%
#   group_by(Snake_ID, na_group = cumsum(!is.na(n))) %>%
#   mutate(n_til_non_na = ifelse(is.na(n), rev(row_number()), 0L)) %>%
#   group_by(Snake_ID) %>%
#   mutate(
#     fill_down = vec_fill_missing(n, direction = "down"),
#     fill_up = vec_fill_missing(n, direction = "up"),
#     seasons.tracked = case_when(
#       is.na(fill_up) ~ fill_down + cumsum(is.na(fill_up)),
#       is.na(n) ~ fill_up - n_til_non_na,
#       TRUE ~ n
#     )
#   ) %>%
#   ungroup()
# 
# #remove unnecessary columns
# summary.final<-summary.final %>% dplyr::select(!(c(26,27,28,29,30)))

summary(lm(pythons.removed~
                sample.size,
              data=summary.final))

summary(glmer(pythons.removed~
                sample.size+
                  (1|Snake_ID),
             data=summary.final,
             family=poisson(link="sqrt")))

ggplot(summary.final,aes(x=sample.size,y=pythons.removed))+
  geom_point()+
  geom_smooth(method=lm)

#testing for interaction terms among our top predictors of python removals
AIC(lm(data=summary.final,
           pythons.removed~
             trapdays.corrected.breeding.season+
             seasons.tracked+
             search+
             search:seasons.tracked))
            # search:trapdays.corrected.breeding.season+
            # trapdays.corrected.breeding.season:seasons.tracked))


#Results: there is a significant interaction between seasons tracked and search. 
#So, the relationship between python removal and search effort depends on the seasons tracked.
