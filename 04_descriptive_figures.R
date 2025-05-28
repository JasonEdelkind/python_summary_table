library(ggplot2)
library(lubridate)
library(yr)

removal_PSRP<-read.csv(".\\data\\removal_PSRP.csv")



removal_PSSF<-read.csv(".\\data\\removal_PSSF.csv")
removal_RBNERR<-read.csv(".\\data\\removal_RBNERR.csv")
removal_PSRP$Date<-as.Date(removal_PSRP$Date,format="%m/%d/%Y")

#add a season column for both the   removal dataframes
#define a function to identify the breeding season
season<-function(x){ifelse(
  x[['Date']] >= ymd(paste0(x[['Year']],"-01-01")) & 
    x[['Date']] <= ymd(paste0(x[['Year']],"-10-14")),
  paste(as.numeric(x[['Year']])-1,"-",x[['Year']],sep=""), 
  paste(x[['Year']],"-",as.numeric(x[['Year']])+1,sep=""))
}

#apply the function
removal_PSSF$season<-apply(removal_PSSF, 1, season)
removal_RBNERR$season<-apply(removal_RBNERR, 1, season)


#PSSF Removal Graphics

# add sample size column and create axis label column
sample_size = removal_PSSF %>% group_by(Sex) %>% summarize(num=n())

removal_PSSF<-removal_PSSF %>%
  left_join(sample_size) %>%
  mutate(myaxis = as.factor(paste0(Sex, "\n", "n=", num))) %>% 
  mutate( type=ifelse(Sex=="Male","Highlighted","Normal"))

#releveling factors so road cruising is first
removal_PSSF$myaxis <- factor(removal_PSSF$myaxis, levels = c('Female\nn=158', 'Male\nn=110'))

#releveling factors so male is first
#removal_PSSF$Sex<-as.factor(removal_PSSF$Sex)
#removal_PSSF$Sex<-factor(removal_PSSF$Sex, levels = c("Male", "Female"))

#boxplot of method by svl grouped by sex
bp<-removal_PSSF %>% 
  filter(Sex!=is.na(Sex))%>%   #remove NA values from sex 
  ggplot()+
  aes(x=myaxis,y=SVL_cm, fill=Sex, na.rm=TRUE)+
  geom_boxplot()+
  scale_fill_viridis_d(option="turbo")+                                #specify viridis turbo palette
  #scale_alpha_manual(values=c(1,0.1))+
  labs(y="SVL (cm)",x="Sex",title="PSSF Python Removals by Sex 2018-2025")+
  scale_fill_manual(name='Sex',                                          
                    breaks=c('Female','Male'),
                    values=c('Female'="pink",'Male'="skyblue"))+
  scale_alpha_manual(values=c(1,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=10),   #change x axis text size
        legend.position="none",
        axis.title.y = element_text(size=15, vjust=2),
        axis.title.x = element_text(size=15, vjust=-1),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )

bp

# removals over time
removal_PSSF$Date<-as.Date(removal_PSSF$Date,format="%Y-%m-%d")
removal_PSSF$month<-base::format(removal_PSSF$Date, format="%B")

#releveling month factors
removal_PSSF$month<-as.factor(removal_PSSF$month)
removal_PSSF$month<-factor(removal_PSSF$month, levels = c("January", "February","March","April","May","June","July","August","September","October","November","December"))


hist.m<-removal_PSSF %>% 
  ggplot(aes(x=month,fill="brown",color="black"))+
  geom_histogram(stat="count")+
  scale_x_discrete(drop=FALSE) + 
  scale_fill_discrete(drop=FALSE) +
  scale_fill_manual(values="#7A0403FF")+
  scale_color_manual(values="black")+
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))+
  labs(x="Month",y="Count",title="Pythons Removed by Month 2018-2025")+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45,vjust = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
hist.m



removal_PSSF$day<-yday(removal_PSSF$Date)


removal_PSSF$day<-as.numeric(removal_PSSF$day)
removal_PSSF$day2<-ifelse(removal_PSSF$day>=288,(removal_PSSF$day-288),(removal_PSSF$day+78))
removal_PSSF$day2<-as.numeric(removal_PSSF$day2)


hist.y<-removal_PSSF %>% filter(removal_PSSF$day2<=184) %>% 
  ggplot(aes(x=day2,fill="brown",color="black"))+
  geom_histogram(binwidth=7)+
  #scale_x_discrete()+
  #scale_x_discrete(drop=FALSE) +
  scale_fill_discrete(drop=FALSE) +
  scale_fill_manual(values="#7A0403FF")+
  scale_color_manual(values="black")+
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))+
  scale_x_continuous(breaks=c(1,32,61,93,124,153,184), labels=c("Oct. 15","Nov.15","Dec.15","Jan.15","Feb.15","Mar.15","Apr.15"),limits=c(1,190))+
  labs(x="Day",y="Count",title="Pythons Removed by Calendar Day 2018-2025")+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    panel.border = element_blank(),
    panel.background = element_blank()
  )
hist.y


hist.mult.scout<-removal_PSSF %>% filter( removal_PSSF$Date>2013-01-01) %>% 
  ggplot(aes(x=Date,fill="#7A0403FF",color="black"))+
  geom_histogram(binwidth=61,width=75)+
  #scale_x_discrete()+
  #scale_x_discrete(drop=FALSE) +
  scale_fill_discrete(drop=FALSE) +
  scale_fill_manual(values="#7A0403FF")+                                #specify viridis turbo palette
  scale_color_manual(values="black")+
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))+
  scale_x_date(labels = scales::date_format("%Y"),
               breaks = c(as.Date("2018-01-01"),as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"),as.Date("2023-01-01"),as.Date("2024-01-01"),as.Date("2025-01-01")),
               limits = c(as.Date("2018-11-01"), as.Date("2025-03-21"))) +
  # scale_x_continuous(limits=c(2013-01-01,2025-01-01))+
  #scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025), labels=c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025"),limits=c(2013,2025))+
  labs(x="Year",y="Count",title="PSSF Pythons Removed 2018-2025",fill="Removal Method",color="none")+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    panel.border = element_blank(),
    panel.background = element_blank()
  )+
  guides(colour = "none")
hist.mult.scout

#creating a seasonal removal plot
season.summ<-removal_PSSF %>% group_by(season) %>% summarise(n=n())
ggplot(data=season.summ,aes(x=season,y=n,fill=season))+
  geom_bar(stat="identity")+
  labs(x="Season",y="Count",title="Seasonal PSSF Pythons Removed 2018-2025")+
  scale_y_continuous(expand = expansion(mult = 0))+
  #scale_fill_viridis_d(option="turbo")+                                #specify viridis turbo palette
  #scale_fill_manual(values=c("red","orange","yellow","green","blue","violet","pink"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45,vjust = .5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    panel.border = element_blank(),
    panel.background = element_blank()
  )+
  guides(colour = "none")


# breeding season stats
summ <-removal_PSSF %>% filter(removal_PSSF$day2<=184) %>% 
  group_by(Sex) %>% 
  summarise(n=n(),
            mean.svl=mean(SVL_cm,na.rm=TRUE),
            max.svl=max(SVL_cm,na.rm=TRUE),
            min.svl=min(SVL_cm,na.rm=TRUE),
            mean.lb=mean(Weight_lbs,na.rm=TRUE),
            #mean.egg=mean(Egg.Count),
            max.lb=max(Weight_lbs,na.rm=TRUE),
            min.lb=min(Weight_lbs,na.rm=TRUE)) %>% 
  arrange(desc(n))

library(flextable)
library(kableExtra)

#create flextable of the data
ft<-flextable(summ)

#rename columns
ft<-set_header_labels(ft,
                      `Method_Grouped`="Method",
                      n="Sample Size",
                      mean.svl="Mean SVL (cm)",
                      mean.lb="Mean Mass (lb)",
                      #mean.egg="",
                      max.svl="Max SVL (cm)",
                      min.svl="Min SVL(cm)",
                      max.lb="Max Mass (lb)",
                      min.lb="Min Mass (lb)") %>% 
  colformat_double(big.mark = ",", digits = 2, na_str = "N/A") %>% #round columns to 2 decimals
  set_caption(as_paragraph("PSSF Python Removal Stats During Breeding Season (Oct.15-Apr.15) 2018-2025")) %>% 
  autofit()


#examine flex table
ft

##RBNERR Removal Graphics######################################################################################
# add sample size column and create axis label column
sample_size = removal_RBNERR %>% group_by(Sex) %>% summarize(num=n())

removal_RBNERR<-removal_RBNERR %>%
  left_join(sample_size) %>%
  mutate(myaxis = as.factor(paste0(Sex, "\n", "n=", num))) %>% 
  mutate( type=ifelse(Sex=="Male","Highlighted","Normal"))

#releveling factors so road cruising is first
removal_RBNERR$myaxis <- factor(removal_RBNERR$myaxis, levels = c('Female\nn=106', 'Male\nn=80'))

#releveling factors so male is first
#removal_RBNERR$Sex<-as.factor(removal_RBNERR$Sex)
#removal_RBNERR$Sex<-factor(removal_RBNERR$Sex, levels = c("Male", "Female"))

#boxplot of method by svl grouped by sex
bp<-removal_RBNERR %>% 
  filter(Sex!=is.na(Sex))%>%   #remove NA values from sex 
  ggplot()+
  aes(x=myaxis,y=SVL_cm, fill=Sex, na.rm=TRUE)+
  geom_boxplot()+
  scale_fill_viridis_d(option="turbo")+                                #specify viridis turbo palette
  #scale_alpha_manual(values=c(1,0.1))+
  labs(y="SVL (cm)",x="Sex",title="RBNERR Python Removals by Sex 2013-2025")+
  scale_fill_manual(name='Sex',                                          
                    breaks=c('Female','Male'),
                    values=c('Female'="pink",'Male'="skyblue"))+
  scale_alpha_manual(values=c(1,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=10),   #change x axis text size
        legend.position="none",
        axis.title.y = element_text(size=15, vjust=2),
        axis.title.x = element_text(size=15, vjust=-1),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )

bp

# removals over time
removal_RBNERR$Date<-as.Date(removal_RBNERR$Date,format="%Y-%m-%d")
removal_RBNERR$month<-base::format(removal_RBNERR$Date, format="%B")

#releveling month factors
removal_RBNERR$month<-as.factor(removal_RBNERR$month)
removal_RBNERR$month<-factor(removal_RBNERR$month, levels = c("January", "February","March","April","May","June","July","August","September","October","November","December"))


hist.m<-removal_RBNERR %>% 
  ggplot(aes(x=month,fill="brown",color="black"))+
  geom_histogram(stat="count")+
  scale_x_discrete(drop=FALSE) + 
  scale_fill_discrete(drop=FALSE) +
  scale_fill_manual(values="#7A0403FF")+
  scale_color_manual(values="black")+
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))+
  labs(x="Month",y="Count",title="Pythons Removed by Month 2013-2025")+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45,vjust = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
hist.m



removal_RBNERR$day<-yday(removal_RBNERR$Date)


removal_RBNERR$day<-as.numeric(removal_RBNERR$day)
removal_RBNERR$day2<-ifelse(removal_RBNERR$day>=288,(removal_RBNERR$day-288),(removal_RBNERR$day+78))
removal_RBNERR$day2<-as.numeric(removal_RBNERR$day2)


hist.y<-removal_RBNERR %>% filter(removal_RBNERR$day2<=184) %>% 
  ggplot(aes(x=day2,fill="brown",color="black"))+
  geom_histogram(binwidth=7)+
  #scale_x_discrete()+
  #scale_x_discrete(drop=FALSE) +
  scale_fill_discrete(drop=FALSE) +
  scale_fill_manual(values="#7A0403FF")+
  scale_color_manual(values="black")+
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))+
  scale_x_continuous(breaks=c(1,32,61,93,124,153,184), labels=c("Oct. 15","Nov.15","Dec.15","Jan.15","Feb.15","Mar.15","Apr.15"),limits=c(1,190))+
  labs(x="Day",y="Count",title="Pythons Removed by Calendar Day 2013-2025")+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    panel.border = element_blank(),
    panel.background = element_blank()
  )
hist.y


hist.mult.scout<-removal_RBNERR %>% filter( removal_RBNERR$Date>2013-01-01) %>% 
  ggplot(aes(x=Date,fill="#7A0403FF",color="black"))+
  geom_histogram(binwidth=61,width=75)+
  #scale_x_discrete()+
  #scale_x_discrete(drop=FALSE) +
  scale_fill_discrete(drop=FALSE) +
  scale_fill_manual(values="#7A0403FF")+                                #specify viridis turbo palette
  scale_color_manual(values="black")+
  scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))+
  scale_x_date(labels = scales::date_format("%Y"),
               breaks = c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"),as.Date("2023-01-01"),as.Date("2024-01-01"),as.Date("2025-01-01")),
               limits = c(as.Date("2013-01-01"), as.Date("2025-03-21"))) +
  # scale_x_continuous(limits=c(2013-01-01,2025-01-01))+
  #scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025), labels=c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025"),limits=c(2013,2025))+
  labs(x="Year",y="Count",title="RBNERR Pythons Removed 2013-2025",fill="Removal Method",color="none")+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    panel.border = element_blank(),
    panel.background = element_blank()
  )+
  guides(colour = "none")
hist.mult.scout

#creating a seasonal removal plot
season.summ<-removal_RBNERR %>% group_by(season) %>% summarise(n=n())
ggplot(data=season.summ,aes(x=season,y=n,fill=season))+
  geom_bar(stat="identity")+
  labs(x="Season",y="Count",title="Seasonal PSSF Pythons Removed 2018-2025")+
  scale_y_continuous(expand = expansion(mult = 0))+
  #scale_fill_viridis_d(option="turbo")+                                #specify viridis turbo palette
  #scale_fill_manual(values=c("red","orange","yellow","green","blue","violet","pink"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=45,vjust = .5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank()
    panel.border = element_blank(),
    panel.background = element_blank()
  )+
  guides(colour = "none")

# breeding season stats
summ <-removal_RBNERR %>% filter(removal_RBNERR$day2<=184) %>% 
  group_by(Sex) %>% 
  summarise(n=n(),
            mean.svl=mean(SVL_cm,na.rm=TRUE),
            max.svl=max(SVL_cm,na.rm=TRUE),
            min.svl=min(SVL_cm,na.rm=TRUE),
            mean.lb=mean(Weight_lbs,na.rm=TRUE),
            #mean.egg=mean(Egg.Count),
            max.lb=max(Weight_lbs,na.rm=TRUE),
            min.lb=min(Weight_lbs,na.rm=TRUE)) %>% 
  arrange(desc(n))

#create flextable of the data
ft<-flextable(summ)

#rename columns
ft<-set_header_labels(ft,
                      `Method_Grouped`="Method",
                      n="Sample Size",
                      mean.svl="Mean SVL (cm)",
                      mean.lb="Mean Mass (lb)",
                      #mean.egg="",
                      max.svl="Max SVL (cm)",
                      min.svl="Min SVL(cm)",
                      max.lb="Max Mass (lb)",
                      min.lb="Min Mass (lb)") %>% 
  colformat_double(big.mark = ",", digits = 2, na_str = "N/A") %>% #round columns to 2 decimals
  set_caption(as_paragraph("RBNERR Python Removal Stats During Breeding Season (Oct.15-Apr.15) 2013-2025")) %>% 
  autofit()


#examine flex table
ft

