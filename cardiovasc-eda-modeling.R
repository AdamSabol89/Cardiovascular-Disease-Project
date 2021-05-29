#Load libraries
library(ggplot2)
library(tidyr)
library(plm)
library(gridExtra)
library(grid)
library(ggpubr)
ggplot( final_data, 
        aes( Year, cardiovasc_deaths, color = Entity)) + 
  geom_line()
# group by sort by nth gdp grab top 6 then graph 
grouped_rates <- final_data %>%  
  group_by(Entity) %>% 
  summarise(obesity_rate = mean(obesity), cardio_death_rate = mean(cardiovasc_deaths), sev_obesity_rate = mean(severe_obesity), gdp_country =mean(GDP_Per_Capita))

#Plot 
ggplot(grouped_rates, 
       aes(obesity_rate, gdp_country))+
  geom_point()

#Check specific year
year_2010 <- final_data %>% 
  filter( Year == 2010)

year_2010<-year_2010[order(-year_2010$GDP),]  

ggplot(year_2010[1:20,], 
       aes(obesity, cardiovasc_deaths ))+
  geom_point()

final_data %>%
  filter(Entity == "United Kingdom")

final_data$Entity %>% 
  unique() %>% 
  length()/3

#Lets plot the top income countries by our feature variables
sorted<- final_data %>%  
  arrange(desc(GDP_Per_Capita)) 
list <- unique(sorted$Entity) 
list[1:10]

rich_countries<-final_data %>% 
  filter(final_data$Entity %in% list[1:10]) 

#plots the data
plot1 <- ggplot( rich_countries, 
           aes( Year, cardiovasc_deaths, color = Entity)) + 
     geom_line(size=1) +
  labs(x= '', y ='Disease Deaths Per 100,000')

plot2 <-ggplot( rich_countries, 
          aes( Year, percent_obesity, color = Entity)) + 
      geom_line(size=1) +
  labs(x= '', y ='Obesity Rate')


plot3 <-ggplot( rich_countries, 
                aes( Year, GDP_Per_Capita, color = Entity)) + 
  geom_line(size=1) +
  labs(x= '', y ='Per Capita GDP')


figure<-ggarrange(plot1, plot2, plot3, nrow=3,common.legend = TRUE,legend="right")

annotate_figure(figure,
                top = text_grob("Time Series for Top 10 Wealthiest Countries", color = "black", size = 20))

#Scatterplot for years 1995,2005, 2015 
segment_wealth <- function(req_year, segment_by){
  
  data_req_year <- final_data %>% 
    filter( Year  == req_year )
  
  data_req_year <- data_req_year %>%  
    arrange(desc(GDP_Per_Capita))
  
  data_req_year<-data_req_year %>% 
   na.omit(GDP_Per_Capita)
  
  data_req_year$Group <- 1
  
  segment_length<- floor(length(data_req_year$GDP_Per_Capita) /segment_by)
  for( i in 1:segment_by){
    if(i == 1){
      data_req_year[i:(i*segment_length),10] <- i
    }
    if(i == segment_by){
      data_req_year[((i-1)*segment_length):length(data_req_year$GDP_Per_Capita),10] <- i 
      
    }
    else{
  data_req_year[((i-1)*segment_length):(i*segment_length),10] <- i
    }
  }
  return(data_req_year)
}



###Plotting corelations over time 
data_2000<-segment_wealth(2000,3)
data_2008 <- segment_wealth(2008, 3)
data_2000_3<-data_2000 %>% 
  filter(Group == 3)
  ggplot( data_2000_3, 
          aes(percent_severe_obesity, cardiovasc_deaths,))+
    geom_point() 

#cor(data_2000_1$percent_obesity, data_2000_1$cardiovasc_deaths)
  
#data_1995<-segment_wealth(1995,3)
p1<-ggplot(data_1995, 
       aes(percent_obesity, cardiovasc_deaths, color=factor(Group)))+
  geom_point(size=3)+
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title = "1995", legend = '')+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "Country Group", labels = c("High Income", "Middle Income", "Low Income"))

#data_2005<-segment_wealth(2005,3)
p2<-ggplot(data_2005, 
       aes(percent_obesity, cardiovasc_deaths, color=factor(Group)))+
  geom_point(size=3)+
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title = "2005", legend = '')+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "Country Group", labels = c("High Income", "Middle Income", "Low Income"))

#data_2015<-segment_wealth(2015,3)
p3<-ggplot(data_2015, 
       aes(percent_obesity, cardiovasc_deaths, color=factor(Group)))+
  geom_point(size=3)+
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title = "2015", legend = '')+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "Country Group", labels = c("High Income", "Middle Income", "Low Income"))

figure2<-ggarrange(p1, p2, p3, ncol = 3,common.legend = TRUE,legend="right")

annotate_figure(figure2,
                top = text_grob("Cardiovascular Disease vs. Percent Obesity by Country's Wealth", color = "black", size = 20))



corelations<-data_2000 %>% 
  group_by(factor(Group)) %>% 
  summarise(cor(percent_obesity, cardiovasc_deaths))
  corelations[,3]<- 2000

final_data$Year %>% 
  unique() %>% 
  length()