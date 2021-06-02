#Load libraries
library(ggplot2)
library(tidyr)
library(plm)
library(gridExtra)
library(grid)
library(plotly)
library(ggpubr)
library(GGally)

# Create a Scatterplot matrix
#Group the data over all years and find means
grouped_rates <- final_data %>%  
  group_by(Entity) %>% 
  summarise(cardio_death_rate = mean(cardiovasc_deaths), obesity_rate = mean(percent_obesity), sev_obesity_rate = mean(percent_severe_obesity), gdp_country =mean(GDP_Per_Capita), meanbmi = mean(mean_bmi))

#Scatterplot matrix
ggpairs(grouped_rates[,2:6])

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

#Now we segment data and plot it 

#Plot for 95
data_1995<-segment_wealth(1995,3)
p1<-ggplot(data_1995, 
       aes(percent_obesity, cardiovasc_deaths, color=factor(Group), text =paste0("Country: " ,Entity)))+
  geom_point(size=3)+
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title = "1995", legend = '')+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "Country Group", labels = c("High Income", "Middle Income", "Low Income"))

#Plot for 05
data_2005<-segment_wealth(2005,3)
p2<-ggplot(data_2005, 
       aes(percent_obesity, cardiovasc_deaths, color=factor(Group), text =paste0("Country: " ,Entity)))+
  geom_point(size=3)+
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title = "2005", legend = '')+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "Country Group", labels = c("High Income", "Middle Income", "Low Income"))

#Plot for 15
data_2015<-segment_wealth(2015,3)
p3<-ggplot(data_2015, 
       aes(percent_obesity, cardiovasc_deaths, color=factor(Group), (text =paste0("Country: " ,Entity) )))+
  geom_point(size=3)+
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title = "2015", legend = '')+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "Country Group", labels = c("High Income", "Middle Income", "Low Income"))

#Arrange plots on a grid
figure2<-ggarrange(p1, p2, p3, ncol = 3,common.legend = TRUE,legend="right")

#Add title
annotate_figure(figure2,
                top = text_grob("Cardiovascular Disease vs. Percent Obesity by Country's Wealth", color = "black", size = 20))


#Make an interactive version of the plot 
#Change group variable
scatter_data <- rbind(data_1995, data_2005, data_2015)
scatter_data$Group<-scatter_data$Group %>% 
  as.character()
scatter_data[scatter_data == "1"] <-"High Income"
scatter_data[scatter_data == "2"] <-"Middle Income"
scatter_data[scatter_data == "3"] <-"Low Income"

#Create the figure
fig<-ggplot(scatter_data, 
       aes(percent_obesity, cardiovasc_deaths, text = paste0("Country: ", Entity), color=Group))+
  geom_point(size=2)+
  facet_wrap(~Year, ncol = 3) +
  labs( x="Percent of Population Obese", y="Cardiovascular Disease Deaths", title ="Cardiovascular Disease vs. Obesity by Income Group")+
  theme(plot.title = element_text(hjust = .5,size = 20, face = "bold")) +
  scale_color_discrete(name = "")
ggplotly(fig, tooltip = "text") %>% 
  layout(showlegend = TRUE, legend=list(title=list(text='<b> Country Group </b>')), yaxis = list(fixedrange = TRUE,showgrid = F),
         xaxis = list(fixedrange = TRUE,showgrid = F, hovermode = "x")) %>% 
  config(displayModeBar = FALSE)

#Graph a timeseries of the corelation between our response and explanatory variables
#Create the function
get_corelations <- function(){
  corelations <- data.frame()
  for(i in 1990:2016){
  yeardata <- segment_wealth(i, 3)
  yeardata <-yeardata %>% 
    group_by(factor(Group)) %>% 
    summarise(cor(GDP_Per_Capita, cardiovasc_deaths), cor(percent_obesity, cardiovasc_deaths), cor(percent_severe_obesity, cardiovasc_deaths), cor(mean_bmi, cardiovasc_deaths))
  yeardata[,6] <- i
  corelations <- rbind(corelations, yeardata)
  }
  names(corelations) <-c("Group","GDP_cor", "Obesity_cor", "sev_obesity_cor", "mean_bmi_cor", "Year")
  corelations$Group<-corelations$Group %>% 
    as.character()
  corelations[corelations == "1"] <-"High Income"
  corelations[corelations == "2"] <-"Middle Income"
  corelations[corelations == "3"] <-"Low Income"
  return(corelations)
}

finalcorelations <- get_corelations()

#Plot the Time series 
fig1<-ggplot(finalcorelations, aes(Year, Obesity_cor, color = Group))+
  labs( y="% Obese", x="Years")+
  geom_line(linetype= "twodash", size=2)+
  theme_classic2()

fig2<-ggplot(finalcorelations, aes(Year, sev_obesity_cor, color = Group))+
  labs( y="% Severely Obese", x="Years")+
  geom_line(linetype= "twodash", size=2)

fig3<-ggplot(finalcorelations, aes(Year, mean_bmi_cor, color = Group))+
  labs( y="Mean BMI ", x="Years")+
  geom_line(linetype= "twodash", size=2)

fig4<-ggplot(finalcorelations, aes(Year, GDP_cor, color = Group))+
  labs( y="GDP Per Capita", x="Years")+
  geom_line(linetype= "twodash", size=2)

#Arrange into grid
figure3<-ggarrange(fig1, fig2, fig3, fig4, nrow=4,common.legend = TRUE,legend="right")

#Add title
annotate_figure(figure3,
                top = text_grob("Time Series of Correlation between Cardiovascular Disease and Explanatory Variables", color = "black", size = 16))