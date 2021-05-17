#load packages needed for analysis
library('ggplot2')
library('dplyr')
library('car')
library('cowplot')
library('RVAideMemoire')
library('lme4')
library('lmerTest')
#create a theme for plots
mytheme <-   theme(#panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                   #panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                   #panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                   #panel.border=element_blank(), #gets rid of square going around the entire graph
                   axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
                   axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
                   axis.title.x = element_text(size=14, color="black", face="bold"), #size of x-axis title
                   axis.title.y = element_text(size=14, color="black", face="bold"), #size of y-axis title
                   axis.text.x = element_text(size=12, color="black"), #size of x-axis text
                   axis.text.y = element_text(size=12, color="black"))#size of y-axis text

#load in the dataset, which I converted from an excel file
killi_data <- read.csv("Johnson2013peckdata.csv")
head(killi_data)

#edit column names so they don't have weird symbols
killi_data %>% rename(Time = ï..Time. ,Tank = Tank.,Water = Water.)
killi_data_fixed <- killi_data %>% rename(Time = ï..Time. ,Tank = Tank.,Water = Water.)

#create a graphs showing data trends before model
red_graph <- ggplot(killi_data_fixed, aes(x=Time, y=Red,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of Red Pecks")+
  ggtitle("Red Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,55),breaks=seq(0,55,5), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

yellow_graph <- ggplot(killi_data_fixed, aes(x=Time, y=Yellow,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of Yellow Pecks")+
  ggtitle("Yellow Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,10),breaks=seq(0,10,2), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

orange_graph <- ggplot(killi_data_fixed, aes(x=Time, y=Orange,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of Orange Pecks")+
  ggtitle("Orange Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,20),breaks=seq(0,20,5), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

blue_graph <- ggplot(killi_data_fixed, aes(x=Time, y=Blue,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of Blue Pecks")+
  ggtitle("Blue Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,20),breaks=seq(0,20,5), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

green_graph <- ggplot(killi_data_fixed, aes(x=Time, y=Green,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of Green Pecks")+
  ggtitle("Green Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,10),breaks=seq(0,10,2), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

black_graph <- ggplot(killi_data_fixed, aes(x=Time, y=Black,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of Black Pecks")+
  ggtitle("Black Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,4),breaks=seq(0,4,1), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

white_graph <- ggplot(killi_data_fixed, aes(x=Time, y=White,color = Water))+
  scale_shape_identity()+
  facet_grid(~Water)+
  geom_point(size = 4)+
  labs(x="Timepoint in Diurnal", y="Number of White Pecks")+
  ggtitle("White Pecks by Timepoint in Water Type")+
  scale_y_continuous(limits =c(-2,4),breaks=seq(0,4,1), expand=c(0,0))+
  scale_x_continuous(limits =c(0,4),breaks=seq(1,3,1), expand=c(0,0))+
  mytheme

plot_grid(red_graph,yellow_graph,orange_graph,blue_graph,green_graph,black_graph,white_graph)

#Calculate proportions for each color
killi_data_fixed$Red_prop <- killi_data_fixed$Red / killi_data_fixed$TotalPeck

killi_data_fixed$Orange_prop <- killi_data_fixed$Orange / killi_data_fixed$TotalPeck

killi_data_fixed$Yellow_prop <- killi_data_fixed$Yellow / killi_data_fixed$TotalPeck

killi_data_fixed$Blue_prop <- killi_data_fixed$Blue / killi_data_fixed$TotalPeck

killi_data_fixed$Green_prop <- killi_data_fixed$Green / killi_data_fixed$TotalPeck

killi_data_fixed$Black_prop <- killi_data_fixed$Black / killi_data_fixed$TotalPeck

killi_data_fixed$White_prop <- killi_data_fixed$White / killi_data_fixed$TotalPeck

killi_data_fixed$time_as_factor <- as.factor(killi_data_fixed$Time)

#Create a generalized linear model for the dataset
#used to model the number of pecks each color received out of the total number of pecks
red_pecks_glm <- glm(Red_prop ~  time_as_factor + Water, weights = TotalPeck, family = quasibinomial(link="logit"), data = killi_data_fixed)
summary(red_pecks_glm)
Anova(red_pecks_glm, type = 3)
Anova(red_pecks_glm, test = 'F')

orange_pecks_glm <- glm(Orange_prop ~ time_as_factor + Water, weights = TotalPeck, family = quasibinomial(link="logit"), data = killi_data_fixed)
summary(orange_pecks_glm)
Anova(orange_pecks_glm, type = 3)

blue_pecks_glm <- glm(Blue_prop ~ time_as_factor + Water, weights = TotalPeck, family = quasibinomial(link="logit"), data = killi_data_fixed)
summary(blue_pecks_glm)
Anova(blue_pecks_glm, type = 3)

green_pecks_glm <- glm(Green_prop ~ time_as_factor + Water, weights = TotalPeck, family = quasibinomial(link="logit"), data = killi_data_fixed)
summary(green_pecks_glm)
Anova(green_pecks_glm, type = 3)

yellow_pecks_glm <- glm(Yellow_prop ~ time_as_factor + Water, weights = TotalPeck, family = quasibinomial(link="logit"), data = killi_data_fixed)
summary(yellow_pecks_glm)
Anova(yellow_pecks_glm, type = 3)



#Create a graph to visualize the model to to the data
red_plot <- ggplot(killi_data_fixed, aes(x=Time, y=Red_prop,fill=Water)) + 
  geom_point(shape=21, position=position_jitter(width=0.05, height=0.05)) + 
  geom_smooth(method = "glm", method.args = list(family = 'quasibinomial')) +
  mytheme

orange_plot <- ggplot(killi_data_fixed, aes(x=Time, y=Orange_prop, fill = Water)) + 
  geom_point(shape=21, position=position_jitter(width=0.05, height=0.05)) + 
  geom_smooth(method = "glm", method.args = list(family = 'quasibinomial')) +
  mytheme

yellow_plot <- ggplot(killi_data_fixed, aes(x=Time, y=Yellow_prop, fill = Water)) + 
  geom_point(shape=21, position=position_jitter(width=0.05, height=0.05)) + 
  geom_smooth(method = "glm", method.args = list(family = 'quasibinomial')) +
  mytheme

green_plot <- ggplot(killi_data_fixed, aes(x=Time, y=Green_prop, fill = Water)) + 
  geom_point(shape=21, position=position_jitter(width=0.05, height=0.05)) + 
  geom_smooth(method = "glm", method.args = list(family = 'quasibinomial')) +
  mytheme

blue_plot <- ggplot(killi_data_fixed, aes(x=Time, y=Blue_prop, fill = Water)) + 
  geom_point(shape=21, position=position_jitter(width=0.05, height=0.05)) + 
  geom_smooth(method = "glm", method.args = list(family = 'quasibinomial')) +
  mytheme

#make a plotgrid to export all the GLM plots
plot_grid(red_plot,orange_plot,yellow_plot,green_plot,blue_plot)


