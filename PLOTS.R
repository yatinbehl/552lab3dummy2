# For all the plots:

library(ggplot2)
library(plotly)
library(dplyr)
library(ggthemes)
library(dplyr)

#install.packages("gghighlight")

# Read the required data set:
data <- read.csv("data/master.csv")

# Reaname the column as country:
names(data)[1] <- 'country'

# Aggregate the required dataset
country_data <- data %>%  
  group_by(data$country) %>%
  summarise(
    suicides_no = mean(suicides_no)
  )

# Rename the columns so that they are helpful for merging:
names(country_data) <- c("region", "value")



# Read the required chloropeth dataset:
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
names(df)[1] <- "region"

# Read the required suicide dataset:
suicide_data <- read.csv("data/master_HDI.csv")

#setdiff(country_data$region, df$region)

# Rename the missing countries:
country_data[country_data$region == "Russian Federation",]$region <- "Russia"

# Create a merged data set:
suicide_df <- left_join(df, country_data, by= "region")

# The code for the world map:

plot1 <- function(){
  # light grey boundaries
  l <- list(color = toRGB("grey"))
  # specify map projection/options
  g <- list(
    showframe = TRUE,
    showland = TRUE,
    showcoastlines = FALSE,
    showcountries = FALSE,
    countrycolor = toRGB("white"),
    projection = list(type = 'Mercator'),
    landcolor = toRGB("grey90")
  )
  fig <- plot_geo(suicide_df)
  fig <- fig %>% add_trace(
    z = ~value, color= ~value,
    colors = 'plasma',
    text = ~region, locations = ~CODE, marker = list(line = l)
  )
  fig <- fig %>% layout(
    geo = g,
    width = 800,
    height = 300,
    paper_bgcolor = '#F5F5DC'
  )
  return(fig)
}


# Yatin's Graph:
plot1_data<- read.csv("data/line_data.csv")
plot1_data$year <- as.Date(plot1_data$year,format="%Y")
plot1_data$age <- as.factor(plot1_data$age)
head(plot1_data)

plot2 <- function(age, country){
  plot1_data <- plot1_data[plot1_data$age == age,]
  plot1_data <- plot1_data[plot1_data$country == country,]
  g <- ggplot(plot1_data, aes(x = year, y = Average_suicides_per_capita, colour = age))+geom_line() +
    labs(title="Average Suicides per Capita by year Colored by Age",
         x="year",
         y="Average_suicides_per_capita")
  g1 <- g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  g1 <- g1+theme(axis.text=element_text(size=5),
                 axis.title=element_text(size=5,face="bold"),
                 legend.title = element_text(color = "black", size = 5),
                 legend.text = element_text(color = 'black', size = 5))
  return(ggplotly(g1))
}


# Poojitha's Graph:
diverge_gender <- read.csv("data/diverge_data.csv")
head(diverge_gender)

x <- diverge_gender[order(diverge_gender$Average_suicides_per_capita, decreasing = TRUE),]
top_n <- y[1:20]
diverge_gender_plot <- diverge_gender[diverge_gender$country %in% top_n,]

diverge_gender_plot
diverge_gender_plot <- diverge_gender_plot %>%
  mutate(Average_suicides_per_capita = ifelse(sex == "male",
                                              Average_suicides_per_capita,
                                              -1*Average_suicides_per_capita))

plot3 <- function(){
  p1 <- diverge_gender_plot %>%
    ggplot(aes(x = country, y = Average_suicides_per_capita, fill = sex))+
    geom_bar(stat = "identity")+
    coord_flip()+labs(title="Sex based factors")
  p1 <- p1+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p2 <- p1+theme(axis.text=element_text(size=5),
           axis.title=element_text(size=5,face="bold"),
           plot.title = element_text(size=5),
           legend.position = 'top',
           legend.title = element_blank(),
           legend.text = element_text(size = 5))
  return(ggplotly(p2))
}


# Aditya plot:

plot4 <- function(countries, gender){
  data_subset <- subset(suicide_data, country==countries & sex==gender)
  p4 <- ggplot(data_subset, aes(x=year, y=suicides.100k.pop)) + 
    geom_point(aes(size = HDI, color = age)) +
    ggtitle('Plot of Suicides per 100k by Year') +
    xlab('Year') + ylab('Suicides per 100k Population')
  return(ggplotly(p4))
}

# Sowmya's plot
plot5 <- function(country_data){
  data[data$generation=='G.I. Generation',]$generation <- '1.G.I'
  data[data$generation=='Silent',]$generation <- '2.Silent'
  data[data$generation=='Boomers',]$generation <- '3.Boomers'
  data[data$generation=='Generation X',]$generation <- '4.Gen X'
  data[data$generation=='Millenials',]$generation <- '5.Millenials'
  data[data$generation=='Generation Z',]$generation <- '6.Gen Z'
  p5 <- ggplot(data = data)+
    aes(x = data$generation,
        y = data$suicides.100k.pop,
        color = generation,
    )+
    geom_boxplot(text = paste('generation', data$generation))+
    facet_wrap(~data$sex)+
    #ggtitle("Suicides per 100k vs generation")+
    ylab("Suicides per 100k")+
    xlab("Generation")+theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),
          legend.position = "bottom")
  p5
    
  return(ggplotly(p5) %>%
           layout(legend = list(orientation= "h", x= 0, y= 0)
                  ))
}

# Aditya's plot:

plot6 <- function(){
  data_subset <- subset(data, country=='Canada' & sex=='male' & age=='15-24 years')
  g6 <- ggplot(data_subset, aes(x=year, y=suicides.100k.pop)) + 
    geom_point(aes(size = 'gdp_per_capita')) +
    ggtitle('Plot of Suicides per 100k by Year') +
    xlab('Year') + ylab('Suicides per 100k Population')
  
  return(ggplotly(g6))
}


