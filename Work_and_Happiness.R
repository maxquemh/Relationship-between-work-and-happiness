################################################################################
# Author: Max Que
# Date: 09 Sep 2024
################################################################################

################################################################################
#Loading libraries and setting working directory
################################################################################

#loads tidyverse package which consists of dpylr package for data manipulation
#and ggplot package for data visualisation
library(tidyverse)
#readxl package is used for loading data from Excel spreadsheets
library(readxl)
#countrycode package is used to provide standardised country codes for each country 
library(countrycode)
#ggrepel package is used for generating labels in graphs and prevent them from overlapping
library(ggrepel)
#ggpubr package is used for further customisation of 'ggplot' graphs,
#e.g. adding correlation coefficients with p-values to a scatter plot 
library(ggpubr)
#gt package is used to create tables
library(gt)
#gganimate package is used for creating animated graphs
library(gganimate)

#sets local working directory where the datasets are located
setwd("C:/Users/sunca/Documents/SUSS/ANL501/TMA") 


################################################################################
################################################################################
# Data preparation on working hours
################################################################################
################################################################################


#import working hours (total) for OECD countries and Singapore
oecdtotalhours <- read_excel("ANL501_TMA_JUL24_HOURS.xlsx", sheet = "OECD Total")
sghours <- read_excel("ANL501_TMA_JUL24_HOURS.xlsx", sheet = "Singapore Total")

#data profiling of 'oecdtotalhours' data frame
str(oecdtotalhours)
head(oecdtotalhours) %>%
  print(width=Inf)

#pivots data in OECD Total from wide to long
pivottotalhours <- oecdtotalhours %>% 
  pivot_longer(!Country,names_to="year",values_to = "hours")
pivottotalhours
#converts all Year entries from character to numerical (integer)
pivottotalhours$year <- as.integer(pivottotalhours$year)
print(pivottotalhours[!complete.cases(pivottotalhours),], n=Inf)  

#data profiling of 'sghours' data frame
str(sghours)
head(sghours) %>%
  print(width=Inf)

#rename the first column to Country to align with columns in OECD Total
names(sghours)[names(sghours) == "Variable"] <- "Country"
#extracts the Annual Average row in Singapore Total and renames the first entry to Singapore as the row name 
sgannualhours <- sghours[1,]
sgannualhours[1] <- "Singapore"
#converts entries in 2015 column from character to numeric data type
sgannualhours$"2015" <- as.numeric(sgannualhours$"2015")
sgannualhours %>%
  print(width=Inf)

#pivots sgannualhours from wide to long
pivotsghours <- sgannualhours %>% 
  pivot_longer(!Country,names_to="year",values_to = "hours")
#converts all Year entries from character to numerical (integer)
pivotsghours$year <- as.integer(pivotsghours$year)
#filters out rows with missing values
pivotsghours[!complete.cases(pivotsghours),]
#the '2022' value was changed to NA when converting year from char to int type. Now we shall introduced '2022' back
pivotsghours[is.na(pivotsghours$year),"year"] <- 2022
pivotsghours

#merges data from OECD Total and Singapore Total into one dataframe
workhours <- full_join(pivottotalhours,pivotsghours)
#creates a new column for country codes. Here the country names are converted to GENC 3-letter codes
workhours <- workhours %>% 
  mutate(country.code = countrycode(workhours$Country,origin = 'country.name', destination = 'genc3c'))
#checks for missing values in "workhours" data frame
workhours[!complete.cases(workhours),]

#loads the ILO dataset and save it in "countrytotalhours" variable
countrytotalhours <- read_csv("Working hours by sex and occupation.csv")
#renames the column to have the names "Country", "year" and "hours" as the column names
names(countrytotalhours)[names(countrytotalhours) %in% c("ref_area.label","time","obs_value")] <- c("Country","year","hours")
#converts the values in the "year" column to integer type
countrytotalhours$year <- as.integer(countrytotalhours$year)
#creates a new column for country codes. Here the country names are converted to GENC 3-letter codes
countrytotalhours <- countrytotalhours %>% 
  mutate(country.code = countrycode(countrytotalhours$Country,origin = 'country.name', destination = 'genc3c'))

#filters out data on South Korea from the ILO dataset (stored in "countrytotalhours" variable) and saved as "koreahours" variable
koreahours <- filter(countrytotalhours, country.code == "KOR",
       sex.label == "Sex: Total", classif1.label == "Occupation (ISCO-08): Total") %>%
  subset(select=c("Country","year","hours")) %>%
  arrange(year)
#missing data on working hours in South Korea is filled by the "koreahours" data, which itself is from the ILO dataset 
workhours[workhours$country.code == "KOR","hours"] <- subset(filter(koreahours, year %in% c(2010:2022)), select = "hours")


countrytotalhours[countrytotalhours$Country == "Japan", "country.code"]

#filters out data on Canada from the ILO dataset and saved as "canadahours" variable
canadahours <- filter(countrytotalhours, country.code == "CAN",
                     sex.label == "Sex: Total", classif1.label == "Occupation (ISCO-88): Total") %>%
  subset(select=c("Country","year","hours","country.code")) %>%
  arrange(year)
#adding rows on working hours in Canada to "workhours" data frame
workhours <- rbind(workhours,canadahours[canadahours$year %in% c(2010:2022),])

#filters out data on Japan from the ILO dataset and saved as "japanhours" variable
japanhours <- filter(countrytotalhours, country.code == "JPN",
                      sex.label == "Sex: Total", classif1.label == "Occupation (ISCO-08): Total") %>%
  subset(select=c("Country","year","hours","country.code")) %>%
  arrange(year)
#adding rows on working hours in Canada to "workhours" data frame
workhours <- rbind(workhours,japanhours[japanhours$year %in% c(2010:2022),])


################################################################################
################################################################################
# Data preparation on working conditions
################################################################################
################################################################################


#import data on working conditions from ANL501_TMA_JUL24_WORKING.xlsx
workcond <- read_excel("ANL501_TMA_JUL24_WORKING.xlsx")

#data profiling on workcond
str(workcond)
head(workcond)%>%
  print(width=Inf)

#renaming column names to shorter ones
colnames(workcond) <- c("Country","Economy Code","year","leave_1year","leave_5_ear","leave_10year","leave_avg","sevpay_1year","sevpay_5year","sevpay_10year","sevpay_avg","unemployment_protection")
#saves severance pay column names to a variable "workcondchar"
workcondchar <- c("sevpay_1year", "sevpay_5year", "sevpay_10year", "sevpay_avg")
#using sapply to convert values in severance pay columns to numeric type
workcond[workcondchar] <- sapply(workcond[workcondchar] ,as.numeric)
#converts values under the "year" column from numeric to integer type
workcond$year <- as.integer(workcond$year)

#checks structure of 'workcond' to ensure data types are correct
str(workcond)

#creates a new column for country codes
workcond <- workcond %>% 
  mutate(country.code = countrycode(workcond$Country,origin = 'country.name', destination = 'genc3c'))

#checks for any missing values in workcond
print(workcond[!complete.cases(workcond),], width=Inf, n=Inf)


################################################################################
################################################################################
# Data preparation on happiness
################################################################################
################################################################################


#saves ANL501_TMA_JUL24_HAPPINESS.csv data to a variable
happiness <- read_csv("ANL501_TMA_JUL24_HAPPINESS.csv")

#data profiling of 'happiness' data frame
str(happiness)
head(happiness)%>%
  print(width=Inf)
#renaming the column name from 'Country name' to 'Country'
names(happiness)[names(happiness) == "Country name"] <- "Country"
#converts values under the year column from numeric to integer
happiness$year <- as.integer(happiness$year)
#creates a new column for country codes
happiness <- happiness %>% 
  mutate(country.code = countrycode(happiness$Country,origin = 'country.name', destination = 'genc3c'))
#checks for missing values in 'happiness'
print(happiness[!complete.cases(happiness),], width=Inf, n=Inf)
#it is noted that there is no country code for Somaliland region in the "happiness" data frame.
#and that country code is assigned as "NA". This is not an issue as there is no data on Somaliland region in the other datasets
#so there is no concern when this dataset is joined to other datasets by the country codes.


################################################################################
################################################################################
# Data visualisation on GDP per Capita and happiness
################################################################################
################################################################################


######### Creating lifeladder function #########

#this creates a function that first groups 'happiness' data by country, then aggregate it by mean life ladder, 
#followed by taking the top or bottom x of the mean values (depending on the input) and
#finally this function filters out countries that match the country names that are top or bottom x in life ladder values
lifeladderfun <- function(x, ladderstats="max1"){
  if(ladderstats== "max1"){
    topladderdf <- happiness %>% group_by(`Country`) %>%
      summarise(avglifeladder = mean(`Life Ladder`, na.rm = TRUE)) %>% 
      slice_max(avglifeladder, n = x)
    filter(happiness, `Country` %in% topladderdf$`Country`)
  } else if(ladderstats== "min1"){
    bottomladderdf <- happiness %>% group_by(`Country`) %>%
      summarise(avglifeladder = mean(`Life Ladder`, na.rm = TRUE)) %>% 
      slice_min(avglifeladder, n = x)
    filter(happiness, `Country` %in% bottomladderdf$`Country`)
  }
}

######### Preparation for Life Ladder plotting #########

#'laddergroupmax' variable contains only the top countries in mean Life Ladder from the 'happiness' data frame
laddergroupmax <- lifeladderfun(10,"max1")
laddergroupmax$Country <- as.factor(laddergroupmax$Country)
#"laddergroup" variable is created to be used in labelling the names of the top
#and bottom countries in mean Life Ladder, in the plot. "laddergroup" consists of both top and bottom countries in Life Ladder from the 'happiness' data frame
laddergroup <- full_join(lifeladderfun(10,"max1"),lifeladderfun(10,"min1"))
laddergroup$Country <- as.factor(laddergroup$Country)
#mutate function is used below to give country labels for the latest year (2023) that particular country has data on Life Ladder under the column "ladder_label"
#and to categorise whether the country is top 10 or bottom 10 in Life Ladder under the column "lifeladderscore" in the "laddergroup" data frame
laddergroup <- laddergroup  %>% 
  mutate(ladder_label = if_else(year == 2023, Country, NA_character_)) %>%
  mutate(lifeladderscore = if_else(Country %in% unique(laddergroupmax$Country), "Top 10 in Life Ladder","Bottom 10 in Life Ladder"))
#For these countries below, the latest year that they have data on Life Ladder is not 2023 but other years. So they are individually
#labelled in the "ladder_label" column for the latest year that they have data on Life Ladder
laddergroup[laddergroup$Country == "Rwanda" & laddergroup$year == 2019,"ladder_label"] <- "Rwanda"
laddergroup[laddergroup$Country == "South Sudan" & laddergroup$year == 2017,"ladder_label"] <- "South Sudan"
laddergroup[laddergroup$Country == "Burundi" & laddergroup$year == 2018,"ladder_label"] <- "Burundi"
laddergroup[laddergroup$Country == "Central African Republic" & laddergroup$year == 2017,"ladder_label"] <- "Central African Republic"
laddergroup[laddergroup$Country == "Lesotho" & laddergroup$year == 2022,"ladder_label"] <- "Lesotho"

#factor function is used here to help to re-order legends in the plot
laddergroup$lifeladderscore <- factor(laddergroup$lifeladderscore, levels = c("Top 10 in Life Ladder","Bottom 10 in Life Ladder"))

######### Plot for Life Ladder #########

#p1 generates a line plot of the Life Ladder over the years for each country
p1 <- ggplot(mapping=aes(x=year,y=`Life Ladder`)) + 
  geom_line(data=happiness, aes(group=`Country`), colour = alpha("grey", 0.7)) +
  geom_line(aes(colour = `Country`), data = lifeladderfun(10,"max1")) +
  geom_line(aes(colour = `Country`), data = lifeladderfun(10,"min1")) + 
  guides(color = "none") 
#q1 creates labels for the top and bottom countries in mean Life Ladder
#"laddergroup" data frame consists only of both top 10 and bottom 10 countries in Life Ladder from the 'happiness' data frame
#and "ladder_label" is a column in "laddergroup" that consists of these country labels
q1 <- geom_text_repel(aes(colour=Country,label=ladder_label),
                      data = laddergroup,size=5, direction="y",xlim = c(2024, NA),
                      hjust=-0.01,segment.linetype = "dotted")

p1 + q1 + expand_limits(x=c(2005,2027)) +
  scale_x_continuous(breaks = c(2005,2008,2011,2014,2017,2020,2023),
                     labels= c("2005","2008","2011","2014","2017","2020","2023")) +
  labs(x = "Year", 
       title = "Life Ladder of each country from 2005 to 2023"
  ) + 
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16)) 

######### Plot for Log GDP per Capita #########

#provides country label to Afganistan to the latest year (2021) that it has data on log GDP per capita in the "ladder_label" column in the "laddergroup" data frame
laddergroup[laddergroup$Country == "Afghanistan" & laddergroup$year == 2021,"ladder_label"] <- "Afghanistan"
#Generates a line plot of the Log GDP per Capita over the years for top and bottom countries in Life Ladder
p3 <- ggplot(data = laddergroup, mapping=aes(x=year,y=`Log GDP Per Capita`,colour=lifeladderscore)) + 
  geom_line(aes(group=Country)) 

#Creates labels for the top and bottom 10 Life Ladder countries in the plot
q3 <- geom_text_repel(aes(colour=lifeladderscore,label=ladder_label),
                      data = laddergroup,size=5, direction="y",xlim = c(2024, NA),
                      hjust=0,segment.linetype = "dotted",show.legend = F)

p3 + q3 + expand_limits(x=c(2005,2027)) +
  scale_x_continuous(breaks = c(2005,2008,2011,2014,2017,2020,2023),
                     labels= c("2005","2008","2011","2014","2017","2020","2023")) +
  labs(x = "Year", 
       title = "Log GDP Capita for countries with the top and bottom 10 mean Life Ladder values", color = "Life Ladder"
  ) + 
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) 

######### Scatterplot for Life Ladder and Positive Affect #########

#Generates a scatter plot of Life Ladder against Log GDP per capita
ggplot(data=happiness, aes(x=`Log GDP Per Capita`,y=`Life Ladder`)) + 
  geom_point(alpha=0.5,show.legend = F) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01,size=6) +
  geom_smooth(colour="red",method="lm",fill=NA) +
  labs(title = "Life Ladder against Log GDP Per Capita") + 
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16))

#Generates a scatter plot of positive affect against Log GDP per capita
ggplot(data=happiness, aes(x=`Log GDP Per Capita`,y=`Positive Affect`)) + 
  geom_point(alpha=0.5,show.legend = F) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01,size=6) +
  geom_smooth(colour="red",method="lm",fill=NA) +
  labs(title = "Positive Affect against Log GDP Per Capita")+
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16))


################################################################################
################################################################################
# Data visualisation on Annual Leave and Unemployment Benefits
################################################################################
################################################################################


######### Preparation for plotting #########

#"workcondgroup" variable is created to be used in labelling the names of the top and bottom countries in mean Life Ladder, in the plot.
#"workcondgroup" consists of both top and bottom countries in Life Ladder from the 'workcond' data frame
workcondgroup <- workcond[workcond$Country %in% unique(laddergroup$Country),]
workcondgroup$Country <- as.factor(workcondgroup$Country)
#mutate function is used below to give country labels for the latest year (2020) that particular country has data on number of days of paid leave under the column "leave_label"
#and to categorise whether the country is top 10 or bottom 10 in Life Ladder under the column "lifeladderscore" in the "workcondgroup" data frame
workcondgroup <- workcondgroup  %>% 
  mutate(leave_label = if_else(year == 2020, Country, NA_character_)) %>%
  mutate(lifeladderscore = if_else(Country %in% unique(laddergroupmax$Country), "Top 10 in Life Ladder","Bottom 10 in Life Ladder"))

#factor function is used here to help to re-order legends in the plot
workcondgroup$lifeladderscore <- factor(workcondgroup$lifeladderscore, levels = c("Top 10 in Life Ladder","Bottom 10 in Life Ladder"))

######### Plot for annual paid leave ##########

#p4 generates a line plot of paid annual leave over the years for top and bottom countries in Life Ladder
#"workcondgroup" data frame consists only of both top 10 and bottom 10 countries in Life Ladder from the 'workcond' data frame
p4 <- ggplot(data = workcondgroup, mapping=aes(x=year,y=`leave_avg`,colour=lifeladderscore)) + 
  geom_line(aes(group = `Country`)) 

#q4 creates labels for the top and bottom countries in mean Life Ladder
#"leave_label" is a column in "workcondgroup" that consists of these country labels
q4 <- geom_text_repel(aes(colour=lifeladderscore,label=leave_label),
                      data = workcondgroup,size=5, direction="y",xlim = c(2021, NA),
                      hjust=0,segment.linetype = "dotted",max.overlaps = 20, show.legend = F)

p4 + q4 + expand_limits(x=c(2004,2026)) +
  scale_x_continuous(breaks = c(2004,2008,2012,2016,2020),
                     labels= c("2004","2008","2012","2016","2020")) +
  labs(x = "Year", y = "Number of days of paid annual leave",
       title = "Average paid annual leave for countries with the top and bottom 10 mean Life Ladder values", color="Life Ladder"
  ) + 
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

######### Preparation for multiple plots on Life Ladder and annual leave ##########

#"laddergroupfiltered" consists of data on life ladder from 2008 to 2020 for the countries with top and bottom 10 Life Ladder from the "happiness" data frame
laddergroupfiltered <- full_join(filter(lifeladderfun(10,"max1"),year %in% c(2008:2020)),filter(lifeladderfun(10,"min1"),year %in% c(2008:2020)))
#mutate function is used below to give country labels for the year 2020 (the latest year to have data on days of paid leave) under the column "ladder_label"
laddergroupfiltered <- laddergroupfiltered  %>% 
  mutate(ladder_label = if_else(year == 2020, Country, NA_character_))
#the following 3 lines serve to provide labels for the countries in "laddergroupfiltered" when plotted.(These countries latest data on Life Ladder are before 2020)
laddergroupfiltered[laddergroupfiltered$Country %in% c("Central African Republic","South Sudan") & laddergroupfiltered$year == 2017,"ladder_label"] <-  c("Central African Republic","South Sudan")
laddergroupfiltered[laddergroupfiltered$Country == "Burundi" & laddergroupfiltered$year == 2018,"ladder_label"] <- "Burundi"
laddergroupfiltered[laddergroupfiltered$Country %in% c("Afghanistan","Comoros","Lesotho","Rwanda","Togo") & laddergroupfiltered$year == 2019,"ladder_label"] <- c("Afghanistan","Comoros","Lesotho","Rwanda","Togo")

######### Plotting on Life Ladder and annual leave ##########

#p5 plots Life Ladder over the years
#"laddergroupfiltered" consists of data on life ladder from 2008 to 2020 for the countries with top and bottom 10 Life Ladder from the "happiness" data frame
p5 <- ggplot(mapping=aes(x=year,y=`Life Ladder`)) + 
  geom_line(aes(colour = `Country`), data = laddergroupfiltered) #+

plot1 <- p5 + xlim(2007,2020) +
  labs(x = "Year", 
       title = "Top 10 and bottom 10 countries in Life Ladder",tag = "A"
  )  + 
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        plot.tag = element_text(size=18)) 

#p6 plots annual leave days over the years
p6 <- ggplot(data = filter(workcondgroup,year %in% c(2008:2020)), mapping=aes(x=year,y=`leave_avg`)) + 
  geom_line(aes(colour = `Country`)) #+ 

plot2 <- p6 + xlim(2007,2020) +
  labs(x = "Year", y = "Number of days of paid annual leave",
       title = "Average paid annual leave for countries with the top and bottom 10 mean Life Ladder values",
       tag = "B"
  ) + 
  theme(plot.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        plot.tag = element_text(size=18))

#combines the Life Ladder graph and Annual Leave plot in the same page
ggarrange(plot1,plot2,ncol = 1, nrow = 2,common.legend = T,legend = "bottom")

######### Scatterplot on Life Ladder and annual leave ##########

#merges working conditions dataset "workcond" with the "happiness" dataset into one data frame
#subset is used on 'happiness' data frame to remove the 'Country' column before joining
join2 <- left_join(workcond,subset(happiness, select = -1),join_by(country.code, year))

#Creates a scatterplot of Life Ladder against average annual leave
ggplot(data=join2, aes(x=leave_avg,y=`Life Ladder`)) + 
  geom_point(alpha=0.5) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size=6,label.x = 60,label.y = 7.5) +
  geom_smooth(colour="red",method="lm",fill=NA) +
  labs(x = "Number of days of paid annual leave", y = "Life Ladder",
       title = "Life Ladder against number of days of paid annual leave"
  ) + 
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16))

######### Table on unemployment protection ##########

#this creates a data frame that only consists of data on the unemployment protection status
#of each of the top 10 and bottom 10 Life Ladder countries
unemply_proc <- subset(filter(workcondgroup,year==2020),select=c("Country","unemployment_protection")) %>%
  arrange(desc(unemployment_protection))
#replaces repeating data in the "unemployment_protection" column with blanks
unemply_proc[c(1:5,7:14,16:20),"unemployment_protection"] <- ""

#creates table using gt package
#unemploy_proc is a data frame that only consists of data on the unemployment protection status of each of the top 10 and bottom 10 Life Ladder countries
unemply_proc %>% 
  gt() %>%
  tab_style(style = cell_borders(sides = c("top","bottom"),
                                 style ="hidden"),
            locations = cells_body(columns=unemployment_protection,
                                   rows = c(2:9,12:19))) %>%
  tab_style(style = cell_borders(sides = "bottom",
                                 style ="solid",
                                 weight = px(1.5)),
            locations = cells_body(columns=c(Country,unemployment_protection),
                                   rows = 10)) %>%
  tab_style(style = list(cell_fill(color="palegreen")),
            locations = cells_body(columns=unemployment_protection,
                                   rows = 1:10)) %>%
  tab_style(style = list(cell_fill(color="lightcoral")),
            locations = cells_body(columns=unemployment_protection,
                                   rows = 11:20)) %>%
  cols_label(unemployment_protection = "Unemployment Protection") %>%
  cols_align(align="center",columns = unemployment_protection)


################################################################################
################################################################################
# Data visualisation on Working Hours
################################################################################
################################################################################


#joins working hours dataset to "happiness" dataset
#subset is used on 'happiness' data frame to remove the 'Country' column before joining
join1 <- left_join(workhours,subset(happiness, select = -1), join_by(country.code, year))

#declares the data, and maps the x and y variables to the graph
r1 <- ggplot(data=join1,aes(x=hours,y=`Life Ladder`))
#creates the points for the scatterplot
r2 <- r1 + geom_point(data=filter(join1,!Country %in% c("Denmark","Finland","Iceland","Norway","Sweden","Singapore","Türkiye","Japan","Korea")),alpha=0.4,size=4) +
  geom_point(data=filter(join1,Country=="Singapore"),aes(colour=Country),alpha=0.7,size=4) +
  geom_point(data=filter(join1,Country %in% c("Denmark","Finland","Iceland","Norway","Sweden")),aes(colour=Country),alpha=0.7,size=4) +
  geom_point(data=filter(join1,Country=="Türkiye"),aes(colour=Country),alpha=0.7,size=4) +
  geom_point(data=filter(join1,Country=="Netherlands"),aes(colour=Country),alpha=0.7,size=4) +
  geom_point(data=filter(join1,Country=="Japan"),aes(colour=Country),alpha=0.7,size=4) +
  geom_point(data=filter(join1,Country=="Korea"),aes(colour=Country),alpha=0.7,size=4) 
#creates arrows and annotations in the graph
r3 <- r2 + geom_segment(aes(x=35, y=5.5, xend=37.5, yend=5.8),
               arrow = arrow(length=unit(.5, 'cm')))+
  annotate("text", x=32.9, y=5.5, label= "Japan, Korea",size=6) +
  geom_segment(aes(x=35, y=5.5, xend=37.5, yend=5.8),
               arrow = arrow(length=unit(.5, 'cm')))+
  geom_segment(aes(x=47, y=7, xend=46, yend=6.7),
               arrow = arrow(length=unit(.5, 'cm'))) +
  annotate("text", x=48.5, y=7.1, label= "Singapore",size=6) +
  geom_segment(aes(x=38, y=7.8, xend=37, yend=7.7),
               arrow = arrow(length=unit(.5, 'cm'))) +
  annotate("text", x=39.8, y=7.8, label= "Scandinavia",size=6) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01,size=6,label.x = 44, label.y = 7.8) +
  geom_smooth(colour="red",method="lm",fill=NA)
#adds and adjusts graph labels 
r3 + labs(x= "Weekly work hours",
       title = "Life Ladder against average weekly work hours")+
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14))


################################################################################
################################################################################
# Animated graph
################################################################################
################################################################################

#gganimate package is used to create animations on ggplot
library(gganimate)
#viridis pacakage is loaded to use the viridis color map
library(viridis)
#creates an animated plot of Life Ladder against log GDP per capita
p7 <- ggplot(data=happiness, aes(x=`Log GDP Per Capita`,y=`Life Ladder`,colour = Country)) + 
  geom_point(alpha=0.7,size=8,show.legend = F) +
  theme(plot.title = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16)) +
  scale_color_viridis_d() +
  #shows the transitions in plot across the years
  transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
anim_save("Figure 9.gif",p7)