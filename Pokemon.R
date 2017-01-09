#ABOUT DATA
#This data set includes 721 Pokemon, including their number, name, first and second type, and basic stats: HP, Attack, Defense, Special Attack, Special Defense, and Speed. It has been of great use when teaching statistics to kids.
#With certain types you can also give a geeky introduction to machine learning.

#load libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Hmisc)
library(corrplot)

#read file
pokedat <- read.csv(".../input/pokemon.csv", header = T)

#structure of data
str(pokedat)
describe(pokedat)
head(pokedat)
tail(pokedat)

#Basic plots of data
ggplot(pokedat, aes(pokedat$Type.1)) + geom_bar(aes(fill = as.factor(pokedat$Type.1))) + 
  scale_fill_discrete(name="Pokemon Types",
                      labels=c( "Bug","Dark","Dragon","Electric","Fairy","Fighting","Fire","Flying","Ghost","Grass","Ground",
                                "Ice","Normal","Poison","Psychic","Rock","Steel","Water")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Type 1",y= "Frequency" , title = "Pokemon Types")

ggplot(pokedat, aes(pokedat$Type.2)) + geom_bar(aes(fill = as.factor(pokedat$Type.2))) + 
  scale_fill_discrete(name="Pokemons of Double Type",
                      labels=c("Doesn't Have","Bug","Dark","Dragon","Electric","Fairy","Fighting","Fire","Flying","Ghost","Grass","Ground",
                                "Ice","Normal","Poison","Psychic","Rock","Steel","Water")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Type 1",y= "Frequency" , title = "Pokemons of Double Type")

ggplot(pokedat, aes(pokedat$Total)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Total",y= "Frequency" , title = "Plot of Total Pokemon Score")

ggplot(pokedat, aes(pokedat$HP)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Health",y= "Frequency" , title = "Plot of Pokemon Health")

ggplot(pokedat, aes(pokedat$Attack)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Attack",y= "Frequency" , title = "Plot of Pokemon Attack Rating")  

ggplot(pokedat, aes(pokedat$Defense)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Defense",y= "Frequency" , title = "Plot of Pokemon Defense Rating")

ggplot(pokedat, aes(pokedat$Sp..Atk)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Special Attack",y= "Frequency" , title = "Plot of Pokemon Special Attack Rating")

ggplot(pokedat, aes(pokedat$Sp..Def)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Special Defense",y= "Frequency" , title = "Plot of Pokemon Special Defense Rating")   

ggplot(pokedat, aes(pokedat$Speed)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Speed",y= "Frequency" , title = "Plot of Pokemon Speed")

#10 Best Pokemons in each category.
#Higher Total - 10 pokemons with Higher Total Score
head(pokedat[order(pokedat$Total,decreasing = TRUE), ], n = 10)
#Higher Health
head(pokedat[order(pokedat$HP,decreasing = TRUE), ], n = 10)
#Higher Attack
head(pokedat[order(pokedat$Attack,decreasing = TRUE), ], n = 10)
#Higher Defense
head(pokedat[order(pokedat$Defense,decreasing = TRUE), ], n = 10)
#Higher Special Attack
head(pokedat[order(pokedat$Sp..Atk,decreasing = TRUE), ], n = 10)
#Higher Special Defense
head(pokedat[order(pokedat$Sp..Def,decreasing = TRUE), ], n = 10)
#Faster
head(pokedat[order(pokedat$Speed ,decreasing = TRUE), ], n = 10)


#Checking correlation between variables.
#Correlation Matrix
cormat <- cor(pokedat[5:11])
cormat

#Plotting Correlations
corrplot(cormat, type="upper", order="hclust", tl.col="black", tl.srt=45)

#Plot Total ~ Attack
qplot(Total, Attack, data = pokedat) + stat_smooth(method = "lm", se = FALSE , colour = "red" ) +
  labs(x= "Total",y= "Attack" , title = "Total ~ Attack")  
#Plot Total ~ Sp.Attack
qplot(Total, Sp..Atk, data = pokedat) + stat_smooth(method = "lm", se = FALSE , colour = "red" ) +
  labs(x= "Total",y= "Sp..Attack" , title = "Total ~ Sp..Atk")  
#Plot Total ~ Sp.Def
qplot(Total, Sp..Def, data = pokedat) + stat_smooth(method = "lm", se = FALSE , colour = "red" ) +
  labs(x= "Total",y= "Sp..Def" , title = "Total ~ Sp..Def")  





#Function to compare pokemons

pcomp=function(x,y){
  cc=apply(rbind(pokedat[x,c(5:11)],pokedat[y,c(5:11)]) , 2 , diff )
  cc[which(cc>0)]=paste(y ,'better than', x)
  cc[which(cc<0)]=paste(y ,'worse', x)
  cc[which(cc==0)]=paste(y ,'equal', x)
  return(cc)}

