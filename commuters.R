library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(choroplethr)
library(readr)
require(scales)

keepers <- c("ST", "AGEP", "CIT", "COW", "JWMNP", "JWRIP", "JWTR", "SCHL", "SEX", "WAGP", "PERNP")
census_A <- fread("pums/ss13pusa.csv", select=keepers )  
census_B <- fread("pums/ss13pusb.csv", select=keepers )
census_comp <- rbind(census_A, census_B)

#state codes
states_st<-c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,                       
                            39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56,72)
states_id<-c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware',
             'District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas',
             'Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi',
              'Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina',
              'North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee',
              'Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming','Puerto Rico')
states<-as.data.frame(cbind(as.numeric(states_st), states_id))
names(states)<-c("ST", "id")
#coerce states from factor to numeric so we can merge the census data with state names
states$ST<-as.numeric(levels(states$ST))[states$ST]

#get state names into my census data
census_comp<-merge(census_comp, states, all=FALSE, by=c("ST"))

#I'm interested in commuters, so let's get rid of NAs on JWMNP
census_comp<-filter(census_comp, JWMNP !="NA")

#Replace calues of the SEX feature with M and F
translator_vector <- c("1"='M',"2"="F")
census_comp$SEX<-translator_vector[census_comp$SEX]

head(census_comp)

#look at commute times by sex
ggplot(census_comp, aes(x=SEX, y=JWMNP)) + geom_boxplot()
aggregate(census_comp$JWMNP, by=list(census_comp$SEX), FUN=mean, na.rm=TRUE)

#women typically commute the same distance to earn way less money
ggplot(census_comp, aes(x=SEX, y=WAGP)) + geom_boxplot() + scale_y_continuous(labels = comma)
aggregate(census_comp$JWMNP, by=list(census_comp$SEX), FUN=mean, na.rm=TRUE)

# Take the mean of JWMNP by State
agg_state <-aggregate(census_comp$JWMNP, by=list(census_comp$id), FUN=mean, na.rm=TRUE)
names(agg_state)<-c("region", "value")
agg_state<-subset(agg_state, region!="Puerto Rico" & region!="Alaska")
#set regions to lcase for use with choroplethr package
agg_state$region<-tolower(agg_state$region)


# MAP
map <- state_choropleth(agg_state,
                        title      = "Avg commute times by state",
                        legend     = "Avg commute (mins)",
                        num_colors = 1)

ggsave("commute_times.png", map, height=8, width=12, units="in")

worst<-head(arrange(agg_state, desc(value)),1)
best<- head(arrange(agg_state, value),1)
worst
best
