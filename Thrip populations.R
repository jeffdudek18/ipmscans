###installs and loads the packages you'll use
#install.packages("dplyr")
#install.packages("ggplot2")
library("dplyr")
library("ggplot2")

###reads the .csv file and filters out racks 1-12
thrips <- read.csv("insect scanner data - MFE weekly data (1).csv")
racks13_16 <-filter(thrips, as.numeric(rack) > 12)


### groups the racks 13-16 data by week and then summarizes into means and variation characteristics
mfe.weekly.thrips<- racks13_16 %>%
  group_by(date) %>%
  summarise(
    weekly.thrips.card.week = mean(as.numeric(thrips.card.week)),
    weekly.thrips.card = mean(as.numeric(thrips.card)),
    weekly.total = sum(total.thrips),
    sd.weekly.thrips.card.week = sd(as.numeric(thrips.card.week)),
    sd.weekly.thrips.card = sd(as.numeric(thrips.card)),
    sd.weekly.total = sd(as.numeric(total.thrips)),
    n = n(),
    se.weekly.thrips.card.week = sd.weekly.thrips.card.week/sqrt(n),
    se.weekly.thrips.card = sd.weekly.thrips.card/sqrt(n),
    se.weekly.total = sd.weekly.total/sqrt(n))


### turns the date into a date object with the format (month/day)
mfe.weekly.thrips$date <- as.Date(mfe.weekly.thrips$date, "%m/%d")

### generates the thrips/card/week plot and stores it as an object 
thrips.per.card.per.week <- ggplot(data=mfe.weekly.thrips, aes(x=date, y=weekly.thrips.card.week))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=weekly.thrips.card.week - se.weekly.thrips.card.week,
                    ymax=weekly.thrips.card.week + se.weekly.thrips.card.week),
                width=.4)+
  ylab("Thrips Per Card Per Week")+
  scale_x_date(breaks = "2 weeks", date_labels = "%m/%d")+
  xlab("Date")

### generates the thrips/card plot and stores it as an object 
thrips.per.card <- ggplot(data=mfe.weekly.thrips, aes(x=date, y=weekly.thrips.card))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=weekly.thrips.card - se.weekly.thrips.card,
                    ymax=weekly.thrips.card + se.weekly.thrips.card),
                width=.4)+
  ylab("Thrips Per Card")+
  scale_x_date(breaks = "2 weeks", date_labels = "%m/%d")+
  xlab("Date")

### generates the total thrips plot and stores it as an object 
all.thrips <- ggplot(data=mfe.weekly.thrips, aes(x=date, y=weekly.total))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=weekly.total - weekly.total,
                    ymax=weekly.total + weekly.total),
                width=.4)+
  ylab("Total Thrips")+
  scale_x_date(breaks = "2 weeks", date_labels = "%m/%d")+
  xlab("Date")


## calls the objects which generates your graphs 
all.thrips
thrips.per.card
thrips.per.card.per.week

