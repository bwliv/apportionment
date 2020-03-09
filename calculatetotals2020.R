totals = read.csv('seat_loss_gain_2018.csv')
totals = totals[,-1]
rownames(totals) = totals$state
totals = totals[,-1]
intervals = seq(-1000000,1000000,10000)
names = intervals
names(totals) = names

positives = totals[,c(101:201)]
negatives = totals[,c(1:100)]

library(dplyr)

occurence = function(row,val){
  occurs = which(row %in% val)
  if (val>=0){
    return(occurs[1])
  } else if (val<0){
    if(length(occurs)>0){
    return(occurs[length(occurs)])
    } else{
      return(NA)
    }
  }
}

transneg = function(val){
  return(-(1000000-(10000*(val-1))))
}

gainoneseat = 10000*apply(positives,1,occurence,1)
gaintwoseats = 10000*apply(positives,1,occurence,2)
loseoneseat = transneg(apply(negatives,1,occurence,-1))
losetwoseats = transneg(apply(negatives,1,occurence,-2))
states = read.csv('2018ACS.csv',header=F)[c(1:50),1]

seatlosses = data.frame(states,losetwoseats,loseoneseat,gainoneseat,gaintwoseats)
colnames(seatlosses) = c('State','Lose 2 Seats','Lose 1 Seat','Gain 1 Seat','Gain 2 Seats')

colnames(seatlosses) = c('State','-2','-1','+1','+2')

library(tidyr)
seats_tidy = gather(seatlosses,key='key',value='value',-state)
seats_tidy$value = seats_tidy$value/1000
seats_tidy$key = factor(seats_tidy$key,levels=c('-2','-1','+1','+2'))

library(ggplot2)
library(forcats)
library(ggthemes)
seats = ggplot(seats_tidy) + 
  geom_col(aes(x=key,y=value)) + 
  xlab('') +
  ylab('') +
  #labs(subtitle = 'How Population Changes Would Have Gained/Lost States House Seats In 2010') +
  facet_wrap(~State,ncol=10) + 
  theme_calc()

ggsave('gainsandlosses2018.jpg',seats,height=10,width=20)

totals_new = cbind(states,totals)

totals_tidy = gather(totals_new,key='key',val='val',-state)

totals_tidy$val = factor(totals_tidy$val,levels=c(2,1,0,-1,-2))
totals_tidy$key = as.double(totals_tidy$key)

# removing gains/losses of two seats to clean up graph

graphing_tidy = totals_tidy

graphing_tidy$val[graphing_tidy$val == 2] = 1
graphing_tidy$val[graphing_tidy$val == -2] = -1
graphing_tidy$val[graphing_tidy$val == 0] = NA

graphing_tidy = graphing_tidy %>%
  filter(!is.na(val))

graphing_tidy$val = factor(graphing_tidy$val,levels=c(-1,1))
graphing_tidy$val = fct_recode(graphing_tidy$val,`Lose Seat(s)` = '-1',
                             `Gain Seat(s)` = '1')
graphing_tidy$val[graphing_tidy$val == -1] = 'Lose Seat(s)'
graphing_tidy$val[graphing_tidy$val == 1] = 'Gain Seat(s)'

state_swings = ggplot(graphing_tidy,aes(key,fct_rev(states))) + 
  geom_tile(aes(fill=val)) +
  scale_fill_manual(values=c('red','darkgreen')) +
  scale_x_continuous(breaks=seq(-1000000,1000000,500000),
                     labels = scales::comma) +
  geom_vline(xintercept=0,color='black') +
  xlab('Swing Needed From 2010 Population To Add/Drop Seat(s)') +
  ggtitle('How Close States Are To Gaining/Losing Congressional Seat In 2020') +
  labs(subtitle='Based on 2018 ACS Population Estimates') +
  theme(axis.ticks.y= element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size=40),
        axis.text.y = element_text(size=40),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=55),
        plot.title = element_text(size=55,hjust=0.5),
        plot.subtitle = element_text(size=50,hjust=0.5),
        legend.position = 'bottom',
        legend.text = element_text(size=40),
        legend.title = element_blank(),
        legend.key.size = unit(0.7,'in'),
        legend.spacing.x = unit(0.5, 'in')) 

ggsave('state_swings2018.jpg',state_swings,height=45,width=30)


# Adjusting seat losses to remove lose/gain 2 seats columns 

seatlosses = seatlosses[,c(1,3,4)]
colnames(seatlosses) = c('state','Lose 1 Seat','Gain 1 Seat')
seatlosses_toprint = seatlosses
seatlosses_toprint = format(seatlosses_toprint,scientific=FALSE,big.mark = ",", big.interval = 3)
write.csv(seatlosses_toprint,'seatlosses2018.csv')

# Moving to percentage of population needed for seat losses 

populations = read.csv('2018ACS.csv',header=F)[c(1:50),2]

seatlosses_per = seatlosses
seatlosses_per$`Lose 1 Seat` = round((100*seatlosses_per$`Lose 1 Seat`/populations),1)
seatlosses_per$`Gain 1 Seat` = round((100*seatlosses_per$`Gain 1 Seat`/populations),1)

write.csv(seatlosses_per,'seatlosses_per2018.csv')

ucr = seatlosses
ucr$`Lose 1 Seat` = round((100*ucr$`Lose 1 Seat`/populations),1)
ucr$`Gain 1 Seat` = round((100*ucr$`Gain 1 Seat`/populations),1)

undercount = read.csv('undercount.csv',sep='',header=F)
undercount = undercount[,7:8]
colnames(undercount) = c('uc','RMSE')

ucr = cbind(ucr,undercount)

ucr$miss_gain_raw = ucr$uc > ucr$`Gain 1 Seat`
ucr$avoided_loss = ucr$uc < ucr$`Lose 1 Seat`

ucr_filtered_raw = ucr

ucr_filtered_raw$`Lose 1 Seat` = ucr_filtered_raw$`Lose 1 Seat` * ucr_filtered_raw$avoided_loss
ucr_filtered_raw$`Gain 1 Seat` = ucr_filtered_raw$`Gain 1 Seat` * ucr_filtered_raw$miss_gain_raw
ucr_filtered_raw = ucr_filtered_raw[ucr_filtered_raw$`Lose 1 Seat` < 0 | ucr_filtered_raw$`Gain 1 Seat` > 0,]
ucr_filtered_raw = ucr_filtered_raw[!is.na(ucr_filtered_raw$uc),]
ucr_filtered_raw = ucr_filtered_raw[,2:4]
ucr_filtered_raw$`Lose 1 Seat` = ifelse(ucr_filtered_raw$`Lose 1 Seat` == 0, '--',ucr_filtered_raw$`Lose 1 Seat`)
ucr_filtered_raw$`Gain 1 Seat` = ifelse(ucr_filtered_raw$`Gain 1 Seat` == 0, '--',ucr_filtered_raw$`Gain 1 Seat`)

ucr = seatlosses
ucr$`Lose 1 Seat` = round((100*ucr$`Lose 1 Seat`/populations),1)
ucr$`Gain 1 Seat` = round((100*ucr$`Gain 1 Seat`/populations),1)

ME = pnorm(1) - .5

undercount = read.csv('undercount.csv',sep='',header=F)
undercount = undercount[,7:8]
colnames(undercount) = c('uc','RMSE')
undercount = undercount %>% mutate(
  lower_uc = round(uc - qnorm(.5 + ME)*RMSE,2),
  upper_uc = round(uc + qnorm(.5 + ME)*RMSE,2)
)

ucr = cbind(ucr,undercount)

ucr$miss_gain_se = ucr$upper_uc > ucr$`Gain 1 Seat`
ucr$avoided_se = ucr$lower_uc < ucr$`Lose 1 Seat`

ucr_filtered_se = ucr
ucr_filtered_se$`Lose 1 Seat` = ucr_filtered_se$`Lose 1 Seat` * ucr_filtered_se$avoided_se
ucr_filtered_se$`Gain 1 Seat` = ucr_filtered_se$`Gain 1 Seat` * ucr_filtered_se$miss_gain_se
ucr_filtered_se = ucr_filtered_se[ucr_filtered_se$`Lose 1 Seat` <0 | ucr_filtered_se$`Gain 1 Seat` > 0,]
ucr_filtered_se = ucr_filtered_se[!is.na(ucr_filtered_se$uc),]
ucr_filtered_se = ucr_filtered_se[,c(2:7)]
ucr_filtered_se$`Lose 1 Seat` = ifelse(ucr_filtered_se$`Lose 1 Seat` == 0, '--',ucr_filtered_se$`Lose 1 Seat`)
ucr_filtered_se$`Gain 1 Seat` = ifelse(ucr_filtered_se$`Gain 1 Seat` == 0, '--',ucr_filtered_se$`Gain 1 Seat`)

write.csv(ucr_filtered_raw,'state_pers_within_uc2018.csv')

write.csv(ucr_filtered_se,'state_pers_within_uc_se2018.csv')

