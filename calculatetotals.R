totals = read.csv('seat_loss_gain.csv')
totals = totals[,-1]
rownames(totals) = totals$NA.
intervals = seq(-1000000,1000000,10000)
names = intervals
names(totals) = names

totals

positives = totals[,c(102:201)]
negatives = totals[,c(1:100)]

library(dplyr)

occurence = function(row,val){
  occurs = which(row %in% val)
  if (val>0){
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
states = read.csv('statepops2010.csv',header=F)[c(1:50),1]
populations = states = read.csv('statepops2010.csv',header=F)[c(1:50),2]

seatlosses = data.frame(states,losetwoseats,loseoneseat,gainoneseat,gaintwoseats)
colnames(seatlosses) = c('State','Lose 2 Seats','Lose 1 Seat','Gain 1 Seat','Gain 2 Seats')

seatlosses_per = seatlosses
seatlosses_per$`Lose 2 Seats` = round((100*seatlosses_per$`Lose 2 Seats`/populations),1)
seatlosses_per$`Lose 1 Seat` = round((100*seatlosses_per$`Lose 1 Seat`/populations),1)
seatlosses_per$`Gain 1 Seat` = round((100*seatlosses_per$`Gain 1 Seat`/populations),1)
seatlosses_per$`Gain 2 Seats` = round((100*seatlosses_per$`Gain 2 Seats`/populations),1)

write.csv(seatlosses,'seatlosses.csv')
write.csv(seatlosses_per,'seatlosses_per.csv')

seatlosses_withpop = cbind(seatlosses,populations)
write.csv(seatlosses_withpop,'seatlosses_withpop.csv')

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

ggsave('gainsandlosses.jpg',seats,height=10,width=20)

totals_new = cbind(states,totals)

totals_tidy = gather(totals_new,key='key',val='val',-state)

totals_tidy$val = factor(totals_tidy$val,levels=c(2,1,0,-1,-2))
totals_tidy$key = as.double(totals_tidy$key)

swings = ggplot(totals_tidy,aes(key,fct_rev(states))) + 
  geom_raster(aes(fill=val)) +
  scale_fill_manual(values=c('darkgreen','green','white','blue','darkblue')) +
  scale_x_continuous(breaks=seq(-1000000,1000000,250000)) +
  geom_vline(xintercept=0) +
  theme(axis.text=element_text(size=100)) +
  theme_calc()

ggsave('state_swings.jpg',swings,height=30,width=20)
