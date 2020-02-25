mult = function(n,m){
  val = m/sqrt((n+1)*(n))
  return(val)
}

states_org = read.csv('statepops2010.csv',header=F,stringsAsFactors = F)
states_org = states[1:50,1:2]
states_org = cbind(states,rep(1,50))
names(states_org) = c('state','population','seats')

popjump = function(jump,statenum){

  states = states_org
  
  row.names(states) = seq(1,50)
  
  population = as.numeric(states$population)
  
  for (i in 1:385){
    states[i+3] = as.numeric(mult(i,population))
  }
  
  statlist = unlist(states[4:388])
  
  for (i in 1:385){
    orgind = which.max(statlist)
    ind = orgind %% 50
    states[ind,'seats'] = states[ind,'seats'] + 1
    statlist[orgind] = 0
  }
  
  original = states[statenum,'seats']
  
  states = states_org
  
  row.names(states) = seq(1,50)
  
  states[statenum,'population'] = states[statenum,'population'] + jump
  
  population = as.numeric(states$population)
  
  for (i in 1:385){
    states[i+3] = as.numeric(mult(i,population))
  }
  
  statlist = unlist(states[4:388])
  
  for (i in 1:385){
    orgind = which.max(statlist)
    ind = orgind %% 50
    states[ind,'seats'] = states[ind,'seats'] + 1
    statlist[orgind] = 0
  }
  
  new = states[statenum,'seats']
  diff = new - original
  
  return(diff)
}


states = read.csv('statepops2010.csv',header=F,stringsAsFactors = F)
states = states[1:50,1:2]
states = cbind(states,rep(1,50))
names(states) = c('state','population','seats')

totals = data.frame(states['state'])

intervals = seq(-1000000,1000000,10000)

for (chg in intervals){
changes = vector()
for (statenum in 1:50){
  changes = c(changes,popjump(chg,statenum))
  print(paste0('state # :',statenum,'... add/drop:',chg))
}
totals = cbind(totals,changes)
}

write.csv(totals,file='seat_loss_gain.csv')