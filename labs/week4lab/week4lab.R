states <- data.frame(state.x77, state.region, state.abb)

fourstates <- c("Colorado", "New Mexico", "Utah", "Arizona")

rownames(states[states$Life.Exp == min(states[fourstates,]$Life.Exp),])
rownames(states[states$Population == max(states[fourstates,]$Population),])

# frost above 100, and murder above 10, illeteracy less than 1
for (i in 1:length(states$Population)) { 
  if (states[,i])
}

# states[which(states$Frost > 100 & states$Murder > 10 & states$Illiteracy < 1),]
region_vec <- unique( states$state.region ) # create vector of unique regions in our data

region_sum <- matrix( nrow=length(region_vec), ncol=2, dimnames=list(region_vec, c('mean','sd')))
