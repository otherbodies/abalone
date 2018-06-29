options(scipen=999)

# choice of Reference Frame (RF) for 20 subjects in two tasks
# H0: the choices in both task are indepependant

monthYears = c(rep('room',4),rep('head',9),rep('trunk',7))  # the actual choices made by 20 observers
horseback = c(rep('room',4),rep('head',9),rep('trunk',7))   # put the actual numbers here for the second task

maxSamples = 100000
sameChoices = rep(NA, maxSamples) # to be able to store the results of many simulations
simulnb = 0

while (simulnb<maxSamples)
{
  ###    select the lines below until the next ### and press CTRL RETURN to run it as many times as you want
  simulnb = simulnb+1
  monthYearsVector = monthYears  # WE NEED TO REINITIALIZE FOR EACH SIMULATION (draws without replacements)
  horsebackVector = horseback
  sameRF = rep(F,20)  # the vector where we will check whether the subject chose the same RF twice
  
  for (subject in c(1:20)){
    # RANDOMIZATION resampling :        
    # We are taking randomly, WITHOUT REPLACEMENT, one observed value
    choice1 = floor(runif(1, min = 1, max = length(monthYearsVector)+.99))
    RF1 = monthYearsVector[choice1]
    monthYearsVector = monthYearsVector[-choice1] # REMOVES the value from the vector
    
    # second draw
    choice2 = floor(runif(1, min = 1, length(horsebackVector)+.99))
    RF2 = horsebackVector[choice2]
    horsebackVector = horsebackVector[-choice2]
    
    sameRF[subject] = RF1==RF2
  }
  sameChoices[simulnb] = sum(sameRF==T)
  ###
}

hist(sameChoices)
sameChoices = sameChoices[order(sameChoices)]
sameChoices[round(simulnb*.975)]+1 # the 2-sided 95% limit for random choices

# choice of Reference Frame (RF) for 20 subjects in two tasks
# H0: the choices in both task are indepependant

monthYears = c(rep('room',4),rep('head',9),rep('trunk',7))  # the actual choices made by 20 observers
horseback = c(rep('room',7),rep('head',9),rep('trunk',4))   # put the actual numbers here for the second task

maxSamples = 100000
sameChoices = rep(NA, maxSamples) # to be able to store the results of many simulations
simulnb = 0

while (simulnb<maxSamples)
{
  ###    select the lines below until the next ### and press CTRL RETURN to run it as many times as you want
  simulnb = simulnb+1
  monthYearsVector = monthYears  # WE NEED TO REINITIALIZE FOR EACH SIMULATION (draws without replacements)
  horsebackVector = horseback
  sameRF = rep(F,20)  # the vector where we will check whether the subject chose the same RF twice
  
  for (subject in c(1:20)){
    # RANDOMIZATION resampling :        
    # We are taking randomly, WITHOUT REPLACEMENT, one observed value
    choice1 = floor(runif(1, min = 1, max = length(monthYearsVector)+.99))
    RF1 = monthYearsVector[choice1]
    monthYearsVector = monthYearsVector[-choice1] # REMOVES the value from the vector
    
    # second draw
    choice2 = floor(runif(1, min = 1, length(horsebackVector)+.99))
    RF2 = horsebackVector[choice2]
    horsebackVector = horsebackVector[-choice2]
    
    sameRF[subject] = RF1==RF2
  }
  sameChoices[simulnb] = sum(sameRF==T)
  ###
}

hist(sameChoices)
sameChoices = sameChoices[order(sameChoices)]
sameChoices[round(simulnb*.975)]+1 # the 2-sided 95% limit for random choices
