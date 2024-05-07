lambda = 2 #message arrival rate

weather <- function(arrival_time){#Function returning 1 for good weather, 0 for bad weather
  weather_coeff = floor(arrival_time) %% 3
  if(weather_coeff == 0){
    return(0)
  } else{return(1)}
} #1 for good weather, 0 for bad


Service_Good <- function(arrival_time){
  return(runif(1))
}#service time under good weather

Service_Bad <- function(arrival_time){
  return(rexp(1, 1/2))
}#service time under bad weather

for(k in 1:200){
  #Re-initialize parameters before each iteration:
  T = 0
  stop_time = 100 #stopping time
  arrivals = rexp(500, rate = lambda) #generating many arrival times to ensure T = 100 is reached, will be used to scale T after each iteration
  S = cumsum(arrivals) 
  S = S[S<100] #Arrival times before T = 100
  
  messages_processed = 0 #Initializing X
  index = 1 #Indexing to be used in while loop
  
  t1 = 0 #channel 1 processing time
  t2 = 0 #channel 2 processing time
  
  while(T < stop_time){
    
    if(S[index] > t1 & S[index] > t2){ #Both Channels free at time of arriva, message goes to channel 2
      current_weather = weather(S[index]) #determine current weather
      if(current_weather == 0){
        t2 = max(t2, S[index]) + Service_Bad(S[index])
      }
      else{t2 = max(t2, S[index]) + Service_Good(S[index])}
    }else if(S[index] > t1 & S[index] < t2){#Channel 1 free at time of arrival
      current_weather = weather(S[index])
      if(current_weather == 0){
        t1 = max(t1, S[index]) + Service_Bad(S[index])
      }
      else{t1 = max(t1, S[index]) + Service_Good(S[index])}
    }else if(S[index] < t1 & S[index] > t2){ #Channel 2 free at arrival time
      current_weather = weather(S[index]) #determine current weather
      if(current_weather == 0){
        t2 = max(t2, S[index]) + Service_Bad(S[index])
      }else{t2 = max(t2, S[index]) + Service_Good(S[index])}
    }else if(S[index] < t1 & S[index] < t2){#Both channels busy, message lost
      messages_processed = messages_processed - 1 #Message lost, subtracting 1 from total because after every iteration of the 
    }                                             #while loop, messages processed gets increased by 1
    
    T  = T + arrivals[index] #Increasing T by inter-arrival time
    index = index + 1 #Update index for next iteration
    if(t1 >100 & t2 > 100){ #Before increasing messages processed counter, need to make sure none finish processing after T = 100
      break
    }
    
    messages_processed = messages_processed + 1 #X increases by 1 if previous check is False
    
    if(t1 > 100){ #Taking away a processed message by channel 1 if it finishes after T = 100 but t2 and T are still less than 100
      messages_processed = messages_processed - 1
    }
    if(t2 > 100){ #Take away message processed by channel 2 if it finishes after T=100, t1 and T could still be < 100
      messages_processed = messages_processed - 1
      
    }
    if(index > length(S)){ #Index on last iteration will be 1 more than the length of the vector S (arrivals before T=100)
      #This condition helps avoid getting that error
      break
    }
    
  }
  messages_processed_per_simulation[k] = messages_processed #Add X to vector of results from each simmulation
}
X = mean(messages_processed_per_simulation)
#Estimate for X after 200 simulations:
X

