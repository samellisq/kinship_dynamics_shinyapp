# to save repetition this is a simple funciton used to generate a random id for out group fathers
sample_random_father = function(n){
  out = sample(1000000000:1000000000000, n)
  return(out)
}

#Adult mortality. Each adult survives a given year with a fixed probability (J & C 2010).
mortality = function(current.living.table, M.adult.mortality, F.adult.mortality, M.adult.Ex, F.adult.Ex){
  current.living.table$death.variable = runif(nrow(current.living.table))
  current.living.table$mortalityrisk= ifelse(current.living.table$sex =="M", M.adult.mortality, F.adult.mortality)
  current.living.table$maxage = ifelse(current.living.table$sex == "M", round(M.adult.Ex*2.39), round(F.adult.Ex*2.39)) # 2.39 comes from comparing average and max age (true max, ratio with age Z = 1.83) from 52 mammal species see 'Mammals PrR Analysis Average vs Max age.R'
  survivors = select(filter(current.living.table, death.variable > mortalityrisk & age <= maxage), id, age, sex, mother, father)
  return(survivors)
}


# Replacement combines 'reproduction', 'dispersal' and 'competition' from J & C 2010. 
# spaces in the group are filled by offspring from i - age_at_maturity ago or dispersers.
replacement = function(living.list, post.mortality.population.df, group.size, m.dispersal.prob, f.dispersal.prob, age.at.maturity, local.mating.prob, fixed.sex.ratio = "N"){
  maxid = max(living.list[[length(living.list)]]$id)
  
  possible.parents = living.list[[length(living.list) - age.at.maturity]]
  # if(nrow(filter(possible.parents, sex =="M"))==0 ){stop("Population ran out of males") }
  # if(nrow(filter(possible.parents, sex =="F"))==0) { stop ("Population ran of females")}
  if(nrow(filter(possible.parents, sex =="M"))==0 ){print("Note: population ran out of males"); return(data.frame() )} # if it runs out of potential parents no replacemnts are returned. They will be made up at the next time point (because currently living < group size)
  if(nrow(filter(possible.parents, sex =="F"))==0) {print("Note: Population ran of females");return( data.frame())} # if it runs out of potential parents no replacemnts are returned. They will be made up at the next time point (because currently living < group size)
  
  spaces = group.size - nrow(post.mortality.population.df)
  
  if (spaces == 0) {return (data.frame())}
  
  if (fixed.sex.ratio == "N"){
    sexes = sample(c("M","F"),spaces, replace = TRUE)
  } else {
    groupsize = nrow(post.mortality.population.df) + spaces
    f.spaces = ceiling(groupsize/2) - nrow(filter(post.mortality.population.df, sex == "F")) # 'Ceiling vs floor' means that in  case of odd numbers there will be one more female tham males.
    m.spaces = floor(groupsize/2) - nrow(filter(post.mortality.population.df, sex == "M")) # 'Ceiling vs floor' means that in  case of odd numbers there will be one more female tham males
    sexes = c(rep.int("F", f.spaces), rep.int("M", m.spaces))
    sexes = sample(sexes,length(sexes)) # randomise the order just in case anything works on order in the future
  }
  
  replacements = list()
  for(i in 1:spaces){
    
    if(sexes[i] == "F"){
      replacement.type = ifelse (runif(1) > (1- f.dispersal.prob), "immigrant", "philopatric")
      if(replacement.type == "immigrant"){
        replacements [[i]] = data.frame(id = maxid + i, 
                                        age = age.at.maturity,
                                        sex = "F",
                                        mother = NA, # NA represents unknown extra group parent
                                        father = NA  # NA represents unknown extra group parent
                                        )
      } else { # when philopatric
        mating = ifelse(runif(1) < local.mating.prob, "local","outgroup")
        possile.mothers = filter(possible.parents, sex == "F")$id
        possible.fathers = filter(possible.parents, sex == "M")$id
        replacements [[i]] = data.frame(id = maxid + i,
                                        age = age.at.maturity,
                                        sex = "F",
                                        mother = ifelse(length(possile.mothers) > 1,
                                                        sample(possile.mothers, 1),
                                                        possile.mothers), # solves a problem that can't sample from a vector of one
                                        father = ifelse(mating == "local",
                                                        ifelse(length(possible.fathers) > 1,
                                                               sample(filter(possible.parents, sex == "M")$id, 1),
                                                               possible.fathers),
                                                        sample_random_father(1) ) # this is not a very elegant solution. Every father needs and id for kinship to work. This should mean they are unique. Checked for later.
                                        )
      }
    } else { # if sex is male 
      replacement.type = ifelse (runif(1) > (1- m.dispersal.prob), "immigrant", "philopatric")
      if(replacement.type == "immigrant"){
        replacements [[i]] = data.frame(id = maxid + i, 
                                        age = age.at.maturity,
                                        sex = "M",
                                        mother = NA, # NA represents unknown extra group parent
                                        father = NA  # NA represents unknown extra group parent
        )
      } else { # when philopatric
        mating = ifelse(runif(1) < local.mating.prob, "local","outgroup")
        possile.mothers = filter(possible.parents, sex == "F")$id
        possible.fathers = filter(possible.parents, sex == "M")$id
        replacements [[i]] = data.frame(id = maxid + i,
                                        age = age.at.maturity,
                                        sex = "M",
                                        mother = ifelse(length(possile.mothers) > 1,
                                                        sample(possile.mothers, 1),
                                                        possile.mothers),
                                        father = ifelse(mating == "local",
                                                        ifelse(length(possible.fathers) > 1,
                                                               sample(filter(possible.parents, sex == "M")$id, 1),
                                                               possible.fathers),
                                                        sample_random_father(1) ) # this is not a very elegant solution. Every father needs and id for kinship to work. This should mean they are unique. Checked for later.
        )
      }
      
    } 
    
  } # end of spaces loop
  
  replacements = do.call(rbind,replacements)
  return(replacements)

}

#Auxillary function to choses sexes during re
sex_chooser = function(post.mortality.dataftame, n.spaces, fixed.sex.ratio = "N"){ # at the moment it simply maintains a 50 50 ratio when run. COuld be cganged

  return(sexes)
}



# Again to save repitition get a random extra big id for the damned
sample_random_damned_id = function(n){
  out = sample(1000000000001:10000000000000, n)
  return(out)
}

create_juveniles = function(living.list, juvenile.group.size, age.at.maturity, local.mating.rate, show.damned.age.plot = FALSE){
  
  # Phase 1: Add the ELECT (those who join the population as adults, or their replacement)
  
  R= length (living.list)
  JUVENILES = rep(list(data.frame()), R) #makes a list of R empty lists
  for(i in R:(age.at.maturity+1)){
    
    living = living.list[[i]]
    new.adults = filter(living, age == age.at.maturity+1)
    
    if(nrow(new.adults) > 0){
      
      new.philos = filter(new.adults, !is.na(mother))
      new.immis = filter(new.adults, is.na(mother))
      
      if(nrow(new.immis)>0){
        
        new.immis$id = sample_random_damned_id(nrow(new.immis)) #This is seperate sample from the random fathers
        possible.mothers = filter(living.list[[i-(age.at.maturity)]], sex == "F")$id
        possible.fathers = filter(living.list[[i-(age.at.maturity)]], sex == "M")$id
        
        new.immis$mother = sample(possible.mothers, nrow(new.immis),replace = TRUE)
        extrapair.randoms = runif(nrow(new.immis))
        father.in = ifelse(extrapair.randoms > local.mating.rate, "Y", "N")
        # new.immis$father = ifelse(father.in == "Y",
        #                           sample(possible.fathers,1),
        #                           sample(1000000000:1000000000000, 1))
        new.immis$father.in = father.in
        for(l in 1:nrow(new.immis)){
          if(new.immis$father.in[l] == "N"){
            new.immis$father[l] = sample_random_father(1)
          } else {
            new.immis$father[l] = sample(possible.fathers,1)
          }
        }
        new.immis = select(new.immis, -(father.in))
        
        
      }
       
      new.adults = rbind(new.philos, new.immis)
      
      for(j in 1:(age.at.maturity)){
        k = i -j
        juves = new.adults
        juves$age = (juves$age -1) - j
        juves$calvin_type = "elect"
        juves = bind_rows(JUVENILES[[k]], juves)
        JUVENILES[[k]] = juves
      }
    }
    
  }
    
    
    # Phase 2: The DAMNED # That is those who do not survive to adulthood 
  
  #a couple of calculations needed 
  # average.n.recruits.peryear = floor(mean(unlist(lapply(JUVENILES, nrow))))
  # average.n.elect = average.n.recruits.peryear * age.at.maturity # because this is (on average) how many elect individuals are juveniles at any one time
  # average.n.damned = juvenile.group.size - average.n.elect
  # rate.of.loss = average.n.damned / age.at.maturity
  damned.mortality.rate =  1 / age.at.maturity  

  


    #Seed the first year of juveniles
  n.missing = juvenile.group.size - nrow(JUVENILES[[1]])
  if(n.missing < 1) {n.missing = 1} #There must be at leased one damned even if this means going about n.juveniles
  damned.juves = data.frame(id = sample_random_damned_id(n.missing),
                            age = 0,
                            sex = sample(c("M","F"),n.missing, replace = TRUE),
                            mother = NA,
                            father = NA,
                            calvin_type = "damned"
                            )
  JUVENILES[[1]] = rbind(JUVENILES[[1]], damned.juves)
  
  overspill_count = 0
  for(i in 2:R){ # forwards this time

    if(nrow(JUVENILES[[i]]) > juvenile.group.size){
      # messege= paste("Number of recruits in year",
      #                i,
      #                "is greater than the specified number of juveniles in a group.",
      #                "Is the number of Juveniles too small?",
      #                "(note: code will keep running with a temporarily",
      #                "articially inflated juvenile group size)",
      #                sep = " ")
      # print(messege)
      overspill_count = overspill_count +1
    }

    previous.damned = as.data.frame(filter(JUVENILES[[i-1]], calvin_type == "damned"))
    
    # Part A, Sample to fill gaps
    n.missing = juvenile.group.size - nrow(JUVENILES[[i]])
    if(n.missing < 1) {n.missing = 0}
    samples = sample(nrow(previous.damned), min(c(nrow(previous.damned), n.missing)))
    damned = previous.damned[samples,]
    damned$age = damned$age + 1



    # Part B apply mortality

    damned = filter(damned, age < age.at.maturity) # Individuals cannot become adults
    dead.probs = runif(nrow(damned))
    damned$survive = ifelse(dead.probs >= damned.mortality.rate, 1, 0)
    damned = filter(damned, survive ==1)
    damned = select(damned, -survive)
    


    # Part C fill gaps with new individuals
    n.missing.II = juvenile.group.size - nrow(damned) - nrow(JUVENILES[[i]])
    if(n.missing.II >0){
      
      damned.size = juvenile.group.size - nrow(JUVENILES[[i]])
      missing.Fs = ceiling(damned.size/2) - nrow(filter(damned, sex == "F"))
      missing.Ms = floor(damned.size/2) - nrow(filter(damned, sex == "M"))
      
      possible.mothers = filter(living.list[[i]], sex == "F"& age > age.at.maturity)$id
      possible.fathers = filter(living.list[[i]], sex == "M" & age > age.at.maturity)$id
      
      if(missing.Fs > 0){
        new.Fs = data.frame(
          id = sample_random_damned_id(missing.Fs),
          age = 0,
          sex = "F", 
          mother = sample(possible.mothers, missing.Fs, replace = TRUE), #Replace has to beTRUE for when juv mortlaity is really high
          father = NA,
          calvin_type = "damned"
        )
        extrapair.randoms = runif(missing.Fs)
        father.in = ifelse(extrapair.randoms > local.mating.rate, "Y", "N")
        # new.Fs$father = ifelse(father.in == "Y",
        #                        sample(possible.fathers,1),
        #                        sample(1000000000:1000000000000, 1)
        #                        )
        new.Fs$father.in = father.in
        for(k in 1:nrow(new.Fs)){
          if(new.Fs$father.in[k] == "N"){
            new.Fs$father[k] = sample_random_father(1)
          } else {
            new.Fs$father[k] = sample(possible.fathers,1)
          }
        }
        new.Fs = select(new.Fs, -(father.in))
        
      } else 
      {new.Fs = data.frame()}
      
      if(missing.Ms > 0){
        new.Ms = data.frame(
          id = sample_random_damned_id(missing.Ms),
          age = 0,
          sex = "M", 
          mother = sample(possible.mothers, missing.Ms, replace = TRUE),
          father = NA,
          calvin_type = "damned"
        )
        extrapair.randoms = runif(missing.Ms)
        father.in = ifelse(extrapair.randoms > local.mating.rate, "Y", "N")
        new.Ms$father.in = father.in
        for(k in 1:nrow(new.Ms)){
          if(new.Ms$father.in[k] == "N"){
            new.Ms$father[k] = sample_random_father(1)
          } else {
            new.Ms$father[k] = sample(possible.fathers,1)
          }
        }
        new.Ms = select(new.Ms, -(father.in))

        # new.Ms$father = ifelse(father.in == "Y",
        #                        sample(possible.fathers,1),
        #                        sample(1000000000:1000000000000, 1)
        # )
        
      } else 
      {new.Ms = data.frame()}
      
      new.damned = rbind(new.Fs,new.Ms)

      
    } else
      {new.damned = data.frame()} #if n.missing close
    
    damned = rbind(damned, new.damned)
    JUVENILES[[i]] = rbind(JUVENILES[[i]], damned)

  }# Looping through juveniles close
  
  if(show.damned.age.plot == TRUE){
    damned.plot = ggplot(filter(bind_rows(JUVENILES), calvin_type == "damned"), aes(age))+
      geom_histogram(binwidth = 1)
    print(damned.plot) 
  }
  
  messege = paste("Juvenile group size was artifically increased in", overspill_count, "of", R, "years", sep = " ")
  print(messege)
  return(JUVENILES)
}


#moved here for clarity in the function
run_checks_in_kindym_model = function(living.list, local.mating.rate, include.juves){
  
  if(is.null(living.list[[length(living.list)]])){
    print("There was an error. Removing non null years and returning them for error checking")
    living.list = Filter(Negate(is.null), living.list)
  }
  
  
  if(local.mating.rate < 1){
    all.living1 = do.call(rbind, living.list)
    all.ids = distinct(select(all.living1,id,father))
    duplicates = duplicated(filter(all.ids, father >= 1000000000)$father)
    if (sum(duplicates) > 1){
      stop("WARNING. Randomly generated father ids accidentally duplicated. Run model again") 
    }

  }
  
  if(include.juves == TRUE){
    all.living1 = bind_rows(living.list)
    all.ids = distinct(select(all.living1,id,mother))
    all.ids = filter(all.ids, id >= 1000000000001 )
    if(length(all.ids$id) != length(unique(all.ids$id))){
      stop("WARNING. Randomly generated juvenile id is accidentally duplicated. Run model again") 
    }
  }
  
}



