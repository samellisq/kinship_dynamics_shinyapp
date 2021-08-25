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
                                                        sample(1000000000:1000000000000, 1) ) # this is not a very elegant solution. Every father needs and id for kinship to work. This should mean they are unique. Checked for later.
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
                                                        sample(1000000000:1000000000000, 1) ) # this is not a very elegant solution. Every father needs and id for kinship to work. This should mean they are unique. Checked for later.
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