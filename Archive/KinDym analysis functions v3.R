require(gridExtra)
require(survival)
require(kinship2)


##################################################################
## Sanity plotting
#################################################################
plot_sex_ratio = function(living.table.list){
  living.table = bind_rows(living.table.list, .id = "model.gen")
  sr.plot = ggplot(living.table, aes(model.gen))+
    geom_bar(aes(fill = sex)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    xlab("Model Year (excl Burn in) -->")+
    ylab("Population")
  return(sr.plot)
}

plot_recruited_origin = function(living.table.list, age.at.maturity){
  recruited.Ms = lapply(LIVING, filter, age  == (age.at.maturity +1), sex =="M")
  recruited.Fs = lapply(LIVING, filter, age  == (age.at.maturity +1), sex =="F")
  immigrant.Ms = unlist(lapply(recruited.Ms, function(x){ifelse(is.na(x$mother)&is.na(x$father),1,0)}))
  immigrant.Fs = unlist(lapply(recruited.Fs, function(x){ifelse(is.na(x$mother)&is.na(x$father),1,0)}))
  all.recruited = data.frame(sex = c(rep.int("M",length(immigrant.Ms)), rep.int("F", length(immigrant.Fs))),
                    immigrant = c(immigrant.Ms, immigrant.Fs)
                    )
  
  roplot = ggplot(all.recruited, aes(x = sex, y = immigrant))+
    stat_summary()+
    xlab("Sex")+
    ylab("Realised Dispersal Rate")
    theme_bw()
  return(roplot)
}

plot_survival = function(living.table.list){
  all.groups = do.call(rbind, living.table.list)
  all.groups$status = numeric(nrow(all.groups))
  all.groups = split(all.groups, all.groups$id)
  all.groups = lapply(all.groups, function(x){x$status = ifelse(x$age == max(x$age),1,0); return(x)})
  all.groups = do.call(rbind, all.groups)
  surv.obj=Surv(time = all.groups$age-1,time2 = all.groups$age,event = all.groups$status) # age -1 because we are looking at a snapshot at the end of the year
  coxtest= coxph(surv.obj ~ sex + cluster(id), data = all.groups)
  newdata.df = data.frame(sex = c("F", "M"))
  plot.data = survfit(coxtest, newdata.df)
  plot.data = data.frame (age = rep.int(c(min(plot.data$time)-1,plot.data$time),2),
                          sex = c(rep.int("F", (nrow(plot.data$surv)+1)),rep.int("M", (nrow(plot.data$surv)+1))),
                          survival = as.vector(rbind(c(1,1),plot.data$surv)),
                          SE = as.vector(rbind(c(0,0), plot.data$std.err)),
                          uCI = as.vector(rbind(c(1,1), plot.data$upper)),
                          lCI = as.vector(rbind(c(1,1), plot.data$lower))
  )
  splot = ggplot(plot.data, (aes(x=age, y = survival, colour = sex, fill = sex)))+
    geom_line(size=1)+
    geom_ribbon(aes(ymin=lCI, ymax=uCI),alpha=0.25, colour =NA)+
    xlab("Age")+
    ylab("Survival (Conf. Int's)")
  return(splot)
}


plot_age_structure = function(living.table.list){
  all.groups = do.call(rbind, living.table.list)
  summarised = count(all.groups, age,sex)
  summarised$n = ifelse(summarised$sex == "M", summarised$n * -1, summarised$n )
  summarised$age = summarised$age -1
  asplot = ggplot(summarised, aes(x = age, y = n, fill = sex))+
    geom_bar(data = subset(summarised,sex =="F"), stat = "identity", colour = "white")+
    geom_bar(data = subset(summarised,sex =="M"), stat = "identity", colour = "white")+
    scale_y_continuous(labels=abs)+
    xlab("Age")+
    ylab("Population")+
    coord_flip()
  return(asplot)
}

plot_local_mating = function(living.table.list, age.at.maturity){
  recruited = lapply(living.table.list, function(x){return(filter(x, age == (age.at.maturity+1)))} )
  philopatric = lapply(recruited, function(x){return(filter(x, !is.na(mother)))} )
  philopatric = lapply(philopatric, function(x){return(mutate(x,philo= ifelse(x$father > 1000000000,0, 1) ))})
  prop.philo = unlist(lapply(philopatric, function(x){mean(x$philo)}))
  prop.philo = data.frame(x = "local.mating", prop = prop.philo[!is.nan(prop.philo)])
  lm.plot = ggplot(prop.philo, aes(x= x, y = prop))+
    stat_summary()+
    ylim(0,1)+
    xlab("Local Mating")+
    ylab("Proportions")+
    theme(axis.text.x=element_blank())
  return(lm.plot)
}



sanity_check_plots = function(living.table.list, age.at.maturity){
  withProgress(message = "Plotting Model Populaiton Traits",min = 0, max =4 ,value = 0, { 
    plot1 = plot_sex_ratio(living.table.list)
    incProgress(1)
    plot2 = plot_recruited_origin(living.table.list, age.at.maturity)
    incProgress(1)
    plot3 = plot_survival(living.table.list)
    incProgress(1)
    plot4 = plot_local_mating(living.table.list, age.at.maturity)
    incProgress(1)
    grid.arrange(plot1,plot2,plot3, plot4)
  })

}


######################################################################################
# Calcualting relatedness
#####################################################################################

get_kinship_matrix = function(living.table.list, burn.in.persiod.living.table.list){
  all.individuals = do.call(rbind,c(living.table.list,burn.in.persiod.living.table.list))
  bonus.fathers = data.frame(id = unique(filter(all.individuals, father > 1000000000)$father),
                             age = rep.int(1, length(unique(filter(all.individuals, father > 1000000000)$father))),
                             sex = rep.int("M", length(unique(filter(all.individuals, father > 1000000000)$father))),
                             mother = rep.int(NA, length(unique(filter(all.individuals, father > 1000000000)$father))),
                             father = rep.int(NA, length(unique(filter(all.individuals, father > 1000000000)$father)))
                             )
  all.individuals = rbind(all.individuals, bonus.fathers)                           
  all.individuals = distinct(select(all.individuals, -age))
  all.individuals$sex = ifelse(all.individuals$sex == "M", "male", "female")
  all.pedigree = pedigree(id =all.individuals$id, dadid = all.individuals$father, momid = all.individuals$mother, sex = all.individuals$sex)
  all.kinship = kinship(all.pedigree)
  return(all.kinship)
}

get_local_relatedness = function(living.table, kinship.matrix){
  # living.table$relatedness.to.Fs
  # living.table$relatedness.to.Ms
  local.kin.all = kinship.matrix[as.character(living.table$id),as.character(living.table$id)]
  living.table$local.relatedness = (rowSums(local.kin.all) - local.kin.all[1,1])/(nrow(living.table)-1) #removing the [1,1] point removes the self-relatedness value, whatever it might be
  local.kin.Fs = kinship.matrix[as.character(living.table$id),as.character(living.table$id[living.table$sex =="F"])]
  if(ncol(data.frame(local.kin.Fs))==1){ # deals with an annoying problem that if down to 1 of either sex R convert it to a vector so can't do rowSums...
    living.table$relatedness.to.Fs = ifelse(local.kin.Fs == local.kin.all[1,1], local.kin.Fs - local.kin.all[1,1], local.kin.Fs)
  } else {
    living.table$relatedness.to.Fs = ifelse(living.table$sex == "F",
                                            (rowSums(local.kin.Fs) - local.kin.all[1,1])/(nrow(filter(living.table, sex == "F"))-1), # if focal female need to remove self value. Note ref to .all is correct
                                            (rowSums(local.kin.Fs))/(nrow(filter(living.table, sex == "F"))) # if male will not be present so not needed
    ) 
  }
  
 
  
  local.kin.Ms = kinship.matrix[as.character(living.table$id),as.character(living.table$id[living.table$sex =="M"])]
  if(ncol(data.frame(local.kin.Ms))==1){ # deals with an annoying problem that if down to 1 of either sex R convert it to a vector so can't do rowSums...
    living.table$relatedness.to.Ms = ifelse(local.kin.Ms == local.kin.all[1,1], local.kin.Ms - local.kin.all[1,1], local.kin.Ms)
  } else {
    living.table$relatedness.to.Ms = ifelse(living.table$sex == "M",
                                            (rowSums(local.kin.Ms) - local.kin.all[1,1])/(nrow(filter(living.table, sex == "M"))-1), # if focal male need to remove self value. Note ref to .all is correct
                                            (rowSums(local.kin.Ms))/(nrow(filter(living.table, sex == "M"))) # if female will not be present so not needed
    ) 
    }
  
  
  living.table$local.relatedness = living.table$local.relatedness*2 # mutliplying by 2 becasue kinship2 calcualtes realtedness ot self as 0.5, whereas I think J & C calcualte it as 1
  living.table$relatedness.to.Fs = living.table$relatedness.to.Fs*2 # mutliplying by 2 becasue kinship2 calcualtes realtedness ot self as 0.5, whereas I think J & C calcualte it as 1
  living.table$relatedness.to.Ms = living.table$relatedness.to.Ms*2 # mutliplying by 2 becasue kinship2 calcualtes realtedness ot self as 0.5, whereas I think J & C calcualte it as 1
  
  return(living.table)
}


###########################################################
# Calculating GAM smoothed fits of relatedness data
###############################################################

relatedness_GAM.plot = function(all.living.df, sex.to.plot, return.graph = TRUE, return.data = FALSE){
  if(sex.to.plot == "both"){ # "both" plots the comapritive kinship dynamics of males and females to an average memeber of the group 
    all.living.df$local.relatedness = (ifelse(all.living.df$local.relatedness < 0, 0, all.living.df$local.relatedness))+0.0001 # gets rid of negative relatedness and makes everything higher than 0 as Koster et al 2019
    all.living.df$relatedness.to.Fs = (ifelse(all.living.df$relatedness.to.Fs < 0, 0, all.living.df$relatedness.to.Fs))+0.0001 # gets rid of negative relatedness and makes everything higher than 0 as Koster et al 2019
    all.living.df$relatedness.to.Ms = (ifelse(all.living.df$relatedness.to.Ms < 0, 0, all.living.df$relatedness.to.Ms))+0.0001 # gets rid of negative relatedness and makes everything higher than 0 as Koster et al 2019
    
    fs = filter(all.living.df, sex == "F")
    ms = filter(all.living.df, sex == "M")
    
    #calculate the GAMs
    print("Calculating GAM 1/2")
    fgam = gam(local.relatedness ~ s(age) + s(id, bs = "re"), #s(age, k = 4)
               data = fs, 
               family=betar(link="logit"),
               method = "REML")
    
    print("Calculating GAM 2/2")
    mgam = gam(local.relatedness ~ s(age) + s(id, bs = "re"), # s(age, k = 4)
               data = ms, 
               family=betar(link="logit"),
               method = "REML")
    
    # predict the values from the GAMS
    plotdata = data.frame(age = seq(min(all.living.df$age),max(all.living.df$age),1),
                          id = median(all.living.df$id)
    )
    
    predict.fgam = predict(fgam, newdata = plotdata, type = "response", se.fit = TRUE)
    predict.mgam = predict(mgam, newdata = plotdata, type = "response", se.fit = TRUE)
    
    #prepare for plotting
    plotdata = data.frame(age = rep.int(plotdata$age,2),
                          id = rep.int(plotdata$id, 2),
                          r.from.sex.to.group = c(rep.int("Females", nrow(plotdata)),rep.int("Males", nrow(plotdata))),
                          relatedness = c(predict.fgam$fit,predict.mgam$fit),
                          r.se = 1.96*c(predict.fgam$se.fit,predict.mgam$se.fit) # 1.96 * SE = estiamted CI
    )
    gplot = ggplot(data = plotdata, aes(x= age, y = relatedness, colour = r.from.sex.to.group, fill = r.from.sex.to.group))+
      geom_line(size = 1)+
      geom_ribbon(aes(ymin = relatedness - r.se, ymax = relatedness + r.se), alpha = 0.5, colour = NA)+
      xlab("Age (years)")+
      ylab("Local Relatedness (r)")
    
    
  } else { # if sex is specified prodece a plot showing the relationship of that sex to all, to Ms and to Fs, cf J and C 2010
    #organise the data
    onesexdf = filter(all.living.df, sex == sex.to.plot)
    onesexdf$local.relatedness = (ifelse(onesexdf$local.relatedness < 0, 0, onesexdf$local.relatedness))+0.0001 # gets rid of negative relatedness and makes everything higher than 0 as Koster et al 2019
    onesexdf$relatedness.to.Fs = (ifelse(onesexdf$relatedness.to.Fs < 0, 0, onesexdf$relatedness.to.Fs))+0.0001 # gets rid of negative relatedness and makes everything higher than 0 as Koster et al 2019
    onesexdf$relatedness.to.Ms = (ifelse(onesexdf$relatedness.to.Ms < 0, 0, onesexdf$relatedness.to.Ms))+0.0001 # gets rid of negative relatedness and makes everything higher than 0 as Koster et al 2019
    
    
    withProgress(message = "Fitting GAM",min = 0, max =3 ,value = 0, { 
      print("Calculating GAM 1/3")
      incProgress(amount = 1 ,detail = "1/3")
      gam.toAll = gam(local.relatedness ~ s(age, k = 4) + s(id, bs = "re"), 
                      data = onesexdf, 
                      family=betar(link="logit"),
                      method = "REML")
      
      print("Calculating GAM 2/3")
      incProgress(amount = 1 ,detail = "2/3")
      gam.toFs = gam(relatedness.to.Fs ~ s(age, k = 4) + s(id, bs = "re"), 
                     data = onesexdf, 
                     family=betar(link="logit"),
                     method = "REML")
      
      print("Calculating GAM 3/3")
      incProgress(amount = 1 ,detail = "3/3")
      gam.toMs = gam(relatedness.to.Ms ~ s(age, k = 4) + s(id, bs = "re"), 
                     data = onesexdf, 
                     family=betar(link="logit"),
                     method = "REML")
      
    })
    # calculate the gams
    
    
    #plot the data
    plotdata = data.frame(age = seq(min(onesexdf$age),max(onesexdf$age),1),
                          id = median(onesexdf$id)
    )
    
    predict.gamtoall = predict(gam.toAll, newdata = plotdata, type = "response", se.fit = TRUE)
    predict.gamtoFs = predict(gam.toFs, newdata = plotdata, type = "response", se.fit = TRUE)
    predict.gamtoMs = predict(gam.toMs, newdata = plotdata, type = "response", se.fit = TRUE)
    
    plotdata = data.frame(age = rep.int(plotdata$age,3),
                          id = rep.int(plotdata$id, 3),
                          to.whom = c(rep.int("toAll", nrow(plotdata)),rep.int("toFs", nrow(plotdata)),rep.int("toMs", nrow(plotdata))),
                          relatedness = c(predict.gamtoall$fit,predict.gamtoFs$fit, predict.gamtoMs$fit),
                          r.se = 2*c(predict.gamtoall$se.fit,predict.gamtoFs$se.fit, predict.gamtoMs$se.fit)
    )
    
    gplot = ggplot(data = plotdata, aes(x= age, y = relatedness, colour = to.whom, fill = to.whom))+
      geom_line(size = 1)+
      geom_ribbon(aes(ymin = relatedness - r.se, ymax = relatedness + r.se), alpha = 0.5, colour = NA)+
      xlab("Age (years)")+
      ylab("Local Relatedness (r)")
    
    
  }
  
  if(return.graph == TRUE){ print(gplot) }
  if(return.data==TRUE){ return(plotdata) }  
  
}