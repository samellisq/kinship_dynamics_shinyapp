apply_thinning = function(living.list, thinning.n){
  n.to.keep = seq(1, length(living.list), thinning.n)
  if(length(n.to.keep) < 100){
    stop("Over thinned, fewer than 100 remainig data years. Raise thinning value")
  }
  
  return(living.list[n.to.keep])
}

make_gam_plot = function(gam.plotdata, plottitle){
  names(gam.plotdata) = ifelse(names(gam.plotdata) == "r.from.sex.to.group" | names(gam.plotdata) == "to.whom", "cats", names(gam.plotdata))
  gplot = ggplot(data = gam.plotdata, aes(x= age, y = relatedness, colour = cats, fill = cats))+
    geom_line(size = 1)+
    geom_ribbon(aes(ymin = relatedness - r.se, ymax = relatedness + r.se), alpha = 0.5, colour = NA)+
    xlab("Age (years)")+
    ylab("Local Relatedness (r)")+
    theme_bw()+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          plot.title = element_text(size = 20, face = "bold")
          )+
    ggtitle(plottitle)
    return(gplot)
}