library(ggplot2)

r <- 13
ggplot() + 
  ylim(c(0,1)) + xlim(c(-r,r)) + theme_classic() + xlab('Log-odds scale') + 
  ylab('Probability scale') + 
  annotate('segment', x = -r, xend = r, y = .5, yend = .5, color = 'dodgerblue3', alpha = .6) + 
  annotate('segment', x = -r, xend = r, y = 0, yend = 0, color = 'orange', alpha = .8) + 
  annotate('segment', x = -r, xend = r, y = 1, yend = 1, color = 'orange', alpha = .8) +
  geom_function(fun = function(x) exp(x)/(1+exp(x)), size = .9, alpha = .9) 
ggsave('logit_plot.pdf', height = 4.8, width = 6)

ggplot() + geom_function(fun = function(x) log(x/(1-x)), alpha = .9) + 
  xlim(c(0,1)) + theme_classic() +ylim(c(-10,10)) + 
  annotate('segment', x = 0, xend = 0, y = -10, yend = 10, color = 'orange', alpha = .4) +
  annotate('segment', x = 1, xend = 1, y = -10, yend = 10, color = 'orange', alpha = .4) 
