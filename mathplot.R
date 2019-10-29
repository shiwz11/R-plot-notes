eqs = function(x){
  y = 25/3 + 1/3 * x
  d = data.frame(x = x, y = y)
  return(d)
}
eqd = function(x){
  y = 25 - 0.5 * x
  d = data.frame(x = x, y = y)
  return(d)
}
df = tibble(x = seq(0, 35, length.out = 10000))

df[['ps']] = 25/3 + 1/3 * df$x
df[['pd']] = 25 - 0.5 * df$x
#library(tidyverse)
df[['x1']] = 20
df[['y1']] = seq(0, 15, length.out = 10000)

df[['x2']] = 14
df[['y2']] = seq(0, 18, length.out = 10000)

df[['x3']] = seq(0, 14, length.out = 10000)
df[['y3']] = 18


df[['x4']] = seq(0, 20, length.out = 10000)
df[['y4']] = 15


df[['x5']] = seq(0, 14, length.out = 10000)
df[['y5']] = 13

df = as.data.frame(df)

windows();ggplot(df) +
  geom_line(aes(x = x, y = ps))+
  geom_line(aes(x = x, y = pd))+
  geom_line(aes(x = x1, y = y1), lty = 2)+ 
  geom_line(aes(x = x2, y = y2), lty = 2)+ 
  geom_line(aes(x = x3, y = y3), lty = 2)+ 
  geom_line(aes(x = x4, y = y4), lty = 2)+ 
  geom_line(aes(x = x5, y = y5), lty = 2) +
  geom_ribbon(data = subset(df, x>=14&x<=20), aes(x, ymin = ps, ymax = pd), fill = 'grey', alpha = "0.3")+
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank())+ 
  theme(axis.text.x = element_blank())+
  #theme(panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(-5, 27))+
  scale_x_continuous(limits = c(-5, 37))+
  annotate("text", x=30, y=20, parse=TRUE,
           label="P==frac(25,3) + frac(Q, 3)")+
  annotate("text", x=30, y=12, parse=TRUE,
           label="P==25 - frac(Q, 2)") +
  annotate("text", x=14, y=-1, parse=TRUE,
           label="14")+
  annotate("text", x=20, y=-1, parse=TRUE,
           label="20")+
  annotate("text", x=-3, y=25, parse=TRUE,
           label="25")+
  annotate("text", x=-3, y=18, parse=TRUE,
           label="P_d == 18")+
  annotate("text", x=-3, y=15, parse=TRUE,
           label="P^'*' == 15")+
  annotate("text", x=-3, y=13, parse=TRUE,
           label="P_s == 13")+
  annotate("text", x=-3, y=25/3, parse=TRUE,
           label="frac(25, 3)")+
  annotate("segment", x=18, xend=17, y=20, yend=16.5, colour="blue",
           size=1, arrow=arrow()) +
  annotate("text", x=18, y=21, label="无谓损失")+
  annotate("text", x=-1, y=-1, parse=TRUE,
           label="0")+
  annotate("text", x=36, y=-1, parse=TRUE,
           label="Q")+
  annotate("text", x=-1, y=27, parse=TRUE,
           label="P")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)

x = seq(0, 35, 0.005)
ps = eqs(x)
pd = eqd(x)
library(tidyverse)
ggplot(ps, aes(x, y)) + geom_line(color='black')+
  p = ggplot(pd, aes(x, y))+geom_line()


curve(eqs, from=0, to = 35, xlab = 'Q', ylab = 'P', ylim = c(0, 30))

curve(eqd, from=0, to = 35, add = T)
lines(c(20, 20), c(0, 15), lty = 2)
lines(c(0, 20), c(15, 15), lty = 2)
lines(c(14, 14), c(13, 18), lty = 2)
lines(c(0, 14), c(13, 13), lty = 2)
lines(c(0, 14), c(18, 18), lty = 2)
