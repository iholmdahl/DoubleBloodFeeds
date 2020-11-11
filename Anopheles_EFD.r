library(digitize)

cal = ReadAndCal('Anopheles_EFD.png')
data.points = DigitData(col = 'red')

df = Calibrate(data.points, cal, 0, 40, 0, 80)

write.csv(df, "Anopheles_EFD.csv")

ggplot(df)+
  geom_point(aes(x=x, y=y))+
  scale_y_continuous(limits=c(0,80))+
  scale_x_continuous(limits=c(0,40))+
  theme_bw()

params <- nls(y ~ q*x^2 + r*x + s, 
    data=df, 
    start = list(q = -0.153, r = 8.61, s = -97.7))
