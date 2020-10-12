library(digitize)
library(ggplot2)

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

quadratic_function <- function(x, q, r, s) {
  y <-  q*x^2 + r*x + s
  return(y)
}

x_range <- seq(15,35,by=0.1)
y_range <- quadratic_function(x_range, -1.908, 100.353, -1257.111)

sim_data <- as.data.frame(cbind(x_range, y_range))

EFD_plot <- ggplot(sim_data)+
  geom_line(aes(x=x_range, y=y_range))+
  geom_point(data=df, aes(x=x, y=y))+
  scale_y_continuous(name ="EFD", limits=c(0,80))+
  scale_x_continuous(name="Temperature", limits=c(0,40))+
  theme_bw()+
  labs(caption = "q=-1.908, r=100.353, s=-1257.111")

ggsave("Anopheles_EFD.pdf", EFD_plot)

