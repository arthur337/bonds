library(tidyverse)
library(lubridate)
a <- read_csv("BONDS.csv")
b <- a %>% mutate(Size_Mio=parse_double(`Issued Amount`)/1000000, 
                  Maturity = parse_date(Maturity, "%d/%m/%Y")) %>% 
  select(Name,Ticker,Size_Mio,Maturity,Yield=`Ask Yield to Maturity`,Identifier) %>% 
  mutate(Yield = as.numeric(Yield),Year = year(Maturity)) %>% distinct()
c <- b %>% filter(Ticker %in% c("MGS", "MGII"))
chart1 <- c %>% ggplot(aes(Maturity,Yield,color=Ticker)) +
  geom_point(aes(size=Size_Mio), alpha=0.5) +
  geom_rug() +
  stat_smooth(geom='line', alpha=0.5, se=FALSE) +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") + 
  scale_y_continuous(breaks=round(seq(min(c$Yield),max(c$Yield),by=0.2),1)) + 
  labs(title = "Malaysian Govies Bonds",
       subtitle = "Maturities vs Yield",
       caption = "Source: Bloomberg") + 
  theme_bw() +
  theme(legend.position = c(0.9, 0.4)) 

chart2 <- c %>% ggplot(aes(Year,Size_Mio)) +
  geom_bar(stat="identity", aes(fill=Ticker), colour="black") +
  labs(y="Issuance size (Mio MYR)",
       title = "Malaysian Govies Bonds",
       subtitle = "Size of Maturities by Year",
       caption = "Source: Bloomberg") + 
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty(c$Year, n = 20)) +
  theme(legend.position = c(0.92, 0.8)) 
