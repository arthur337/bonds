library(tidyverse)
library(lubridate)
library(plotly)
a <- read_csv("BONDS.csv")
b <- a %>% mutate(Size_Mio=parse_double(`Issued Amount`)/1000000, Maturity = parse_date(Maturity, "%d/%m/%Y"), Maturity2 = as.numeric(Maturity)) %>% 
  select(Name,Ticker,Size_Mio,Maturity,Maturity2,Yield=`Ask Yield to Maturity`,Identifier) %>% 
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

chart3 <- c %>% ggplot(aes(Maturity,Yield,color=Ticker)) +
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

chart4 <- c %>% ggplot(aes(Maturity,Yield,color=Ticker)) +
  geom_point(aes(size=Size_Mio), alpha=0.5) +
  geom_rug() +
  geom_smooth(se = FALSE,alpha=0.5, size=0.5)

(p1 <- ggplotly(chart4) %>% 
    layout(legend = list(x = 0.6, y = 0.4), 
      title = "Malaysian Govies Bonds",
      xaxis = list(title = "Maturity"),
      yaxis = list(title = "Traded Yield")))
p1$x$data[[2]]$text <- paste('Maturity Date: ', c$Maturity,
                               '<br> Traded Yield: ', c$Yield,
                               '<br> Issuance Size in millions: ', c$Size_Mio,
                               '<br> Type: ', c$Ticker)
p1$x$data[[1]]$text <- paste('Maturity Date: ', c$Maturity,
                               '<br> Traded Yield: ', c$Yield,
                               '<br> Issuance Size in millions: ', c$Size_Mio,
                               '<br> Type: ', c$Ticker)

(p <- plot_ly(c, x = ~Maturity, y = ~Yield, type = 'scatter', mode = 'markers', size = ~Size_Mio, color =~Ticker, sizes = c(50, 0),
              hoverinfo = 'text',
              text = ~paste('Maturity Date: ', Maturity,
                            '<br> Traded Yield: ', Yield,
                            '<br> Issuance Size: ', Size_Mio,
                            '<br> Type: ', Ticker)) %>%
             layout(legend = list(x = 0.6, y = 0.4), 
                    title = "Malaysian Govies Bonds",
                    xaxis = list(title = "Maturity"),
                    yaxis = list(title = "Traded Yield"),
                    paper_bgcolor = 'rgb(243, 243, 243)',
                    plot_bgcolor = 'rgb(243, 243, 243)')
  )


(p2 <- plot_ly(c, x = ~Maturity, color =~Ticker) %>% 
    add_markers(y = ~Yield, type = 'scatter', mode = 'markers', size = ~Size_Mio, sizes = c(50, 0),
              hoverinfo = 'text', text = ~paste('Maturity Date: ', Maturity,
                            '<br> Traded Yield: ', Yield,
                            '<br> Issuance Size: ', Size_Mio,
                            '<br> Type: ', Ticker)) %>%
    add_lines(y = ~fitted(loess(Yield ~ Maturity2))) %>%
    layout(legend = list(x = 0.6, y = 0.4), 
           title = "Malaysian Govies Bonds",
           xaxis = list(title = "Maturity"),
           yaxis = list(title = "Traded Yield"),
           paper_bgcolor = 'rgb(243, 243, 243)',
           plot_bgcolor = 'rgb(243, 243, 243)')
)
