# grafos

if (!require("pacman")) install.packages("pacman") 

pacman::p_load(tidyverse, ggplot2, sjmisc, summarytools, sjPlot,
               kableExtra, withr, magick, png, gifski, dygraphs, 
               showtext, htmlTable, gridExtra, gtsummary, openxlsx,
               ggrepel)

a <- read.xlsx("input/compendio.xlsx",sheet = 20,rows=c(2:18),cols=c(1,2,5,6),na.strings = "**")

names(a) <- c("ano", "pob.afiliada", "tot.ocupados.potencia", "ts2")

b <- read.xlsx("input/compendio.xlsx",sheet = 21,rows=c(2:17),cols=c(1,2,5,6),na.strings = "**")

names(b) <- c("ano", "pob.afiliada", "tot.ocupados.potencia", "ts2")

df <- rbind(a,b) %>% as.data.frame()

formatter <- function(...){
  function(x) format(round(x, 1), ...)
}

g2 <- df %>% select(ano, ts2) %>% 
  mutate(ts2 = as.numeric(ts2)) %>% 
  filter(ano != 2020) %>% 
  ggplot(aes(x=ano, y = ts2)) +
  geom_line(size = 0.7, colour = "#E16462")+
  geom_point(size = 1.5, colour = "#E16462") +
  scale_x_continuous(breaks= c(1990, 1995, 2000, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(labels = formatter(nsmall = 1)) +
  geom_text_repel(aes(label = ifelse(ano %in% c(1991, 1995,2000,2004,2009,2015,2018), 
                                     format(paste0(round(ts2,1),"%")),"")), 
                  color = "#E16462", fontface = "bold", vjust = 1) +
  labs(subtitle = "Porcentaje afiliación sindical 1990-2019",
       y = "Afiliación sindical (%)",
       x = "Año",
       caption = "Fuente: elaboración propia en base a datos Dirección del Trabajo") + 
  theme_bw() +
  theme(plot.subtitle = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(0.9), 
                                    color = "grey30", hjust = 1))

g2 



ggsave(
  plot = last_plot(),
  filename = "../alast2022/images/density.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 25,
  height = 15
)


ohl_19 <- readWorkbook("../alast2022/input/Labor_Strikes_Dataset_2010-2019_Public.xlsx", detectDates = T)

ohl_18 <- readWorkbook("../alast2022/input/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates = T)

ohl <- ohl_18 %>%
  plyr::rbind.fill(ohl_19) %>%
  mutate(id = seq(3390, 15886, by = 1),
         folio = if_else(is.na(folio), id, folio)) %>% 
  distinct(folio, .keep_all = T)

g1 <- ohl %>% 
  filter(yr >= 1990) %>% 
  group_by(yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x= yr, y = n)) +
  geom_line(size = 0.7, colour = "#007df6")+
  geom_point(size = 1.5, colour = "#007df6")+
  scale_x_continuous(breaks = seq(1990, 2020, 1)) +
  scale_y_continuous(breaks = seq(200,450,50))+
  geom_text_repel(aes(label = ifelse(yr %in% c(1990, 1995,2000,2005,2010,2015,2019), format(paste0(n,"")),"")), color = "#007df6", fontface = "bold", size = 3.5) +
  labs(subtitle = "Número de huelgas 1990-2019",
       y = "Número huelgas",
       x = "Año",
       caption = "Fuente: elaboración propia en base a datos OHL (1979-2019)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.4),
        plot.subtitle = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(0.9), 
                                    color = "grey30", hjust = 1))

g1

ggsave(
  plot = last_plot(),
  filename = "../alast2022/images/strikes.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 25,
  height = 15
)
