library(readxl)
library(httr)
library(tidyverse)
library(plotly)
library(googleVis)
library(R2HTML)
library(stringi)
library(pander)

url<-'https://plataforma.chileconvencion.cl/m/iniciativa_popular/iniciativas.xls'
GET(url, write_disk(TF <- tempfile(fileext = ".xls")))

iniciativas <- read_excel(TF) %>% as_tibble()

# Iniciativas más populares


populares_01 <- iniciativas %>% 
  select(Nº, `Título`, `Cantidad de Apoyos`, URL) %>%
  rename(NUMERO = Nº,
         TITULO = `Título`,
         APOYOS = `Cantidad de Apoyos`) %>% 
  mutate(TITULO = stri_trans_general(toupper(TITULO), "Latin-ASCII")) %>% 
  arrange(desc(APOYOS)) %>% 
  filter(APOYOS > 3000) %>% 
  slice_head(n = 30) 

populares_01$NUMERO <- factor(populares_01$NUMERO, 
                              levels = populares_01$NUMERO[order(populares_01$APOYOS)]) 

m <- list(l=10, r=0, b=50, t=50, pad=4)

fig <- plot_ly(populares_01, x = ~NUMERO, y = ~APOYOS, type = 'bar', text = ~TITULO,
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>% 
  layout(title = 'Iniciativas con mayor apoyo', font=list(size = 20)) %>%
  layout(xaxis = list(title = 'Número de la iniciativa'), yaxis = list(title = 'Número de apoyos')) %>%
  layout(xaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)), 
         yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10))) %>% 
  layout(autosize = F, width = 800, margin = m)

fig

populares_02 <- populares_01 %>% 
  filter(APOYOS > 15000)

nrow(populares_02)

# Iniciativas por comisión

comision_01 <- iniciativas %>% 
  group_by(Comisión) %>% 
  summarise("Iniciativas por comisión" = n())

comision_01$Comisión <- factor(comision_01$Comisión, 
                              levels = comision_01$Comisión[order(comision_01$`Iniciativas por comisión`)]) 

m <- list(l=500, r=0, b=10, t=30, pad=4)

fig <- plot_ly(comision_01, x = ~`Iniciativas por comisión`, y = ~Comisión, type = 'bar', text = ~Comisión,
               marker = list(color = 'rgb(183,191,16)',
                             line = list(color = 'rgb(121,134,60)',
                                         width = 1.5)),
               orientation = 'h') %>% 
  layout(title = 'Iniciativas por comisión', font=list(size = 15)) %>%
  layout(xaxis = list(title = 'Número de iniciativas'), yaxis = list(title = '')) %>%
  layout(xaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)), 
         yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10))) %>% 
  layout(margin=m)

fig

# Iniciativas por tema

tema_01 <- iniciativas %>% 
  group_by(Tema) %>% 
  count() %>% 
  filter(n > 10) %>% 
  arrange(desc(n)) 

tema_02 <- tema_01 %>% 
  filter(n > 20)

tema_02$Tema <- factor(tema_02$Tema, levels = tema_02$Tema[order(tema_02$n)])
m <- list(l=500, r=0, b=10, t=30, pad=4)


fig <- plot_ly(tema_02, x = ~n, y = ~Tema, type = 'bar', text = ~Tema,
               marker = list(color = 'rgb(183,191,16)',
                             line = list(color = 'rgb(121,134,60)',
                                         width = 1.5)),
               orientation = 'h') %>%
  layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
  layout(title = 'Temas con más de 20 iniciativas', font=list(size = 15)) %>%
  layout(xaxis = list(title = 'Número de iniciativas'), yaxis = list(title = '')) %>%
  layout(xaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)), 
         yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10))) %>%
  layout(margin=m)

fig

# Por persona

personas <- iniciativas %>% 
  group_by(Autor) %>% 
  summarise("Número de iniciativas" = n()) %>% 
  filter(`Número de iniciativas` > 1)

personas_01 <- personas %>% 
  group_by(`Número de iniciativas`) %>% 
  summarise("Número de personas" = n()) 

sum(personas_01$`Número de personas`)

# Personas con más de una iniciativa

personas_01$`Número de iniciativas` <- factor(personas_01$`Número de iniciativas`, 
                                              levels = personas_01$`Número de iniciativas`[order(personas_01$`Número de personas`)]) 

m <- list(l=50, r=0, b=10, t=30, pad=4)

fig <- plot_ly(personas_01, x = ~`Número de personas`, y = ~`Número de iniciativas`, type = 'bar',
               marker = list(color = 'rgb(183,191,16)',
                             line = list(color = 'rgb(121,134,60)',
                                         width = 1.5)),
               orientation = 'h') %>% 
  layout(title = 'Número de iniciativas por persona', font=list(size = 15)) %>%
  layout(xaxis = list(title = 'Número de personas'), yaxis = list(title = 'Cantidad de iniciativas')) %>%
  layout(xaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)), 
         yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10))) %>% 
  layout(margin=m)

fig


personas_02 <- iniciativas %>% 
  group_by(Autor) %>% 
  summarise("Número de iniciativas" = n()) %>% 
  filter(`Número de iniciativas` > 3)

# Personas con más de una iniciativa

personas_02$Autor <- factor(personas_02$Autor, 
                            levels = personas_02$Autor[order(personas_02$`Número de iniciativas`)]) 

m <- list(l=170, r=0, b=10, t=30, pad=4)

fig <- plot_ly(personas_02, x = ~`Número de iniciativas`, y = ~Autor, type = 'bar',
               marker = list(color = 'rgb(183,191,16)',
                             line = list(color = 'rgb(121,134,60)',
                                         width = 1.5)),
               orientation = 'h') %>% 
  layout(title = 'Personas con más de 3 iniciativas', font=list(size = 15)) %>%
  layout(xaxis = list(title = 'Número de iniciativas'), yaxis = list(title = '')) %>%
  layout(xaxis = list(titlefont = list(size = 10), tickfont = list(size = 10)), 
         yaxis = list(titlefont = list(size = 10), tickfont = list(size = 10))) %>% 
  layout(margin=m)

fig
