library(tidyverse)
library(magrittr)
library(coda)

rosa <- "#F78D7C"
azul <- "#4E67C8"
lucify_basics <- function(){
  theme(line = element_line(colour = rgb(red = 102, green = 102, blue = 102, maxColorValue = 255)),
        rect = element_rect(colour = rgb(red = 198, green = 198, blue = 198, maxColorValue = 255)),
        text = element_text(family = "AvantGarde Bk BT", 
                            colour = rgb(red = 60, green = 60, blue = 60, maxColorValue = 255),
                            size = 17),
        axis.title.x = element_text(size = rel(1.2),
                                    margin = margin(t = 15)),
        axis.title.x.top = element_text(size = rel(1.2),
                                        margin = margin(b = 15)),
        axis.title.y = element_text(size = rel(1.2),
                                    margin = margin(r = 30)),
        axis.title.y.right = element_text(size = rel(1.2),
                                          margin = margin(l = 30)),
        axis.text.x = element_text(margin = margin(t = 8)),
        axis.text.x.top = element_text(margin = margin(b = 8)),
        axis.text.y = element_text(margin = margin(r = 8)),
        axis.text.y.right = element_text(margin = margin(l = 8)),
        axis.ticks = element_blank(),
        legend.margin = margin(t = 15, r = 15, b = 15, l = 15),
        legend.title = element_text(size = rel(1.15)),
        panel.spacing = unit(15,"pt"),
        plot.title = element_text(size = rel(1.3),
                                  hjust = 0.5,
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(hjust = 0.5, 
                                     margin = margin(t = -12.5, b = 20)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        strip.text = element_text(size = rel(1.1),
                                  margin = margin(t = 8, r = 8, b = 8, l = 8)))
}


GS_Norm2 <- function(N, inicio, media = c(0,0), var1 = 1, var2 = 1, rho = 0){
  
  #Creamos factores y varianzas para condicionales completas
  factor1 <- rho*sqrt(var1/var2)
  factor2 <- rho*sqrt(var2/var1)
  sd_cond1 <- sqrt(var1*(1-rho^2))
  sd_cond2 <- sqrt(var2*(1-rho^2))
  #Creamos data frames
  Propuestas <- data_frame(Var1 = rep(NA_real_,N+1), Var2 = rep(NA_real_,N+1))
  Propuestas[1,] <- inicio # propuesta inicial
  

  #Simulaciones
  for(n in 2:{N+1}){
    # Pasos de Gibbs
    Propuestas[n,1] <- rnorm(1, mean = media[1] + factor1*(Propuestas$Var2[n-1] - media[2]), sd = sd_cond1)
    Propuestas[n,2] <- rnorm(1, mean = media[2] + factor2*(Propuestas$Var1[n] - media[1]), sd = sd_cond2)
  }
  
  return(mutate(Propuestas, n = 0:N, Tipo = "Simulación"))
}

graf_base <- expand.grid(seq(-3,3,by = 0.05),seq(-3,3,by = 0.05)) %>% 
  as_data_frame %>% 
  {mutate(., Densidad = mvtnorm::dmvnorm(., mean = c(0,0), sigma = matrix(c(1,0,0,1), nrow=2)))} %>% 
  ggplot(aes(x=Var1,y=Var2)) + 
  geom_raster(aes(fill = Densidad)) + 
  geom_contour(aes(z=Densidad),
               color = rgb(red = 60, green = 60, blue = 60, maxColorValue = 255), bins = 10, size = rel(.25)) + 
  scale_fill_gradient(low = "transparent", high = rosa) + 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

graf_base_corr <- expand.grid(seq(-3,3,by = 0.05),seq(-3,3,by = 0.05)) %>% 
  as_data_frame %>% 
  {mutate(., Densidad = mvtnorm::dmvnorm(., mean = c(0,0), sigma = matrix(c(1,0.95,0.95,1), nrow=2)))} %>% 
  ggplot(aes(x=Var1,y=Var2)) + 
  geom_raster(aes(fill = Densidad)) + 
  geom_contour(aes(z=Densidad),
               color = rgb(red = 60, green = 60, blue = 60, maxColorValue = 255), bins = 10, size = rel(.25)) + 
  scale_fill_gradient(low = "transparent", high = rosa) + 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

# graf_base_convergencia <- expand.grid(seq(-5,5,by = 0.05),seq(-5,5,by = 0.05)) %>% 
#   as_data_frame %>% 
#   {mutate(., Densidad = mvtnorm::dmvnorm(., mean = c(0,0), 
#                                          sigma = matrix(c(3,-0.99*sqrt(3*1.5),-0.99*sqrt(3*1.5),1.5), nrow=2)))} %>% 
#   ggplot(aes(x=Var1,y=Var2)) + 
#   geom_raster(aes(fill = Densidad)) + 
#   geom_contour(aes(z=Densidad),
#                color = rgb(red = 60, green = 60, blue = 60, maxColorValue = 255), bins = 10, size = rel(.25)) + 
#   scale_fill_gradient(low = "transparent", high = "darkorange") + 
#   theme_minimal() + 
#   lucify_basics() + 
#   theme(panel.grid = element_blank())  

#### Ejemplo Base ####
#set.seed(31122018)
set.seed(51295)
Ejemplo_Est_Indep <- GS_Norm2(N = 1250,inicio = c(3,-3))
datos_graf <- data_frame(Var1 = Ejemplo_Est_Indep$Var1[-1], 
                         Var2 = Ejemplo_Est_Indep$Var2[-nrow(Ejemplo_Est_Indep)]) %>% 
  mutate(n = 1:n(), Tipo = "Paso intermedio") %>% 
  bind_rows(Ejemplo_Est_Indep) %>% 
  arrange(n)
ejemplo_est_indep_a <- graf_base + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n < 10, n >0), 
             color = azul, size = rel(4), shape = 18) + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n == 0), 
             color = azul, size = rel(2)) + 
  geom_path(data = filter(datos_graf, n <= 10), 
            color = azul, size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-2.5,3.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,by=1))+ 
  labs(title = "Primeras iteraciones con pasos intermedios") + 
  theme(legend.position = c(0.9,0.5))

ggsave("Bayes/Ejemplo_GS_A.pdf",plot = ejemplo_est_indep_a, device = cairo_pdf, width = 22.5/3, height = 17/3)

ejemplo_est_indep_b <- graf_base + 
  geom_point(data = filter(Ejemplo_Est_Indep, n >0), 
             color = azul, size = rel(1), shape = 18) + 
  geom_point(data = filter(Ejemplo_Est_Indep, n == 0), 
             color = azul, size = rel(1)) + 
  geom_path(data = Ejemplo_Est_Indep,
            color = azul, size = rel(0.05), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-3.5,5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-3.25,3.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "1,250 iteraciones") + 
  theme(legend.position = "none")

ggsave("Bayes/Ejemplo_GS_B.pdf",plot = ejemplo_est_indep_b, device = cairo_pdf, width = 22.5/3, height = 17/3)


ejemplo_est_indep_c <- Ejemplo_Est_Indep %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 1250, y = 0, yend = 0, color = rosa, size = rel(1)) +
  geom_path(size = rel(0.75), color = azul) + 
  annotate("text",x = 80, y = c(3.05,-3.05), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 1300, y = 0, label = "Media real", size = rel(5), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos por variable") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,1500), breaks = seq(0,1250,by=250))+ 
  scale_y_continuous(limits = c(-3.5,3.5), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo_GS_C.pdf",plot = ejemplo_est_indep_c, device = cairo_pdf, width = 22.5/3, height = 17/3)

#### Ejemplo con correlación ####
set.seed(31122018)
Ejemplo_Est_Corr <- GS_Norm2(N = 1250, inicio = c(-2.5,2.5), rho = .95)
datos_graf <- data_frame(Var1 = Ejemplo_Est_Corr$Var1[-1], 
                         Var2 = Ejemplo_Est_Corr$Var2[-nrow(Ejemplo_Est_Corr)]) %>% 
  mutate(n = 1:n(), Tipo = "Paso intermedio") %>% 
  bind_rows(Ejemplo_Est_Corr) %>% 
  arrange(n)
ejemplo_est_corr_a <- graf_base_corr + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n < 10, n >0), 
             color = azul, size = rel(4), shape = 18) + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n == 0), 
             color = azul, size = rel(2)) + 
  geom_path(data = filter(datos_graf, n <= 10), 
            color = azul, size = rel(0.75), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-3.5,4), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-3,4), breaks = seq(-3,3,by=1))+ 
  labs(title = "Primeras 10 iteraciones de GS") + 
  theme(legend.position = c(0.8,0.3))

ggsave("Bayes/Ejemplo_GS_Compara1.pdf",plot = ejemplo_est_corr_a, device = cairo_pdf, width = 22.5/3, height = 17/3)

ejemplo_est_corr_b <- graf_base_corr + 
  geom_point(data = Ejemplo_Est_Corr,
             color = azul, size = rel(0.5), shape = 18) + 
  scale_x_continuous(limits = c(-3.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-3,3.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "1,250 transiciones de GS") + 
  theme(legend.position = c(0.8,0.3))

ggsave("Bayes/Ejemplo_GS_Compara2.pdf",plot = ejemplo_est_corr_b, device = cairo_pdf, width = 22.5/3, height = 17/3)

ejemplo_est_corr_c <- Ejemplo_Est_Corr %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 1250, y = 0, yend = 0, color = rosa, size = rel(0.75)) +
  geom_path(size = rel(0.75), color = azul) + 
  annotate("text",x = 80, y = c(-2.5,2.5), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 1300, y = 0, label = "Media real", size = rel(5), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos para GS") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,1500), breaks = seq(0,1250,by=250))+ 
  scale_y_continuous(limits = c(-2.75,2.75), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo_GS_Compara3.pdf",plot = ejemplo_est_corr_c, device = cairo_pdf, width = 22.5/3, height = 17/3)

#### Ejemplos para Convergencia ####
set.seed(51295)
datos_graf <- data_frame(V1 = c(-5,5,5,-5),
                         V2 = c(-5,5,-5,5),
                         Cadena = 1:4) %>% 
  pmap_dfr(~GS_Norm2(N = 6000, inicio = c(..1,..2), var1 = 3, var2 = 1.5, rho = -0.92) %>% 
             mutate(Cadena = as.character(..3))) 

promedios_cadenas <- datos_graf %>% 
  filter(Tipo == "Simulación") %>% 
  group_by(Cadena) %>% 
  mutate_at(c("Var1","Var2"), funs(Media = cummean(.))) %>% 
  ungroup %>% 
  select(n,ends_with("Media"),Cadena) %>% 
  gather(Variable,Media,-n,-Cadena) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,color=Cadena)) + 
  annotate("rect",xmin = 0, xmax = 1200, ymin = -6.5, ymax = 6.5, alpha = 0.4, fill = "gray85") + 
  geom_segment(x = 0, xend = 6050, y = 0, yend = 0, color = "gray5", size = rel(.75)) +
  geom_path(size = rel(0.5)) + 
  facet_grid(Variable~.) + 
  annotate("text",x = 600, y = 5, label = "Calentamiento") + 
  labs(title = "Promedios Ergódicos de GS para 4 cadenas") + 
  ylab("Promedio ergódico") + 
  scale_color_manual(values = c(azul,"#2D9779","#F78D7C","#B8C2E9")) + 
  scale_y_continuous(breaks = seq(-5,5,by=2.5)) + 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank()) 

ggsave("Bayes/Ejemplos_Convergencia_Prom_Erg.pdf",
       plot = promedios_cadenas, device = cairo_pdf, width = 22.5/2, height = 17/2.5)

traceplot_inicial <- datos_graf %>% 
  filter(Tipo == "Simulación") %>% 
  gather(Variable,Valor,Var1,Var2) %>% 
  ggplot(aes(x=n,y = Valor, color = Cadena)) + 
  annotate("rect",xmin = 0, xmax = 1200, ymin = -8, ymax = 8, alpha = 0.4, fill = "gray85") + 
  annotate("text",x = 600, y = 7, label = "Calentamiento") + 
  geom_path(size = rel(0.1)) + 
  facet_grid(Variable~.) + 
  scale_color_manual(values = c(azul,"#2D9779","#F78D7C","#B8C2E9")) + 
  labs(title = "Traceplot inicial de GS para 4 cadenas") + 
  theme_minimal() + 
  lucify_basics() + 
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("Bayes/Ejemplos_Convergencia_Traceplot_Inicial.pdf",
       plot = traceplot_inicial, device = cairo_pdf, width = 22.5/2, height = 17/2.5)


autocorr_completas <- datos_graf %>% 
  filter(Tipo == "Simulación") %>% 
  gather(Variable,Valor,Var1,Var2) %>% 
  unite(Aux,Cadena,Variable) %>% 
  split(.$Aux) %>% 
  map_dfr(~ acf(.x$Valor,plot=FALSE) %>% 
            extract2("acf") %>% 
            as_data_frame %>% 
            set_colnames("acf") %>% 
            mutate(lag = 1:n()-1, Aux = unique(.x$Aux)) %>% 
            separate(Aux,c("Cadena","Variable"))) %>% 
  ggplot(aes(x=lag,y=acf,fill=Cadena)) + 
  geom_col() + 
  facet_grid(Variable~Cadena) + 
  scale_fill_manual(values = c(azul,"#2D9779","#F78D7C","#B8C2E9")) + 
  labs(title = "Autocorrelación de las cadenas completas") + 
  theme_minimal() + 
  lucify_basics() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

ggsave("Bayes/Ejemplos_Convergencia_Autocorr_Completas.pdf",
       plot = autocorr_completas, device = cairo_pdf, width = 22.5/2, height = 17/2.5)


traceplot_final <- datos_graf %>% 
  filter(Tipo == "Simulación", mod(n,6) %>% equals(0), n > 1200) %>% 
  group_by(Cadena) %>% 
  mutate(n = 1:n()) %>% 
  ungroup %>% 
  gather(Variable,Valor,Var1,Var2) %>% 
  ggplot(aes(x=n,y = Valor, color = Cadena)) + 
  geom_path(size = rel(0.2)) + 
  facet_grid(Variable~.) + 
  scale_color_manual(values = c(azul,"#2D9779","#F78D7C","#B8C2E9")) + 
  labs(title = "Traceplot final de GS para 4 cadenas",
       subtitle = "Calentamiento de 1200 y espaciamiento de 6 iteraciones") + 
  theme_minimal() + 
  lucify_basics() + 
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("Bayes/Ejemplos_Convergencia_Traceplot_Final.pdf",
       plot = traceplot_final, device = cairo_pdf, width = 22.5/2, height = 17/2.5)


autocorr_final <- datos_graf %>% 
  filter(Tipo == "Simulación", mod(n,6) %>% equals(0), n > 1200) %>% 
  gather(Variable,Valor,Var1,Var2) %>% 
  unite(Aux,Cadena,Variable) %>% 
  split(.$Aux) %>% 
  map_dfr(~ acf(.x$Valor,plot=FALSE) %>% 
            extract2("acf") %>% 
            as_data_frame %>% 
            set_colnames("acf") %>% 
            mutate(lag = 1:n()-1, Aux = unique(.x$Aux)) %>% 
            separate(Aux,c("Cadena","Variable"))) %>% 
  ggplot(aes(x=lag,y=acf,fill=Cadena)) + 
  geom_col() + 
  facet_grid(Variable~Cadena) + 
  scale_fill_manual(values = c(azul,"#2D9779","#F78D7C","#B8C2E9")) + 
  labs(title = "Autocorrelación de las cadenas después del calentamiento y espaciamiento") + 
  theme_minimal() + 
  lucify_basics() + 
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2),hjust = 0.9),
        panel.grid.minor = element_blank())

ggsave("Bayes/Ejemplos_Convergencia_Autocorr_Final.pdf",
       plot = autocorr_final, device = cairo_pdf, width = 22.5/2, height = 17/2.5)

histogramas <- datos_graf %>% 
  filter(Tipo == "Simulación", mod(n,6) %>% equals(0), n > 1200) %>% 
  gather(Variable,Valor,Var1,Var2) %>% 
  {bind_rows(.,mutate(.,Cadena="Todas juntas"))} %>% 
  {ggplot(data = ., aes(x=Valor,fill = Cadena, y = ..density..)) + 
      geom_histogram(binwidth = 0.15) + 
      facet_grid(Variable~Cadena, scales = "free_y") + 
      scale_fill_manual(values = c(azul,"#2D9779","#F78D7C","#B8C2E9","gray45")) + 
      labs(title = "Histogramas de las muestras por cadena y todas juntas") + 
      theme_minimal() + 
      lucify_basics() + 
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            panel.grid = element_blank())}

ggsave("Bayes/Ejemplos_Convergencia_Histogramas.pdf",
       plot = histogramas, device = cairo_pdf, width = 22.5/2, height = 17/2)


# Verificar diagn. Gelman
datos_graf %>% 
  split(.$Cadena) %>% 
  map(~ select(.x,Var1,Var2) %>% 
        as.matrix %>% 
        mcmc) %>% 
  as.mcmc.list %>% 
  gelman.diag() %>% 
  extract2("psrf") %>% 
  as.data.frame %>% 
  rownames_to_column(var="Var") %>% 
  as_tibble %>% 
  set_colnames(c("Variable","Estimado","Cota"))


