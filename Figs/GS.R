library(tidyverse)
library(magrittr)

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
  scale_fill_gradient(low = "transparent", high = "darkorange") + 
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
  scale_fill_gradient(low = "transparent", high = "darkorange") + 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

#### Ejemplo Base ####
set.seed(31122018)
Ejemplo_Est_Indep <- GS_Norm2(N = 1250,inicio = c(3,-3))
datos_graf <- data_frame(Var1 = Ejemplo_Est_Indep$Var1[-1], 
                         Var2 = Ejemplo_Est_Indep$Var2[-nrow(Ejemplo_Est_Indep)]) %>% 
  mutate(n = 1:n(), Tipo = "Paso intermedio") %>% 
  bind_rows(Ejemplo_Est_Indep) %>% 
  arrange(n)
ejemplo_est_indep_a <- graf_base + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n < 10, n >0), 
             color = "steelblue4", size = rel(4), shape = 18) + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n == 0), 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(datos_graf, n <= 10), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "Primeras iteraciones con pasos intermedios")

ggsave("Bayes/Ejemplo_GS_A.pdf",plot = ejemplo_est_indep_a, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_indep_b <- graf_base + 
  geom_point(data = filter(Ejemplo_Est_Indep, n >0), 
             color = "steelblue4", size = rel(1.25), shape = 18) + 
  geom_point(data = filter(Ejemplo_Est_Indep, n == 0), 
             color = "steelblue4", size = rel(1)) + 
  geom_path(data = Ejemplo_Est_Indep,
            color = "steelblue4", size = rel(0.2), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "1,250 iteraciones")

ggsave("Bayes/Ejemplo_GS_B.pdf",plot = ejemplo_est_indep_b, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_indep_c <- Ejemplo_Est_Indep %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 1250, y = 0, yend = 0, color = "darkorange", size = rel(1.5)) +
  geom_path(size = rel(1), color = "steelblue4") + 
  annotate("text",x = 50, y = c(3.15,-3.15), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 1300, y = 0, label = "Media real", size = rel(6), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos por variable") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,1500), breaks = seq(0,1250,by=250))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo_GS_C.pdf",plot = ejemplo_est_indep_c, device = cairo_pdf, width = 10, height = 8)

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
             color = "steelblue4", size = rel(4), shape = 18) + 
  geom_point(data = filter(datos_graf, Tipo == "Simulación", n == 0), 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(datos_graf, n <= 10), 
            color = "steelblue4", size = rel(0.75), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "Primeras 10 iteraciones de GS")

ggsave("Bayes/Ejemplo_GS_Compara1.pdf",plot = ejemplo_est_corr_a, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_corr_b <- graf_base_corr + 
  geom_point(data = Ejemplo_Est_Corr,
             color = "steelblue4", size = rel(0.75), shape = 18) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "1,250 transiciones de GS")

ggsave("Bayes/Ejemplo_GS_Compara2.pdf",plot = ejemplo_est_corr_b, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_corr_c <- Ejemplo_Est_Corr %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 1250, y = 0, yend = 0, color = "darkorange", size = rel(1.5)) +
  geom_path(size = rel(1), color = "steelblue4") + 
  annotate("text",x = 50, y = c(-2.75,2.75), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 1300, y = 0, label = "Media real", size = rel(6), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos para GS") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,1500), breaks = seq(0,1250,by=250))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo_GS_Compara3.pdf",plot = ejemplo_est_corr_c, device = cairo_pdf, width = 10, height = 8)
