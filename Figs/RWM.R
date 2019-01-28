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


calcula_alpha_Norm2 <- function(anterior,propuesta,media,matriz_varianzas){
  
  densidad_propuesta <- mvtnorm::dmvnorm(propuesta, mean = media, sigma = matriz_varianzas)
  densidad_anterior <- mvtnorm::dmvnorm(anterior, mean = media, sigma = matriz_varianzas)
  
  alpha <- min(c(1,densidad_propuesta/densidad_anterior))
  return(alpha)
}


RWM_Norm2_KTunif <- function(N, inicio, r, 
                             media = c(0,0), matriz_varianzas = matrix(c(1,0,0,1), nrow=2)){
  
  #Creamos data frames
  Propuestas <- data_frame(Var1 = rep(NA_real_,N+1), Var2 = rep(NA_real_,N+1))
  Propuestas[1,] <- inicio # propuesta inicial
  Simulaciones <- Propuestas
  Alphas <- rep(NA_real_,N+1)
  Uniformes <- runif(N+1)
  
  
  #Simulaciones
  for(n in 2:{N+1}){
    # Kernel de propuestas
    Propuestas[n,] <- Simulaciones[n-1,] + runif(2,min = -r, max = r)
    # Corrección de Metropolis
    Alphas[n] <- calcula_alpha_Norm2(anterior = Simulaciones[n-1,],
                                     propuesta = Propuestas[n,],
                                     media = media,matriz_varianzas = matriz_varianzas)
    #print(Alphas[n])
    if(Uniformes[n] <= Alphas[n]){
      #print("Aceptada")
      Simulaciones[n,] <- Propuestas[n,]
    } else{
      #print("Rechazada")
      Simulaciones[n,] <- Simulaciones[n-1,]
    }
    #print(Simulaciones[n,])
    
  }
  
  return(list(Propuestas = mutate(Propuestas, n = 0:N, Tipo = "Propuesta"), 
              Simulaciones = mutate(Simulaciones, n = 0:N, Tipo = "Simulación")))
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
Ejemplo_Est_Indep <- RWM_Norm2_KTunif(N = 2500,inicio = c(3,-3),r = 1.5)
n_rechazadas <- Ejemplo_Est_Indep$Simulaciones %>% 
  filter(equals(Var1,lag(Var1)),
         equals(Var2,lag(Var2))) %>% 
  extract2("n") 
Ejemplo_Est_Indep %<>% 
  map(~ mutate(.x,Rechazada = n %in% n_rechazadas))

ejemplo_est_indep_a <- graf_base + 
  geom_point(data = filter(Ejemplo_Est_Indep$Propuestas,Rechazada, n <= 10), 
             color = "steelblue4", size = rel(5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Indep$Simulaciones[1,], 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(Ejemplo_Est_Indep$Simulaciones,not(Rechazada), n <= 10), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "Primeras iteraciones")

ggsave("Bayes/Ejemplo_RWM_A.pdf",plot = ejemplo_est_indep_a, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_indep_b <- graf_base + 
  geom_point(data = filter(Ejemplo_Est_Indep$Propuestas,Rechazada), 
             color = "steelblue4", size = rel(1.5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Indep$Simulaciones[1,], 
             color = "steelblue4", size = rel(1)) + 
  geom_path(data = filter(Ejemplo_Est_Indep$Simulaciones,not(Rechazada)), 
            color = "steelblue4", size = rel(0.2)) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "2,500 iteraciones")

ggsave("Bayes/Ejemplo_RWM_B.pdf",plot = ejemplo_est_indep_b, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_indep_c <- Ejemplo_Est_Indep$Simulaciones %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 2500, y = 0, yend = 0, color = "darkorange", size = rel(1.5)) +
  geom_path(size = rel(1), color = "steelblue4") + 
  annotate("text",x = 110, y = c(3.15,-3.15), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 2550, y = 0, label = "Media real", size = rel(6), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos por variable") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,3000), breaks = seq(0,2500,by=500))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo_RWM_C.pdf",plot = ejemplo_est_indep_c, device = cairo_pdf, width = 10, height = 8)

#### Ejemplo Lento ####
set.seed(31122018)
Ejemplo_Est_Indep <- RWM_Norm2_KTunif(N = 2500,inicio = c(3,-3),r = 0.25)
n_rechazadas <- Ejemplo_Est_Indep$Simulaciones %>% 
  filter(equals(Var1,lag(Var1)),
         equals(Var2,lag(Var2))) %>% 
  extract2("n") 
Ejemplo_Est_Indep %<>% 
  map(~ mutate(.x,Rechazada = n %in% n_rechazadas))

ejemplo_est_indep_a <- graf_base + 
  geom_point(data = filter(Ejemplo_Est_Indep$Propuestas,Rechazada, n <= 10), 
             color = "steelblue4", size = rel(5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Indep$Simulaciones[1,], 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(Ejemplo_Est_Indep$Simulaciones,not(Rechazada), n <= 10), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "Primeras iteraciones")

ggsave("Bayes/Ejemplo2_RWM_A.pdf",plot = ejemplo_est_indep_a, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_indep_b <- graf_base + 
  geom_point(data = filter(Ejemplo_Est_Indep$Propuestas,Rechazada), 
             color = "steelblue4", size = rel(1.5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Indep$Simulaciones[1,], 
             color = "steelblue4", size = rel(1)) + 
  geom_path(data = filter(Ejemplo_Est_Indep$Simulaciones,not(Rechazada)), 
            color = "steelblue4", size = rel(0.2)) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "2,500 iteraciones")

ggsave("Bayes/Ejemplo2_RWM_B.pdf",plot = ejemplo_est_indep_b, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_indep_c <- Ejemplo_Est_Indep$Simulaciones %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 2500, y = 0, yend = 0, color = "darkorange", size = rel(1.5)) +
  geom_path(size = rel(1), color = "steelblue4") + 
  annotate("text",x = 110, y = c(3.15,-3.15), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 2550, y = 0, label = "Media real", size = rel(6), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos por variable") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,3000), breaks = seq(0,2500,by=500))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo2_RWM_C.pdf",plot = ejemplo_est_indep_c, device = cairo_pdf, width = 10, height = 8)


#### Ejemplo con correlación ####
set.seed(31122018)
r=0.25
Ejemplo_Est_Corr <- RWM_Norm2_KTunif(N = 2500,inicio = c(-2.5,2.5), r = r,
                                     matriz_varianzas = matrix(c(1,0.9,0.9,1), nrow = 2))
n_rechazadas <- Ejemplo_Est_Corr$Simulaciones %>% 
  filter(equals(Var1,lag(Var1)),
         equals(Var2,lag(Var2))) %>% 
  extract2("n") 
Ejemplo_Est_Corr %<>% 
  map(~ mutate(.x,Rechazada = n %in% n_rechazadas))

aux <- filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 30) %>% 
  top_n(1,n) %>% 
  select(Var1,Var2)
ejemplo_est_corr_compara <- graf_base_corr + 
  geom_point(data = filter(Ejemplo_Est_Corr$Propuestas,Rechazada, n <= 30), 
             color = "steelblue4", size = rel(5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Corr$Simulaciones[1,], 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 30), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  annotate("rect", xmin = aux$Var1 - r, xmax = aux$Var1 + r, ymin = aux$Var2 - r, ymax = aux$Var2 + r,
           color = "steelblue4", alpha = 0.2) +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  labs(title = "Rectángulo de propuestas estrecho de lado 0.5")

ggsave("Bayes/Ejemplo_RWM_Compara1.pdf",plot = ejemplo_est_corr_compara, device = cairo_pdf, width = 10, height = 8)

set.seed(31122018)
r = 1.5
Ejemplo_Est_Corr <- RWM_Norm2_KTunif(N = 2500,inicio = c(-2.5,2.5),r = r,
                                     matriz_varianzas = matrix(c(1,0.95,0.95,1), nrow = 2))
n_rechazadas <- Ejemplo_Est_Corr$Simulaciones %>% 
  filter(equals(Var1,lag(Var1)),
         equals(Var2,lag(Var2))) %>% 
  extract2("n") 
Ejemplo_Est_Corr %<>% 
  map(~ mutate(.x,Rechazada = n %in% n_rechazadas))

aux <- filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 30) %>% 
  top_n(1,n) %>% 
  select(Var1,Var2)
ejemplo_est_corr_compara <- graf_base_corr + 
  geom_point(data = filter(Ejemplo_Est_Corr$Propuestas,Rechazada, n <= 30), 
             color = "steelblue4", size = rel(5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Corr$Simulaciones[1,], 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 30), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  annotate("rect", xmin = aux$Var1 - r, xmax = aux$Var1 + r, ymin = aux$Var2 - r, ymax = aux$Var2 + r,
           color = "steelblue4", alpha = 0.2) +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  labs(title = "Rectángulo de propuestas intermedio de lado 3")

ggsave("Bayes/Ejemplo_RWM_Compara2.pdf",plot = ejemplo_est_corr_compara, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_corr_a <- graf_base_corr + 
  geom_point(data = filter(Ejemplo_Est_Corr$Propuestas,Rechazada, n <= 10), 
             color = "steelblue4", size = rel(5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Corr$Simulaciones[1,], 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 10), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  scale_x_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  labs(title = "Primeras 10 iteraciones de RWM")

ggsave("Bayes/Ejemplo_RWM_Compara2A.pdf",plot = ejemplo_est_corr_a, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_corr_b <- graf_base_corr + 
  geom_point(data = filter(Ejemplo_Est_Corr$Simulaciones, !Rechazada, n <= 1250), 
             color = "steelblue4", size = rel(0.75), shape = 18) + 
  geom_jitter(data = filter(Ejemplo_Est_Corr$Simulaciones, Rechazada, n <= 1250), 
              color = "steelblue4", size = rel(0.75), shape = 18, width = 0.02, height = 0.02) + 
  scale_x_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  labs(title = "1,250 iteraciones de RWM")

ggsave("Bayes/Ejemplo_RWM_Compara2B.pdf",plot = ejemplo_est_corr_b, device = cairo_pdf, width = 10, height = 8)

ejemplo_est_corr_c <- Ejemplo_Est_Corr$Simulaciones %>% 
  filter(n <= 1250) %>% 
  mutate_if(is.double,funs(Media = cummean(.))) %>% 
  select(n,ends_with("Media")) %>% 
  gather(Variable,Media,-n) %>% 
  separate(Variable,c("Variable","Aux")) %>% 
  ggplot(aes(x=n,y=Media,group=Variable)) + 
  geom_segment(x = 0, xend = 1250, y = 0, yend = 0, color = "darkorange", size = rel(1.5)) +
  geom_path(size = rel(1), color = "steelblue4") + 
  annotate("text",x = 50, y = c(-2.75,2.75), label = c("Var1","Var2"), size = rel(6)) + 
  annotate("text",x = 1300, y = 0, label = "Media real", size = rel(6), hjust = 0, vjust = 0.5) + 
  labs(title = "Promedios Ergódicos para RWM") + 
  ylab("Promedio ergódico") + 
  scale_x_continuous(limits = c(0,1500), breaks = seq(0,1250,by=250))+ 
  scale_y_continuous(limits = c(-4.5,4.5), breaks = seq(-3,3,by=1))+ 
  theme_minimal() + 
  lucify_basics() + 
  theme(panel.grid = element_blank())  

ggsave("Bayes/Ejemplo_RWM_Compara2C.pdf",plot = ejemplo_est_corr_c, device = cairo_pdf, width = 10, height = 8)

set.seed(31122018)
r=2.5
Ejemplo_Est_Corr <- RWM_Norm2_KTunif(N = 2500,inicio = c(-2.5,2.5),r = r,
                                     matriz_varianzas = matrix(c(1,0.95,0.95,1), nrow = 2))
n_rechazadas <- Ejemplo_Est_Corr$Simulaciones %>% 
  filter(equals(Var1,lag(Var1)),
         equals(Var2,lag(Var2))) %>% 
  extract2("n") 
Ejemplo_Est_Corr %<>% 
  map(~ mutate(.x,Rechazada = n %in% n_rechazadas))

aux <- filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 30) %>% 
  top_n(1,n) %>% 
  select(Var1,Var2)
ejemplo_est_corr_compara <- graf_base_corr + 
  geom_point(data = filter(Ejemplo_Est_Corr$Propuestas,Rechazada, n <= 30), 
             color = "steelblue4", size = rel(5), shape = 88) + 
  geom_point(data = Ejemplo_Est_Corr$Simulaciones[1,], 
             color = "steelblue4", size = rel(2)) + 
  geom_path(data = filter(Ejemplo_Est_Corr$Simulaciones,not(Rechazada), n <= 30), 
            color = "steelblue4", size = rel(1), arrow = arrow(angle = 40, length = unit(0.15, "inches"))) + 
  annotate("rect", xmin = aux$Var1 - r, xmax = aux$Var1 + r, ymin = aux$Var2 - r, ymax = aux$Var2 + r,
           color = "steelblue4", alpha = 0.2) +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,by=2))+ 
  labs(title = "Rectángulo de propuestas amplio de lado 5")

ggsave("Bayes/Ejemplo_RWM_Compara3.pdf",plot = ejemplo_est_corr_compara, device = cairo_pdf, width = 10, height = 8)
