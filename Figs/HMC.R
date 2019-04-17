library(tidyverse)
library(magrittr)

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
extrafont::loadfonts(device = "win")
theme_set(theme_minimal() + lucify_basics())
set.seed(51295)

distr_normal <- tibble(theta = seq(-3,3,by=0.05),
                       dens = dnorm(theta),
                       ener = -log(dens))
muestra_normal <- tibble(theta = rnorm(500))

muestra_normal_original <- ggplot(distr_normal, aes(x = theta)) + 
  geom_path(aes(y = dens), color = rosa, size = rel(2)) + 
  geom_rug(data = muestra_normal, color = azul) + 
  annotate("text",x=0,y=0.05,label= "Más muestra", size = rel(8)) + 
  annotate("text",x=-2.5,y=0.05,label= "Menos muestra", size = rel(4)) + 
  annotate("text",x=2.5,y=0.05,label= "Menos muestra", size = rel(4)) + 
  labs(title ="Densidad normal original", 
       subtitle = "Más muestra cerca de la moda y menos en las colas") + 
  xlab(expression(theta)) + 
  ylab(expression(f(theta))) + 
  theme(panel.grid = element_blank())

ggsave("Bayes/Muestra_Normal_Original.pdf",
       plot = muestra_normal_original, device = cairo_pdf, width = 20, height = 10)

muestra_normal_volteada <- ggplot(distr_normal, aes(x = theta)) + 
  geom_path(aes(y = ener), color = rosa, size = rel(2)) + 
  geom_rug(data = muestra_normal, color = azul) + 
  annotate("text",x=0,y=1.5,label= "Más muestra", size = rel(8)) + 
  annotate("text",x=-2.5,y=1.5,label= "Menos muestra", size = rel(4)) + 
  annotate("text",x=2.5,y=1.5,label= "Menos muestra", size = rel(4)) + 
  labs(title ="Densidad normal volteada", 
       subtitle = "Más muestra al fondo del tazón y menos en los bordes") + 
  xlab(expression(theta)) + 
  ylab(expression(-ln(f(theta)))) + 
  theme(panel.grid = element_blank())

ggsave("Bayes/Muestra_Normal_Volteada.pdf",
       plot = muestra_normal_volteada, device = cairo_pdf, width = 20, height = 10)

part_rodando_a <- ggplot(distr_normal, aes(x = theta, y = ener)) + 
  geom_path(color = rosa, size = rel(2)) + 
  geom_path(data = tibble(theta = 2.1*cos(seq(0,pi/2.25,by=0.05)),
                          ener = -log(dnorm(theta)) + 0.08),
            color = azul, size = rel(2), arrow = arrow()) + 
  geom_point(data = tibble(theta = 2.1*cos(0), ener = -log(dnorm(theta)) + 0.08),
             color = azul, size = rel(5)) + 
  labs(title ="Sistema físico ficticio", 
       subtitle = "Una partícula con momentum rodaría siguiendo las leyes de la física") + 
  xlab(expression(theta)) + 
  ylab(expression(-ln(f(theta)))) + theme(panel.grid = element_blank())

ggsave("Bayes/Part_Rodando_A.pdf",
       plot = part_rodando_a, device = cairo_pdf, width = 20, height = 10)

part_rodando_b <- ggplot(distr_normal, aes(x = theta, y = ener)) + 
  geom_path(color = rosa, size = rel(2)) + 
  geom_path(data = tibble(theta = 2.1*cos(seq(pi/2,0,by=-0.05)),
                          ener = -log(dnorm(theta)) + 0.08),
            color = azul, size = rel(1.75), arrow = arrow()) + 
  geom_path(data = tibble(theta = 2.1*cos(seq(pi/2,pi,by=0.05)),
                          ener = -log(dnorm(theta)) + 0.08),
            color = azul, size = rel(1.75), arrow = arrow()) + 
  geom_point(data = tibble(theta = 2.1*cos(pi/2.25), ener = -log(dnorm(theta)) + 0.08),
             color = azul, size = rel(5)) + 
  annotate("text",x=1.5,y=3.25,label= "Más energía potencial", size = rel(6),color="gray25") + 
  annotate("text",x=-1.5,y=3.25,label= "Más energía potencial", size = rel(6),color="gray25") + 
  annotate("text",x=0,y=1.5,label= "Más energía cinética", size = rel(6),color="gray25") + 
  labs(title ="Sistema sin fricción", 
       subtitle = "La partícula subiría y regresaría constantemente") + 
  xlab(expression(theta)) + 
  ylab(expression(-ln(f(theta)))) + 
  theme(panel.grid = element_blank())

ggsave("Bayes/Part_Rodando_B.pdf",
       plot = part_rodando_b, device = cairo_pdf, width = 20, height = 10)


datos_espacio_fase <- map_dfr(seq(0.3,3,length.out = 10), ~ 
          tibble(t = seq(0,2*pi,length.out = 50),
                 r = .x,
                 theta = r*cos(t),
                 m = -r*sin(t),
                 dens_theta = dnorm(theta),
                 ener_pot = -log(dens_theta),
                 dens_m = dnorm(m),
                 ener_cin = -log(dens_m),
                 hamilton = ener_pot + ener_cin)) 

distr_conjunta <- expand.grid(seq(-3,3,by = 0.05),seq(-3,3,by = 0.05)) %>% 
  as_tibble %>% 
  rename(theta = Var1, m = Var2) %>% 
  {mutate(., 
          Dens_theta = dnorm(theta),
          Ener_Pot = -log(Dens_theta),
          Dens_m = dnorm(m),
          Ener_Cin = -log(Dens_m),
          Dens_Fase = mvtnorm::dmvnorm(., mean = c(0,0), sigma = matrix(c(1,0,0,1), nrow=2)),
          Hamiltoniano = Ener_Pot + Ener_Cin)}

trayectoria <- datos_espacio_fase %>% 
  ggplot(aes(x=theta,y=m)) + 
  geom_raster(data = distr_conjunta, aes(fill=Hamiltoniano)) + 
  geom_path(aes(group=r),color = rgb(red = 60, green = 60, blue = 60, maxColorValue = 255)) + 
  geom_path(data = tibble(t = seq(0,pi/2.25,by=0.05),
                          theta = 2.1*cos(t),
                          m = -2.1*sin(t)),
            color = azul, size = rel(2), arrow = arrow()) + 
  geom_point(data = tibble(theta = 2.1*cos(0), m = -2.1*sin(0)),
             color = azul, size = rel(5)) + 
  scale_fill_gradient(low = rosa, high = "transparent") + 
  labs(title ="Trayectoria en el espacio de fases", 
       subtitle = "Las curvas de nivel de la distribución canónica representan trayectorias con energía constante") + 
  xlab(expression(theta)) + 
  ylab(expression(m)) + 
  theme(panel.grid = element_blank())

ggsave("Bayes/Trayectoria_Espacio_Fase.pdf",
       plot = trayectoria, device = cairo_pdf, width = 15, height = 10)



  
  

