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


