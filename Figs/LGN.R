
library(magrittr)
library(ggplot2)
library(dplyr)

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

N <- 1000
volados <- function(n_volados){
  
  data_frame(i = 1:n_volados, U = runif(n_volados)) %>% 
  mutate(Volado = if_else(U<=0.5,"Águila","Sol"),
         n_sol = Volado == "Sol",
         n_aguila = Volado == "Águila",
         p_sol = cummean(n_sol),
         p_aguila = 1-p_sol) %>% 
    return()
}

set.seed(51295)
lgn <- purrr::map_dfr(factor(1:4),~volados(N) %>% mutate(Cadena = .x)) %>% 
  ggplot(aes(x=i,y=p_aguila,color=Cadena)) + 
  geom_hline(aes(yintercept = 0.5),color="gray5",size=1.5) + 
  geom_line(size=0.6) +
  scale_color_manual(values = c("#4E67C8","#2D9779","#F78D7C","#B8C2E9","gray45")) + 
  ylim(0,1) + 
  ylab("Proporción empírica de águila") + 
  xlab("Número de volados") + 
  labs(title = "Ley de los grandes números",
       subtitle = "Un ejemplo con volados") + 
  theme_minimal() + 
  lucify_basics() + 
  theme(legend.position = "none")
  
ggsave("Bayes/LGN.pdf",
       plot = lgn, device = cairo_pdf, width = 22.5/3, height = 17/3)
  
  
