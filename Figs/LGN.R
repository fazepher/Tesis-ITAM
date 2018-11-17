
library(magrittr)
library(ggplot2)
library(dplyr)

N <- 1500
set.seed(30092018)
volados <- function(n_volados){
  
  data_frame(i = 1:n_volados, U = runif(n_volados)) %>% 
  mutate(Volado = if_else(U<=0.5,"Águila","Sol"),
         n_sol = Volado == "Sol",
         n_aguila = Volado == "Águila",
         p_sol = cummean(n_sol),
         p_aguila = 1-p_sol) %>% 
    return()
}


purrr::map_dfr(factor(1:5),~volados(N) %>% mutate(Cadena = .x)) %>% 
ggplot(aes(x=i,y=p_aguila,color=Cadena)) + 
  geom_hline(aes(yintercept = 0.5),color="gray5",size=1.5) + 
  geom_line(size=1) +
  scale_color_viridis_d() + 
  ylim(0,1) + 
  ylab("Proporción empírica de águila") + 
  xlab("Número de volados") + 
  labs(title = "Ley de los grandes números",
       subtitle = "Un ejemplo con volados", 
       caption = "@fazepher") + 
  theme_minimal() + 
  theme(legend.position = "none")
  
