
library(magrittr)
library(ggplot2)
library(dplyr)

interceptos_variables <- ggplot(data = data_frame(x = 0:10, y = 0:10)) + 
  geom_segment(x = 0, xend = 0, y = 0, yend = 10, color = gris_nk_escala, size = 1) +
  geom_segment(x = 0, xend = 5, y = 0, yend = 0, color = gris_nk_escala, size = 1) +
  geom_segment(x = 0, xend = 5, y = 1, yend = 1+5*.3, color = PALETA_TESIS_FN$COLOR[7], size = 1) +
  geom_segment(x = 0, xend = 5, y = 3, yend = 3+5*.3, color = PALETA_TESIS_FN$COLOR[2], size = 1) +
  geom_segment(x = 0, xend = 5, y = 5, yend = 5+5*.3, color = PALETA_TESIS_FN$COLOR[3], size = 1) +
  geom_segment(x = 0, xend = 5, y = 7, yend = 7+5*.3, color = PALETA_TESIS_FN$COLOR[4], size = 1) +
  xlim(c(0,5)) + 
  ylim(c(0,10)) + 
  labs(title = "Interceptos Variables") + 
  theme_classic() + 
  lucify_basics() + 
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 20),
        title = element_text(size = 30))

pendientes_variables <- ggplot(data = data_frame(x = 0:10, y = 0:10)) + 
  geom_segment(x = 0, xend = 0, y = 0, yend = 10, color = gris_nk_escala, size = 1) +
  geom_segment(x = 0, xend = 5, y = 0, yend = 0, color = gris_nk_escala, size = 1) +
  geom_segment(x = 0, xend = 5, y = 4, yend = 4+5*-.3, color = PALETA_TESIS_FN$COLOR[7], size = 1) +
  geom_segment(x = 0, xend = 5, y = 4, yend = 4+5*0.1, color = PALETA_TESIS_FN$COLOR[2], size = 1) +
  geom_segment(x = 0, xend = 5, y = 4, yend = 4+5*.5, color = PALETA_TESIS_FN$COLOR[3], size = 1) +
  geom_segment(x = 0, xend = 5, y = 4, yend = 4+5*.9, color = PALETA_TESIS_FN$COLOR[4], size = 1) +
  xlim(c(0,5)) + 
  ylim(c(0,10)) + 
  labs(title = "Pendientes Variables") + 
  theme_classic()  + 
  lucify_basics() + 
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 20),
        title = element_text(size = 30))

pend_int_variables <- ggplot(data = data_frame(x = 0:10, y = 0:10)) + 
  geom_segment(x = 0, xend = 0, y = 0, yend = 10, color = gris_nk_escala, size = 1) +
  geom_segment(x = 0, xend = 5, y = 0, yend = 0, color = gris_nk_escala, size = 1) +
  geom_segment(x = 0, xend = 5, y = 4, yend = 4+5*-.3, color = PALETA_TESIS_FN$COLOR[7], size = 1) +
  geom_segment(x = 0, xend = 5, y = 2, yend = 4+5*0.1, color = PALETA_TESIS_FN$COLOR[2], size = 1) +
  geom_segment(x = 0, xend = 5, y = 1, yend = 4+5*.5, color = PALETA_TESIS_FN$COLOR[3], size = 1) +
  geom_segment(x = 0, xend = 5, y = 0, yend = 4+5*.9, color = PALETA_TESIS_FN$COLOR[4], size = 1) +
  xlim(c(0,5)) + 
  ylim(c(0,10)) + 
  labs(title = "Pendientes e Interceptos Variables") + 
  theme_classic() + 
  lucify_basics() + 
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 20),
        title = element_text(size = 30))


ggsave(filename = "Interceptos_Variables.pdf", plot = interceptos_variables, 
       device = cairo_pdf, width = 15, height = 11) 
ggsave(filename = "Pendientes_Variables.pdf", plot = pendientes_variables, 
       device = cairo_pdf, width = 15, height = 11) 
ggsave(filename = "Pend_e_Inter_Variables.pdf", plot = pend_int_variables, 
       device = cairo_pdf, width = 15, height = 11) 
