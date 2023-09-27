
library(scales)
library(ggplot2)
library(directlabels)
library(GGally)
library(tidyverse)
library(gghighlight)
library(patchwork)

##### EMS ####

data_graph <- data_graph %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

data_graph %>%
  ggplot(aes(temps, tx_std, group = médicament)) +
  geom_point() +
  geom_line()+
  gghighlight(covid == "1", use_direct_label = F) +
  labs(x = "Mois", 
       y = "Taux standardisés mensuels de consommation", 
       shape = "Territoire de consommation")+
  annotate(geom = "rect", 
           xmin = "202003", 
           xmax = "202005", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), 
                     limits = c(0,50), breaks = seq(0,50, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face="bold"))+
  annotate(geom = "rect", 
           xmin = "202011", 
           xmax = "202012", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  ) +
  annotate(geom = "rect", 
           xmin = "202104", 
           xmax = "202105", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  )  +
  annotate(geom = "segment", 
           x = "202003", 
           xend = "202106", 
           y = 45, yend = 45, 
           col = alpha("steelblue", 0.7)) +
  annotate(geom = "segment", 
           x = "202003", 
           xend = "202003", 
           y = 45, yend = 40.5 , 
           col = alpha("steelblue", 0.7), 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "segment", 
           x = "202106", 
           xend = "202106", 
           y = 45, yend = 40.5 , 
           col = alpha("steelblue", 0.7), 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "text", 
           y = 45.7, 
           x = "202010", 
           label = "Etat d'urgence sanitaire", 
           col = "steelblue", 
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/2.5)+
  annotate(geom = "text", 
           y = 44.5, 
           x = "202010", 
           label = "24/03/2020 - 01/06/2021", 
           col = "steelblue", 
           fontface = "italic", 
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/2.5) +
  annotate(geom = "text", 
           x = "202005", 
           y = 49.7, 
           col = "firebrick",
           label = "Validité des prescriptions périmées", 
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/2.5)+
  annotate(geom = "segment", 
           x = "202003", 
           xend = "202006", 
           y = 49, yend = 49,
           col = alpha("firebrick", 0.5))+
  annotate(geom = "segment", 
           x = "202003", xend = "202003",
           y = 49, yend = 47.2,
           col = alpha("firebrick", 0.5),
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm")))+
  annotate(geom = "segment", 
           x = "202006", xend = "202006",
           y = 49, yend = 47.2,
           col = alpha("firebrick", 0.5),
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm")))+
  annotate(geom = "segment", 
           x = "202010", 
           xend = "202102", 
           y = 25, 
           yend = 25, 
           col = alpha("#2F4858", 0.5), linetype = 2
  )+
  annotate(geom = "segment", 
           x = "202010", 
           xend = "202010", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "segment", 
           x = "202102", 
           xend = "202102", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  geom_richtext(aes(x = "202112", y = 50, 
                    label = lab_md), 
                color ="grey9", 
                fill = alpha("lightgrey", 0.1),
                hjust = 0, vjust = 0, 
                family = "serif")+
  annotate(geom = "segment", 
           x = "202104", 
           xend = "202107", 
           y = 25, 
           yend = 25, 
           col = alpha("#2F4858", 0.5), linetype = 2
  )+
  annotate(geom = "segment", 
           x = "202104", 
           xend = "202104", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "segment", 
           x = "202107", 
           xend = "202107", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  geom_richtext(aes(x = "202112", y = 50, 
                    label = lab_md), 
                color ="grey9", 
                fill = alpha("lightgrey", 0.1),
                hjust = 0, vjust = 0, 
                family = "serif")+
  geom_richtext(aes(x = "202112", y = 25, 
                    label = lab_cf), color ="grey9", label.color = "#2F4858", 
                hjust = 0, vjust = 0, 
                family = "serif")+
  theme(legend.position="none")+
  directlabels::geom_dl(aes(label = médicament), method = list("last.points", fontfamily = "serif")) +
  theme(plot.margin = unit(c(1, 10, 1, 1), "lines")) +
  coord_cartesian(clip = "off") 
#### highlight EMS ####
data_graph_high <- data_std_mensuel_tx %>%
  select(temps, territoire,starts_with("tx_std")) %>%
  pivot_longer(!c(temps, territoire), names_to = "médicament", values_to = "tx_std")

data_graph_high$médicament <- as.factor(data_graph_high$médicament)
data_graph_high$médicament <- data_graph_high$médicament %>%
  fct_recode(
    "Antipsychotiques (N05A)" = "tx_std_N05A",
    "Anxiolytiques (N05B)" ="tx_std_N05B", 
    "Hypnotiques (N05C)" = "tx_std_N05C", 
    "Antidépresseurs (N06A)" = "tx_std_N06A"
  )

##### highlight EMS graph ####
anxio_mensu <- data_graph_high %>%
  filter(médicament == "Anxiolytiques (N05B)") %>%
  ggplot(aes(temps, tx_std, group = territoire)) +
  geom_point() +
  geom_line()+
  gghighlight(territoire == "Eurométropole de Strasbourg", use_direct_label = F)+
  labs(x = "Mois", 
       y = "Taux standardisés mensuels de consommation", 
       shape = "Territoire de consommation")+
  annotate(geom = "rect", 
           xmin = "202003", 
           xmax = "202005", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), 
                     limits = c(0,50), breaks = seq(0,50, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face="bold"))+
  annotate(geom = "rect", 
           xmin = "202011", 
           xmax = "202012", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  ) +
  annotate(geom = "rect", 
           xmin = "202104", 
           xmax = "202105", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  )  +
  geom_richtext(aes(x = "202112", y = 50, 
                    label = lab_md), 
                color ="grey9", 
                fill = alpha("lightgrey", 0.1),
                hjust = 0, vjust = 0, 
                family = "serif")+
  
  theme(legend.position="none")+
  theme(plot.margin = unit(c(1, 10, 1, 1), "lines")) +
  coord_cartesian(clip = "off") 

antidep_mensu <- data_graph_high %>%
  filter(médicament == "Antidépresseurs (N06A)") %>%
  ggplot(aes(temps, tx_std, group = territoire)) +
  geom_point() +
  geom_line()+
  gghighlight(territoire == "Eurométropole de Strasbourg", use_direct_label = F)+
  labs(x = "Mois", 
       y = "Taux standardisés mensuels de consommation", 
       shape = "Territoire de consommation")+
  annotate(geom = "rect", 
           xmin = "202003", 
           xmax = "202005", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), 
                     limits = c(0,50), breaks = seq(0,50, by = 5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face="bold"))+
  annotate(geom = "rect", 
           xmin = "202011", 
           xmax = "202012", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  ) +
  annotate(geom = "rect", 
           xmin = "202104", 
           xmax = "202105", 
           ymin = -Inf, ymax = Inf, 
           fill = alpha("lightgrey", 0.3
           )
  )  +
  annotate(geom = "segment", 
           x = "202003", 
           xend = "202106", 
           y = 45, yend = 45, 
           col = alpha("steelblue", 0.7)) +
  annotate(geom = "segment", 
           x = "202003", 
           xend = "202003", 
           y = 45, yend = 40.5 , 
           col = alpha("steelblue", 0.7), 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "segment", 
           x = "202106", 
           xend = "202106", 
           y = 45, yend = 40.5 , 
           col = alpha("steelblue", 0.7), 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "text", 
           y = 45.7, 
           x = "202010", 
           label = "Etat d'urgence sanitaire", 
           col = "steelblue", 
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/2.5)+
  annotate(geom = "text", 
           y = 44.5, 
           x = "202010", 
           label = "24/03/2020 - 01/06/2021", 
           col = "steelblue", 
           fontface = "italic", 
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/2.5) +
  annotate(geom = "text", 
           x = "202005", 
           y = 49.7, 
           col = "firebrick",
           label = "Validité des prescriptions périmées", 
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/2.5)+
  annotate(geom = "segment", 
           x = "202003", 
           xend = "202006", 
           y = 49, yend = 49,
           col = alpha("firebrick", 0.5))+
  annotate(geom = "segment", 
           x = "202003", xend = "202003",
           y = 49, yend = 47.2,
           col = alpha("firebrick", 0.5),
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm")))+
  annotate(geom = "segment", 
           x = "202006", xend = "202006",
           y = 49, yend = 47.2,
           col = alpha("firebrick", 0.5),
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm")))+
  annotate(geom = "segment", 
           x = "202010", 
           xend = "202102", 
           y = 25, 
           yend = 25, 
           col = alpha("#2F4858", 0.5), linetype = 2
  )+
  annotate(geom = "segment", 
           x = "202010", 
           xend = "202010", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "segment", 
           x = "202102", 
           xend = "202102", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  geom_richtext(aes(x = "202112", y = 50, 
                    label = lab_md), 
                color ="grey9", 
                fill = alpha("lightgrey", 0.1),
                hjust = 0, vjust = 0, 
                family = "serif")+
  annotate(geom = "segment", 
           x = "202104", 
           xend = "202107", 
           y = 25, 
           yend = 25, 
           col = alpha("#2F4858", 0.5), linetype = 2
  )+
  annotate(geom = "segment", 
           x = "202104", 
           xend = "202104", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  annotate(geom = "segment", 
           x = "202107", 
           xend = "202107", 
           y = 25, 
           yend = 23.5, 
           col = alpha("#2F4858", 0.5), linetype = 2, 
           arrow = arrow(
             type = "closed", 
             length = unit(.2,"cm"))) +
  geom_richtext(aes(x = "202112", y = 50, 
                    label = lab_md), 
                color ="grey9", 
                fill = alpha("lightgrey", 0.1),
                hjust = 0, vjust = 0, 
                family = "serif")+
  geom_richtext(aes(x = "202112", y = 25, 
                    label = lab_cf), color ="grey9", label.color = "#2F4858", 
                hjust = 0, vjust = 0, 
                family = "serif")+
  theme(legend.position="none")+
  theme(plot.margin = unit(c(1, 10, 1, 1), "lines")) +
  coord_cartesian(clip = "off") 

anxio_mensu / antidep_mensu + plot_layout(guides = "collect")

