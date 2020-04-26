library(readr)
library(ggplot2)
library(dplyr)

casosPeloMundo <- read_csv("datasets/confirmed_cases_worldwide.csv")
casosPeloMundo

ggplot(casosPeloMundo, aes(date,cum_cases))+
  ylab("Casos confirmados") +
  xlab("Data dos registros") +
  geom_line()

casosChinaMundo <- read_csv("datasets/confirmed_cases_china_vs_world.csv")

casosChinaMundo

plotCasosChinaCumulativos <- ggplot(casosChinaMundo) +
  geom_line(aes(date, cum_cases, color = is_china, group = is_china))+
  ylab("Casos confirmados cumulativamente")+
  xlab("Data")

plotCasosChinaCumulativos

eventos_oms <- tribble(~date, ~event,
                       "2020-01-30", "Emergência na saúde \nglobal declarada",
                       "2020-03-11", "Pandemia \nanunciada",
                       "2020-02-13", "\n\n\n\n\nChina relata \na mudança na contagem"
) %>%
  mutate(date = as.Date(date))

eventos_oms

plotCasosChinaCumulativos +
  geom_vline(aes(xintercept = date),
             data = eventos_oms,
             linetype = "dashed") +
  geom_text(aes(date, label = event), data = eventos_oms, y = 100000)

chinaDepois15Fevereiro <- casosConfirmadosChina %>%
  filter(is_china == "China", date >= "2020-02-15")

casosForaDaChina <- casosConfirmadosChina %>%
  filter(is_china == "Not China", date >= "2020-02-15")

plotMundoTrendLine <- ggplot(casosForaDaChina, aes(date, cum_cases))+
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)+
  ylab("Casos confirmados cumulativamente")+
  scale_y_log10()

plotMundoTrendLine
