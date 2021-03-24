# Manipulação de dados
library(tidyverse)

# Manipulação de Séries Temporais
library(tsibble)

# Funções de Previsão
library(fable)

# Gráficos e Estatísticas de Séries Temporais
library(feasts)

# Séries Temporais Tidy
library(tsibbledata)

# Todos os itens acima e mais
library(fpp3)

#----

# Um tsibble permite o armazenamento e manipulação de múltiplas séries temporais em R
# Ele contêm: um index (informação de tempo), variáveis medidas e variáveis chave (identificadores únicos opcionais para cada série)

global_economy
tourism

# lendo um arquivo csv e convertendo para tsibble

covid = readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
covid

covid = readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  select(date, state, newDeaths, newCases)%>% 
  as_tsibble(
    index = date,
    key = state
  ) %>% 
  group_by(state) %>% 
  mutate(MM_mortes = zoo::rollmean(newDeaths, k = 7, fill = NA, align = "right"),
         MM_casos = zoo::rollmean(newCases, k = 7, fill = NA, align = "right"))
covid

# plotando com o ggplot2

covid %>% 
  filter(state == "TOTAL") %>% 
  autoplot(MM_mortes) +
  labs(x="Dia",y="Mortes",title="Média Móvel (7 dias) do número de mortes por COVID-19 no Brasil")

covid %>% 
  filter(state != "TOTAL") %>% 
  autoplot(MM_mortes) +
  labs(x="Dia",y="Mortes (MM 7 dias)")

covid %>% 
  filter(state != "TOTAL") %>% 
  autoplot(MM_mortes) +
  facet_wrap(~state, scales = "free") +
  labs(x="Dia",y="Mortes (MM 7 dias)")

covid %>% 
  filter(state == "TOTAL") %>% 
  gg_season(MM_mortes) +
  labs(x="Dia",y="Mortes (MM 7 dias)")

covid %>% 
  filter(state == "TOTAL") %>% 
  gg_season(MM_mortes, period = "month")  +
  labs(x="Dia",y="Mortes (MM 7 dias)")

# Componentes de uma ST
# Tendência: quando há um aumento ou diminuição de longo prazo nos dados
# Sazonalidade: quando uma série é influenciada por fatores sazonais (por exemplo, o trimestre de o ano, o mês ou o dia da semana)
# Ciclo: quando a série apresenta padrões que não estão fixos no tempo

# Sazonalidade vs Ciclo: O momento de picos e depressões é previsível com dados sazonais, mas imprevisível a longo prazo com dados cíclicos

covid %>% 
  filter(state == "TOTAL") %>% 
  autoplot(newCases) +
  labs(x="Dia",y="Casos",title="Número de casos de COVID-19 por dia no Brasil")

# Função de autocorrelação (ou ACF): correlação do instante t com t-k

covid %>% 
  filter(state == "TOTAL") %>% 
  ACF(newCases, lag_max = 50)


covid %>% 
  filter(state == "TOTAL") %>% 
  ACF(newCases, lag_max = 50) %>% 
  autoplot()

# Componentes da ST na ACF
# Presença de tendência = as autocorrelações para pequenos lags tendem a ser grandes e positivas
# Presença de sazonalidade = as autocorrelações serão maiores em lags múltiplos da frequência sazonal
# Tendência + Sazonalidade = combinação desses dois efeitos

# Ruído Branco: dados não estão correlacionados ao longo do tempo, possuem média zero e variância constante
# (Tecnicamente, também exigimos independência)

rb = tsibble(t = seq(36), y = rnorm(36), index = t)
rb %>% autoplot(y)
rb %>% ACF(y) %>% autoplot()

# Transformações

aus_retail

food = aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

food %>% 
  autoplot()

food %>% 
  autoplot(sqrt(Turnover)) +
  labs(y = "Raiz Quadrada do turnover")

food %>% 
  autoplot(Turnover^(1/3)) +
  labs(y = "Raiz Cúbica do turnover")

food %>% 
  autoplot(log(Turnover)) +
  labs(y = "Log do turnover")

food %>% 
  autoplot(-1/Turnover) +
  labs(y = "Inversa do turnover")

# Transformação Box-Cox
# log(yt) se lambda = 0
# (yt^lambda-1)/lambda se lambda != 0

#lambda = 1 --> propria série

food %>% 
  autoplot(box_cox(Turnover, 1)) +
  labs(y = "Transformação Box-Cox do turnover")

# melhor lambda?

food %>% 
  features(Turnover, features = guerrero)

food %>% 
  autoplot(box_cox(Turnover, 0.0524)) +
  labs(y = "Transformação Box-Cox do turnover")

# Sempre verifique os resultados
# Valores muito baixos de lambda resultam em intervalos de previsão extremamente grandes
# As transformações devem ser revertidas para obter previsões na escala original
# (fable automaticamente já lida com isso)

# Decomposição

# ADITIVA: Yt = Tt + St + Rt
# MULTIPLICATIVA: Yt = Tt*St*Rt
# Yt: ST
# Tt: componente de tendência e ciclo
# St: componente sazonal
# Rt: resto (remainder)

# Seasonal and Trend decomposition using Loess (STL)
# versátil e robusta para outliers (se desejável)
# formulação apenas aditiva
# usar box-cox para outras formulações

vic_elec %>% 
  model(STL(Demand)) %>% 
  components()

vic_elec %>% 
  model(STL(Demand)) %>% 
  components() %>% 
  autoplot()

# Ajuste Sazonal
# Aditiva: Yt-St=Tt+Rt
# Multiplicativa: Yt/St=Tt*Rt

vic_elec_mes = vic_elec %>% 
  group_by_key() %>%
  index_by(date = ~yearmonth(.)) %>% 
  summarise(Demand = sum(Demand))

vic_elec_mes

dcmp = vic_elec_mes %>% 
  model(STL(Demand)) %>% 
  components()

vic_elec %>%
  autoplot(Demand, col = "gray") +
  autolayer(dcmp, season_adjust, color = "blue")

# "Força" Tendência = max(0, 1 - Var(Rt)/Var(Tt+Rt))
# "Força" Sazonal = max(0, 1 - Var(Rt)/Var(St+Rt))

tourism %>% 
  features(Trips, feat_stl)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Purpose)) +
  geom_point() + 
  facet_wrap(vars(State))

# Feriados (Holidays) são mais sazonais
# Western Australia possui as maiores tendências

# Todas as features do pacote {feasts}
tourism_features = tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))

# PREVISÃO ESTATÍSTICA
# y{T+h} --> coisa a ser prevista
# y{1},...,y{T} --> o que conhecemos/histórico
# yhat{T+h|T} = E[y{T+h}|y{1},...,y{T}] --> previsão pontual

# BENCHMARKS
# MEAN(y): previsões são iguais a média histórica
# NAIVE(y): previsões são iguais ao último valor observado
# SNAIVE(y~lag(m)): previsões iguais ao último valor do mesmo período
# RW(y~drift()): previsões iguais ao último valor mais variação média

# a função model() treina o modelo nos dados

covid_fit = covid %>% 
  filter(state == "TOTAL") %>% 
  model(
    Seasonal_naive = SNAIVE(newCases),
    Naive = NAIVE(newCases),
    Drift = RW(newCases ~ drift()),
    Mean = MEAN(newCases)
  )
covid_fit

# para produzir as previsões use a função forecast()

covid_fc = covid_fit %>% 
  forecast(h = 12)
covid_fc

# fable é uma tabela de previsão com previsões pontuais e distribuições

covid_fc %>% 
  autoplot(covid, level = NULL)

# Residuos: et = y{t}-haty{t|t-1}
# Premissas:
# {et} não são correlacionados, caso sejam: ficaram informações nos resíduos que deveriam estar no modelo
# {et} possui média zero, caso não seja então as previsões são viesadas

augment(covid_fit)

augment(covid_fit) %>%
  filter(.model == "Seasonal_naive") %>% 
  autoplot(.resid)

augment(covid_fit) %>%
  filter(.model == "Seasonal_naive") %>% 
  ACF(.resid) %>%
  autoplot()

# Teste de Ljung-box
# H0: os resíduos são iid
# não quero rejeitar H0 (pvalor grande)

augment(covid_fit) %>%
  features(.resid, ljung_box)

# Medidas de acurácia

covid_fit = covid %>% 
  filter(state == "TOTAL",
         date <= "2020-12-31") %>% 
  model(Seasonal_naive = SNAIVE(newCases))

covid_fit %>% 
  forecast(h = 83) %>% 
  autoplot(covid %>% 
             filter(state == "TOTAL"), level = NULL)

#MAE = mean(|eT+h|)
#MSE = mean(e^2T+h)
#MAPE = 100mean(|eT+h|/|yT+h|)
#RMSE = sqrt(mean(e^2{T+h}))

#MAE,MSE e RMSE são dependentes da escala dos dados
#MAPE é independente da escala, porém só é sensível

covid_fc = covid_fit %>% forecast(h = 83)
accuracy(covid_fc, covid %>% 
           filter(state == "TOTAL"))

# EXPONENTIAL SMOOTHING

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

covid_RJ = covid %>% 
  filter(state == "RJ") %>% 
  select(newDeaths)

covid_RJ %>% autoplot()

fit = covid_RJ %>%
  model(ets = ETS(newDeaths))

fit
report(fit)
components(fit)

components(fit) %>% autoplot()

fit %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ)

covid_RJ %>%
  model(ets = ETS(newDeaths ~ trend("Ad"))) %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ)

# Bootstrap

sim = fit %>% 
  generate(h = 14, times = 5, bootstrap = TRUE)
sim

covid_RJ %>% 
  filter(year(date) >= 2021) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = newDeaths)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  guides(col = FALSE)

# ARIMA

# AR: autoregressivo (observações defasadas como input)
#  I: integrado (diferenciação para tornar a série estacionária)
# MA: média móvel (erros defasados como input)

fit_ARIMA = covid_RJ %>%
  model(arima = ARIMA(newDeaths))

fit_ARIMA
report(fit_ARIMA)

fit_ARIMA %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ)

covid_RJ %>%
  model(arima = ARIMA(newDeaths ~ pdq(1,1,1)+PDQ(1,1,1))) %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ)

covid_RJ %>% 
  filter(year(date) >= 2021) %>% 
  model(ets = ETS(newDeaths),
        arima = ARIMA(newDeaths)) %>% 
  forecast(h = 14) %>% 
  autoplot(covid_RJ %>% filter(year(date) >= 2021))
