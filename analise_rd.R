library(stats)
library(dplyr)
library(readxl)
library(stringr)
library(forecast)
library(tidyr)
library(FinTS)
library(caret)
library(pROC)
library(ggplot2)
library(MASS)
library(lubridate)
library(corrplot)
library(patchwork)
library(statmod)
library(seasonal)
library(tsibble)
library(fable)
library(feasts)
library(distributional)
library(fabletools)

path_ <- "C:\\Users\\Isaac\\Desktop\\Saida-ComCVU-ONS\\Saida-ComCVU-ONS\\"















# 1. CARREGAMENTO
# ------------------------------------------------------------------------------


df_demanda <- bind_rows(
  read_excel(paste0(path_, "SaidasExcel-2024/ReducaoDemanda_AnoCompletoPrecosIndividualizadosPorEmpresaSemanal-2024.xlsx")) %>% mutate(Ano = 2024),
  read_excel(paste0(path_, "SaidasExcel-2025/ReducaoDemanda_AnoCompletoPrecosIndividualizadosPorEmpresaSemanal-2025.xlsx")) %>% mutate(Ano = 2025)
)

df_demanda <- df_demanda %>%
  mutate(
    data_limpa = as.Date(data),
    inicio_da_semana = floor_date(data_limpa, unit = "week", week_start = 3),
    Semana_Num = as.integer(difftime(inicio_da_semana, as.Date("2024-01-03"), units = "weeks")) + 1
  ) %>%
  group_by(Semana_Num, Subsistema) %>% 
  summarise(
    Potencia = sum(Potencia, na.rm = TRUE),
    .groups = "drop" 
  )




df_termica <- bind_rows(
  read_excel(paste0(path_, "SaidasExcel-2024/GeracaoTermicaPorUsinaComCVU-Repdoe-Semanal-2024.xlsx")) %>% mutate(Ano = 2024),
  read_excel(paste0(path_, "SaidasExcel-2025/GeracaoTermicaPorUsinaComCVU-Repdoe-Semanal-2025.xlsx")) %>% mutate(Ano = 2025)
)


df_termica<-df_termica %>%
  mutate(
    data_limpa = as.Date(Data),
    
    
    inicio_da_semana = floor_date(data_limpa, unit = "week", week_start = 3),
    Semana_Num = as.integer(difftime(inicio_da_semana, as.Date("2024-01-03"), units = "weeks")) + 1
  )

df_precos_bruto <- bind_rows(
  read_excel(paste0(path_, "SaidasExcel-2024/ReducaoDemanda_AnoCompletoPrecosIndividualizadosPorEmpresaSemanal-2024.xlsx")) %>% mutate(Ano = 2024),
  read_excel(paste0(path_, "SaidasExcel-2025/ReducaoDemanda_AnoCompletoPrecosIndividualizadosPorEmpresaSemanal-2025.xlsx")) %>% mutate(Ano = 2025)
)

df_precos_bruto<-df_precos_bruto %>%
  mutate(
    data_limpa = as.Date(data),
    
    
    inicio_da_semana = floor_date(data_limpa, unit = "week", week_start = 3),
    Semana_Num = as.integer(difftime(inicio_da_semana, as.Date("2024-01-03"), units = "weeks")) + 1
  )


df_carga <- bind_rows(
  read_excel(paste0(path_, "SaidasExcel-2024/GeracaoTermicaPorMotivoDeDespacho-2024.xlsx")) %>% mutate(Ano = 2024),
  read_excel(paste0(path_, "SaidasExcel-2025/GeracaoTermicaPorMotivoDeDespacho-2025.xlsx")) %>% mutate(Ano = 2025)
)

df_carga <- df_carga %>%
  mutate(
    data_limpa = as.Date(data),
    
    
    inicio_da_semana = floor_date(data_limpa, unit = "week", week_start = 3),
    Semana_Num = as.integer(difftime(inicio_da_semana, as.Date("2024-01-03"), units = "weeks")) + 1
  )



df_renovavel <- bind_rows(
  read_excel(paste0(path_, "SaidasExcel-2024/PDO_Sist_AnoCompleto-2024-MarcadoSemanalmente.xlsx")) %>% mutate(Ano = 2024),
  read_excel(paste0(path_, "SaidasExcel-2025/PDO_Sist_AnoCompleto-2025-MarcadoSemanalmente.xlsx")) %>% mutate(Ano = 2025)
)


df_renovavel <- df_renovavel %>%
  mutate(
    data_limpa = as.Date(data),
    
    
    inicio_da_semana = floor_date(data_limpa, unit = "week", week_start = 3),
    Semana_Num = as.integer(difftime(inicio_da_semana, as.Date("2024-01-03"), units = "weeks")) + 1
  )










# =====================
# 2. PROCESSAMENTO
# =====================
termica_se <- df_termica %>%
  filter(Submercado == "SUDESTE") %>%
  mutate(
    Semana_Num = Semana_Num,
    V_CVU = as.numeric(replace_na(as.character(`CVU-MAX`), "0")),
    # Tratamento de texto para número:
    V_Razao =  as.numeric(replace_na(as.character(`Razão Elétrica`), "0")),
    V_Ordem = as.numeric(replace_na(as.character(`Ordem de Mérito`), "0")),
    Data = Data,
    Quarta_Correspondente=inicio_da_semana
  ) %>%
  mutate(
    V_Razao = replace_na(V_Razao, 0),
    V_Ordem = replace_na(V_Ordem, 0)
  )

termica_se <- termica_se %>%
  group_by(Semana_Num) %>%
  summarise(Quarta_Correspondente=unique(Quarta_Correspondente),
            CVU_MAX_Semana = max(V_CVU, na.rm = TRUE), 
            Soma_Razao = round(sum(V_Razao, na.rm = TRUE), 2),
            Soma_Ordem = round(sum(V_Ordem, na.rm = TRUE), 2),
            .groups = "drop"
  ) %>%
  mutate(
    Motivo_Despacho = case_when(
      # Em vez de "maior que 0 absoluto", colocamos 0.5 MW como tolerância
      # para ignorar resíduos de telemetria ou testes de máquinas do ONS
      Soma_Razao > 0.5 ~ "Razao_Eletrica",
      Soma_Ordem > 0.5 ~ "Ordem_Merito",
      TRUE ~ "Sem_Despacho"
    ),
    Motivo_Despacho = factor(Motivo_Despacho, levels = c("Sem_Despacho", "Ordem_Merito", "Razao_Eletrica"))
  )
termica_se$Quarta_Correspondente<-as.Date(termica_se$Quarta_Correspondente)

renovavel_se<-  df_renovavel %>%
  filter(subsistema == "SE") %>%
  mutate(
    Semana_Num = Semana_Num,
    grenova=`grenova`,
    somatgh=	`somatgh`,
    somatgt=`somatgt`,
    Data= `data`,
    Quarta_Correspondente = Quarta_Correspondente
  )



renovavel_se <- renovavel_se %>%
  group_by(Semana_Num) %>%
  summarise(
    Soma_renovavel = sum(grenova, na.rm = TRUE),
    Soma_gh = sum(somatgh, na.rm = TRUE),
    Soma_gt=sum(somatgt,na.rm = TRUE),
    .groups = "drop"
  )






demanda_se <- df_demanda %>%
  filter(Subsistema == "SE") %>%
  dplyr::select(Semana_Num, Potencia)

precos_se <- df_precos_bruto %>%
  filter(Subsistema == "SE") %>%
  mutate(
    Preco_Num = suppressWarnings(as.numeric(gsub(",", ".", as.character(Preco))))
  ) %>%
  group_by(Semana_Num) %>%
  summarise(
    Preco_Max_Semana = if(all(is.na(Preco_Num))) 0 else max(Preco_Num, na.rm = TRUE), 
    .groups = "drop"
  )

carga_se <- df_carga %>%
  filter(nom_subsistema == "SUDESTE") %>%
  group_by(Semana_Num) %>%
  summarise(
    Carga_razao_eletrica = sum(val_prograzaoeletrica, na.rm = TRUE),
    Carga_Ordem_de_mérito = sum(val_progordemmerito, na.rm = TRUE),
    .groups = "drop"
  ) 

# =====================
# 3. DATAFRAME BRUTO & PERIODOGRAMA (Série Completa N=106)
# =====================
df_model <- tibble(Semana_Num = 1:106) %>%
  left_join(termica_se, by = "Semana_Num") %>%
  left_join(demanda_se, by = "Semana_Num") %>%
  left_join(precos_se, by = "Semana_Num") %>% 
  left_join(carga_se,by="Semana_Num") %>%
  left_join(renovavel_se,by="Semana_Num") %>%
  mutate(Motivo_Despacho = as.character(Motivo_Despacho)) %>% 
  replace_na(list(
    Potencia = 0, CVU_MAX_Semana = 0, Preco_Max_Semana = 0, Motivo_Despacho = "Sem_Despacho" 
  )) %>%
  mutate(
    Motivo_Despacho = factor(Motivo_Despacho),
    Motivo_Despacho = relevel(Motivo_Despacho, ref = "Sem_Despacho"),
    Teve_RD = factor(if_else(Potencia> 0, "Teve_RD", "Nao_Teve_RD"), levels = c("Nao_Teve_RD", "Teve_RD"))
  ) %>% drop_na()




df_model$Montante <- ts(df_model$Potencia, start = c(2024, 1), frequency = 52)


grafico_valores <- ggplot(df_model, aes(x = Quarta_Correspondente)) +geom_line(aes(y = Montante, color = "Dado Real (Observado)"), linewidth = 1) +
  scale_color_manual(values = c("Dado Real (Observad ço)" = "black", "Ajuste do Modelo (Fitted)" = "blue")) +
  labs(
    title = "Valores Semanais do Total de Montante Elétrico Reduzido por RD.",
    subtitle = "Submercado Sudeste/Centro-Oeste, perído 2024 e 2025",
    x = "Tempo", y = "Montante (MW)", color = "Legenda"
  ) +
  theme_minimal()
print(grafico_valores)



##############
#Comentário###
##############
#O Decompose não explica nada, como tinha dito
decompose(df_model$Montante,type="additive") %>% plot()
decompose(df_model$Montante,type="multiplicative") %>% plot()



##############
#Comentário###
##############
#Aqui aparece algumas sazonalidades, mas de forma diferente do que no periodograma
df_model$Montante %>% ggAcf( lag.max = 50) + 
  ggplot2::ggtitle("ACF dos Montantes Semanais \n para o submercado Sudeste/Centro-Oeste")



df_ts <- df_model %>%
  mutate(Montante = as.numeric(Potencia)) %>%
  distinct(Quarta_Correspondente, .keep_all = TRUE) %>%
  as_tsibble(index = Quarta_Correspondente)

##############
#Comentário###
##############
# Não tem como fazer decomposição STL a nível mensal pq tem  apenas  2 periodos sazonais, apenas 2 comjuntos de 52 semanas.

# da erro por só ter 2 periodos sazonais também
df_mes <- df_ts |>
  index_by(Mes = yearmonth(Quarta_Correspondente)) |>
  summarise(
    # Use mean() se o Montante for potência/vazão (ex: MWmed)
    # Use sum() se for volume acumulado de energia (ex: MWh)
    Montante = mean(Montante, na.rm = TRUE) 
  )

#x11_dcmp <- df_mes |>
#  model(x11 = X_13ARIMA_SEATS(Montante ~ x11())) |>
#  components()

#autoplot(x11_dcmp) +
#  labs(title = "Decomposição do Montante Mensal usando X-11") +
#  theme_minimal()

#coef_hurst 0.9359846 está entre 0 e 1, o que significa persistencia Estranho, signfica que ela é não estácionária?
coef_hurst(df_model$Montante)


#spectral_entropy 0.9083456  entropia alta, perto de 1, indica uma série muito ruidosa.
feat_spectral(df_model$Montante)


# Box pierce 3.643336e+01 com p valor 1.579763e-09 indica rejeição de H_0 a nível de significancia de 0.05
#A série NÃo é um ruido branco
box_pierce(df_model$Montante)

#ljung_box 3.749452e+01 p valor9.167007e-10  indica a mesma coisa do boxpierce  
ljung_box(df_model$Montante)


### ESTACIONARIEDADE

##############
#Comentários###
##############

# Kwiatkowski-Phillips-Schmidt-Shin
# Hipotese nula indica que a série é estacionária, alternativa indica quea série não é estacíonaria
#Estatistica 0.162994 pvalor 0.100000 ou seja, não rejeita h_0 a nível de signficancia de 0.05 
# Indica estacionariedade, uma vez que não rejeita H_0
unitroot_kpss(df_model$Montante)

# Phillips-Perron 
#Hipótese Nula= A série possui raiz unitária, e a alternativa fala que a série não possui raiz unitaria, se H_0 é valido a série é não estacionária.
# Estatística medida-5.129092 p valor 0.010000 indica que a nível de sgnificancia de 0.05 rejeita-se h_0
# A série é estacionária
unitroot_pp(df_model$Montante)

#unitroot_ndiffs(df_model$Montante) o núemro de diferenças necessárias para série se tornar estacionária
# Como esperado pelos testes anteriores, esse número é ZERO pelos testes, a série parece ser estacionária
unitroot_ndiffs(df_model$Montante)

#unitroot_nsdiffs(df_model$Montante) Mesma coisa do anterior, mas na questão sazonal, tambem dá ZERO, como esperado
unitroot_nsdiffs(df_model$Montante)


#var_tiled_var 0.6953152 signfica a variancia da volatividade parece quase constante, pois o valor é muito menor do que a escala  da série.
var_tiled_var(df_model$Montante)

#shift_level_max  1355.8  shift_level_index 26.0 como dá para ver pelo o gráfico, esse salto ocorre no mes de 2024.5
shift_level_max(df_model$Montante)



#
#n_crossing_points 19 cruza 19 vezes a mediana, indicando a volatilidade

n_crossing_points(df_model$Montante)


#longest_flat_spot 29 29 semanas com a série relativamnte sem alterar
longest_flat_spot(df_model$Montante)


#stat_arch_lm 0.1872869 
stat_arch_lm(df_model$Montante)
ArchTest(df_model$Montante)
#lambda_guerrero        0.1535261 
guerrero(df_model$Montante+1)
lambda_guerrero <- guerrero(df_model$Montante+1)


###################################################
### Modelos########################################
###################################################
# =====================
# FUNÇÃO AUXILIAR: REVERSÃO DO BOX-COX
# =====================
# Para calcular os envelopes in-sample corretamente, precisamos calcular
# a margem de erro na escala transformada e depois reverter para MW.
reverter_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    exp(x) - 1
  } else {
    # pmax(..., 0) garante que a curva nunca tente calcular raiz de número negativo
    (pmax(x * lambda + 1, 0))^(1 / lambda) - 1
  }
}

# =====================
# 1. MODELO NAIVE
# =====================

lambda_ideal <- df_ts |>
  features(Montante + 1, features = guerrero) |>
  pull(lambda_guerrero)

modelo_naive <- df_ts |> 
  model(NAIVE(box_cox(Montante + 1, lambda_ideal)))

previsao_naive <- modelo_naive |> 
  forecast(h = "4 weeks")

p_naive_out <- previsao_naive |> 
  autoplot(df_ts) +
  ggplot2::labs(
    title = "Previsão NAIVE com Box-Cox Nativo",
    y = "Montante (MW)", 
    x = "Tempo"
  )
print(p_naive_out)

df_ajuste_naive <- augment(modelo_naive) |> 
  mutate(
    sigma_trans = sd(.innov, na.rm = TRUE),
    
    fitted_trans = box_cox(.fitted + 1, lambda_ideal),
    
    lim_inf_trans = fitted_trans - (1.96 * sigma_trans),
    lim_sup_trans = fitted_trans + (1.96 * sigma_trans),
    limite_inferior = reverter_boxcox(lim_inf_trans, lambda_ideal),
    limite_superior = reverter_boxcox(lim_sup_trans, lambda_ideal)
  )

p_naive_in <- ggplot(df_ajuste_naive, aes(x = Quarta_Correspondente)) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = .fitted, color = "Ajuste do Modelo (Fitted)"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = Montante, color = "Dado Real (Observado)"), linewidth = 1) +
  scale_color_manual(values = c("Dado Real (Observado)" = "black", "Ajuste do Modelo (Fitted)" = "blue")) +
  labs(
    title = "Avaliação Histórica (NAIVE) - Envelopes Box-Cox Reais",
    x = "Tempo", y = "Montante (MW)", color = "Legenda"
  ) +
  theme_minimal()
print(p_naive_in)


# =====================
# 2. MODELO SNAIVE (Sazonal)
# =====================

modelo_snaive <- df_ts |> 
  model(SNAIVE(box_cox(Montante + 1, lambda_ideal) ~ lag(52)))

previsao_snaive <- modelo_snaive |> 
  forecast(h = "4 weeks")

p_snaive_out <- previsao_snaive |> 
  autoplot(df_ts) +
  ggplot2::labs(
    title = "Previsão SNAIVE do Montante (Com Box-Cox Nativo)",
    y = "Montante (MW)", 
    x = "Tempo"
  )
print(p_snaive_out)

df_ajuste_snaive <- augment(modelo_snaive) |> 
  mutate(
    sigma_trans = sd(.innov, na.rm = TRUE),
    fitted_trans = box_cox(.fitted + 1, lambda_ideal),
    lim_inf_trans = fitted_trans - (1.96 * sigma_trans),
    lim_sup_trans = fitted_trans + (1.96 * sigma_trans),
    limite_inferior = reverter_boxcox(lim_inf_trans, lambda_ideal),
    limite_superior = reverter_boxcox(lim_sup_trans, lambda_ideal)
  )

# Comentário: Agora os envelopes vão contornar os vales e picos de forma proporcional!
p_snaive_in <- ggplot(df_ajuste_snaive, aes(x = Quarta_Correspondente)) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = .fitted, color = "Ajuste do Modelo (Fitted)"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = Montante, color = "Dado Real (Observado)"), linewidth = 1) +
  scale_color_manual(values = c("Dado Real (Observado)" = "black", "Ajuste do Modelo (Fitted)" = "blue")) +
  labs(
    title = "Avaliação Histórica (SNAIVE) - Envelopes Box-Cox Reais",
    x = "Tempo", y = "Montante (MW)", color = "Legenda"
  ) +
  theme_minimal()
print(p_snaive_in)


# =====================
# RESÍDUOS QUANTÍLICOS DO SNAIVE
# =====================

# O truque aqui é usar o .innov (resíduo de inovação), pois ele é o erro que o
# modelo assume ser Normal (com média 0) no espaço Box-Cox.
df_residuos <- augment(modelo_snaive) |> 
  filter(!is.na(.innov)) |> 
  mutate(
    sigma_erro = sd(.innov, na.rm = TRUE),
    # Como .innov já é o resíduo estabilizado, calculamos a probabilidade na Normal:
    cdf_prob = pnorm(.innov, mean = 0, sd = sigma_erro),
    cdf_prob = pmax(pmin(cdf_prob, 0.9999), 0.0001),
    residuo_quantilico = qnorm(cdf_prob)
  )

# Comentário: O modelo baseline SNAIVE não limpa a autocorrelação. Isso é esperado!
# Ele só lida com sazonalidade longa. Picos recentes continuam afetando o hoje.
acf_plot <- df_residuos$residuo_quantilico |> acf(plot = FALSE)
plot(acf_plot, main = "ACF dos Resíduos Quantílicos (SNAIVE)")


# =====================
# RANDOM WALK COM DRIFT
# =====================

# Comentário Original: "Pior intervalo de confiança de previsão dentre todos os modelos!"
# Correção: Ao usar o mesmo lambda do Guerrero ao invés do log1p (que é lambda = 0),
# o modelo entende exatamente a variância correta e não infla os envelopes!
modelo_drift_seguro <- df_ts |> 
  model(Drift_BoxCox = RW(box_cox(Montante + 1, lambda_ideal) ~ drift()))

previsao_drift_seguro <- modelo_drift_seguro |> 
  forecast(h = "4 weeks")

p_drift <- previsao_drift_seguro |> 
  autoplot(df_ts) +
  ggplot2::labs(
    title = "Previsão Drift com Box-Cox Otimizado (Garantidamente >= 0)",
    subtitle = "O envelope não irá mais explodir como acontecia com o log1p",
    y = "Montante (MW)", 
    x = "Tempo"
  )
print(p_drift)















#################################### ETS


#ABAIXO USEI O LAMBDA DE GUERRERO CALCULADO para a transformação de boxcox,
# ETS(A,N,N) encontrado...
meu_lambda=guerrero(df_model$Montante+1)

modelo_ets <- ets(df_model$Montante, lambda = meu_lambda)
#O ACF e o PACF ficam bons, quase indicam ruido branco com apenas 1 barra excedendo o limite do teste no acf, e 2 barras excedendo o limite do pacf.
modelo_ets$residuals %>% acf(lag.max=52)
modelo_ets$residuals %>% pacf(lag.max=52)

#ABAIXo a modelagem com os intervlaos de confiança de 95%, como aconteceu com modelos mais avançados, os intervalos ficam bem flat.
desvio_padrao <- sqrt(modelo_ets$sigma2)

fitted_trans <- BoxCox(modelo_ets$fitted, lambda = meu_lambda)
#gráfico dos dados transformados na transformação de boxcox
plot(fitted_trans)
Z <- 1.96
limite_inf_trans <- fitted_trans - (Z * desvio_padrao)
limite_sup_trans <- fitted_trans + (Z * desvio_padrao)
df_in_sample <- data.frame(
  Tempo = as.numeric(time(df_model$Montante)),
  Real = as.numeric(df_model$Montante),
  Modelado = as.numeric(modelo_ets$fitted),
  Limite_Inf_95 = as.numeric(InvBoxCox(limite_inf_trans, lambda = meu_lambda)),
  Limite_Sup_95 = as.numeric(InvBoxCox(limite_sup_trans, lambda = meu_lambda))
)

ggplot(df_in_sample, aes(x = Tempo)) +
  geom_ribbon(aes(ymin = Limite_Inf_95, ymax = Limite_Sup_95), 
              fill = "gray70", alpha = 0.5) +
  geom_line(aes(y = Modelado), color = "blue", linewidth = 1) +
  geom_line(aes(y = Real), color = "black", linewidth = 1) +
  
  labs(
    title = "Ajuste do Modelo ETS (In-Sample) com Envelopes de 95%",
    y = "Montante",
    x = "Tempo"
  ) +
  theme_bw()

previsao <- forecast(modelo_ets, h = 4, biasadj = TRUE)
plot(previsao)






n_semanas <- nrow(df_ts)
df_train <- df_ts |> slice(1:(n_semanas - 10))


##############
#Comentário###
##############
# O LAMBDA VARIOU MUITO QUANDO COMPARADO COM O LAMBDA DA SÉrie toda!!!!
lambda_ideal <- guerrero(head(df_model$Montante + 1, -10))

print(paste("Lambda de Guerrero calculado:", round(lambda_ideal, 4)))


modelos_comparativos <- df_train |>
  model(
    `Média` = MEAN(box_cox(Montante + 1, lambda_ideal)),
    `Ingênuo (NAIVE)` = NAIVE(box_cox(Montante + 1, lambda_ideal)),
    `Ingênuo Sazonal (SNAIVE)` = SNAIVE(box_cox(Montante + 1, lambda_ideal) ~ lag(52)),
    `Random Walk (Drift)` = RW(box_cox(Montante + 1, lambda_ideal) ~ drift()),
    `Suavização (ETS)` = ETS(box_cox(Montante + 1, lambda_ideal))
  )

report(modelos_comparativos |> dplyr::select(`Suavização (ETS)`))

previsoes_multiplas <- modelos_comparativos |>
  forecast(h = 10)
p1 <- previsoes_multiplas |>
  autoplot(df_ts, level = NULL, linewidth = 1) +
  labs(
    title = "Previsões Comparativas (Out-of-Sample) vs Dados Reais",
    subtitle = "Treinado em dados passados, prevendo as últimas 10 semanas",
    y = "Montante (MW)",
    x = "Tempo",
    color = "Modelos"
  ) +
  theme_bw() +
  guides(colour = guide_legend(title = "Modelo"))

print(p1)


p2 <- previsoes_multiplas |>
  filter(.model == "Suavização (ETS)") |>
  autoplot(df_ts, linewidth = 1) +
  labs(
    title = "Previsão ETS com Transformação Box-Cox",
    subtitle = "Note que os intervalos de 80% e 95% não cruzam o eixo zero",
    y = "Montante (MW)",
    x = "Tempo"
  ) +
  theme_bw()

print(p2)

metricas <- accuracy(previsoes_multiplas, df_ts) |> 
  arrange(RMSE) # Ordena do menor erro para o maior erro

print("Ranking dos melhores modelos:")
print(metricas |> dplyr::select(.model, RMSE, MAE))



reverter_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    exp(x) - 1
  } else {
    (pmax(x * lambda + 1, 0))^(1 / lambda) - 1
  }
}


modelos_box_jenkins <- df_ts |> 
  model(
    `Auto ARIMA` = ARIMA(box_cox(Montante + 1, lambda_ideal)),
    `ARIMA(1,0,1)` = ARIMA(box_cox(Montante + 1, lambda_ideal) ~ pdq(1, 0, 1))
  )


print("Parâmetros do Modelo Box-Jenkins selecionado:")
report(modelos_box_jenkins |> dplyr::select(`Auto ARIMA`))

diagnostico_plot <- modelos_box_jenkins |> 
  dplyr::select(`Auto ARIMA`) |> 
  gg_tsresiduals() +
  ggplot2::labs(title = "Diagnóstico de Resíduos (Box-Jenkins Auto-ARIMA)")

print(diagnostico_plot)


previsao_arima <- modelos_box_jenkins |> 
  dplyr::select(`Auto ARIMA`) |> 
  forecast(h = "4 weeks")

p_arima_out <- previsao_arima |> 
  autoplot(df_ts) +
  ggplot2::labs(
    title = "Previsão Box-Jenkins (Auto-ARIMA) com Box-Cox",
    subtitle = "Limites matematicamente protegidos acima de zero",
    y = "Montante (MW)", 
    x = "Tempo"
  ) +
  theme_bw()

print(p_arima_out)


df_ajuste_arima <- augment(modelos_box_jenkins |> dplyr::select(`Auto ARIMA`)) |> 
  mutate(
    sigma_trans = sd(.innov, na.rm = TRUE),
    fitted_trans = box_cox(.fitted + 1, lambda_ideal),
    
    lim_inf_trans = fitted_trans - (1.96 * sigma_trans),
    lim_sup_trans = fitted_trans + (1.96 * sigma_trans),
    
    limite_inferior = reverter_boxcox(lim_inf_trans, lambda_ideal),
    limite_superior = reverter_boxcox(lim_sup_trans, lambda_ideal)
  )

p_arima_in <- ggplot(df_ajuste_arima, aes(x = Quarta_Correspondente)) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = .fitted, color = "Ajuste do Modelo (Fitted)"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = Montante, color = "Dado Real (Observado)"), linewidth = 1) +
  scale_color_manual(values = c("Dado Real (Observado)" = "black", "Ajuste do Modelo (Fitted)" = "blue")) +
  labs(
    title = "Avaliação Histórica (ARIMA) - Envelopes Box-Cox Dinâmicos ",
    subtitle = "Ajuste in-sample da metodologia ARIMA sob os dados do montante elétrico\n reduzido por RD para o submercado Sudeste/Centro-Oeste",
    x = "Tempo", 
    y = "Montante (MW)", 
    color = "Legenda"
  ) +
  theme_minimal()

print(p_arima_in)





modelo_sarima <- df_ts |> 
  model(
    `SARIMA_Forcado` = ARIMA(box_cox(Montante + 1, lambda_ideal) ~ pdq(1, 0, 1) + PDQ(0, 1, 1))
  )
previsao_sarima <- modelo_sarima |> forecast(h = "4 weeks")

p_sarima_out <- previsao_sarima |> 
  autoplot(df_ts) +
  ggplot2::labs(
    title = "Previsão SARIMA Explícito (1,0,1)(0,1,1)[52] com Box-Cox",
    subtitle = "Forçando a sazonalidade semanal no braço PDQ",
    y = "Montante (MW)", 
    x = "Tempo"
  ) +
  theme_bw()

print(p_sarima_out)



df_ajuste_sarima <- augment(modelo_sarima) |> 
  mutate(
    sigma_trans = sd(.innov, na.rm = TRUE),
    fitted_trans = box_cox(.fitted + 1, lambda_ideal),
    lim_inf_trans = fitted_trans - (1.96 * sigma_trans),
    lim_sup_trans = fitted_trans + (1.96 * sigma_trans),
    limite_inferior = reverter_boxcox(lim_inf_trans, lambda_ideal),
    limite_superior = reverter_boxcox(lim_sup_trans, lambda_ideal)
  )

p_sarima_in <- ggplot(df_ajuste_sarima, aes(x = Quarta_Correspondente)) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), fill = "red", alpha = 0.2) +
  geom_line(aes(y = .fitted, color = "Ajuste SARIMA (Fitted)"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = Montante, color = "Dado Real (Observado)"), linewidth = 1) +
  scale_color_manual(values = c("Dado Real (Observado)" = "black", "Ajuste SARIMA (Fitted)" = "red")) +
  labs(
    title = "Avaliação Histórica (SARIMA Explícito) - Envelopes Box-Cox",
    x = "Tempo", 
    y = "Montante (MW)", 
    color = "Legenda"
  ) +
  theme_minimal()

print(p_sarima_in)

report(modelo_sarima)







# =====================
# 7. MODELO HOLT-WINTERS PARA ALTA FREQUÊNCIA (STL + ETS ADITIVO) CORRIGIDO
# =====================

modelo_hw <- df_ts |> 
  model(
    `Holt-Winters Semanal` = decomposition_model(
      STL(box_cox(Montante + 1, lambda_ideal) ~ season(window = "periodic")),
      
      ETS(season_adjust ~ error("A") + trend("A") + season("N"))
    )
  )


previsao_hw <- modelo_hw |> forecast(h = "4 weeks")

p_hw_out <- previsao_hw |> 
  autoplot(df_ts) +
  ggplot2::labs(
    title = "Previsão Holt-Winters (STL+ETS) com Box-Cox",
    subtitle = "Sazonalidade isolada pelo STL e tendência/erro suavizados pelo ETS",
    y = "Montante (MW)", 
    x = "Tempo"
  ) +
  theme_bw()

print(p_hw_out)

df_ajuste_hw <- augment(modelo_hw) |> 
  mutate(
    sigma_trans = sd(.innov, na.rm = TRUE),
    
    fitted_trans = box_cox(.fitted + 1, lambda_ideal),
    
    lim_inf_trans = fitted_trans - (1.96 * sigma_trans),
    lim_sup_trans = fitted_trans + (1.96 * sigma_trans),
    
    limite_inferior = reverter_boxcox(lim_inf_trans, lambda_ideal),
    limite_superior = reverter_boxcox(lim_sup_trans, lambda_ideal)
  )

p_hw_in <- ggplot(df_ajuste_hw, aes(x = Quarta_Correspondente)) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), fill = "darkgreen", alpha = 0.2) +
  geom_line(aes(y = .fitted, color = "Ajuste Holt-Winters (Fitted)"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = Montante, color = "Dado Real (Observado)"), linewidth = 1) +
  scale_color_manual(values = c("Dado Real (Observado)" = "black", "Ajuste Holt-Winters (Fitted)" = "darkgreen")) +
  labs(
    title = "Avaliação Histórica (Holt-Winters Semanal) - Envelopes Box-Cox",
    subtitle = "Ajuste in-sample da série",
    x = "Tempo", 
    y = "Montante (MW)", 
    color = "Legenda"
  ) +
  theme_minimal()

print(p_hw_in)

report(modelo_hw)







# 1. Treinando TODOS os modelos univariados na base de treino (df_train)
modelos_completos <- df_train |>
  model(
    `Média` = MEAN(box_cox(Montante + 1, lambda_ideal)),
    `Ingênuo (NAIVE)` = NAIVE(box_cox(Montante + 1, lambda_ideal)),
    `Ingênuo Sazonal (SNAIVE)` = SNAIVE(box_cox(Montante + 1, lambda_ideal) ~ lag(52)),
    `Random Walk (Drift)` = RW(box_cox(Montante + 1, lambda_ideal) ~ drift()),
    `Suavização (ETS)` = ETS(box_cox(Montante + 1, lambda_ideal)),
    
    `Auto ARIMA` = ARIMA(box_cox(Montante + 1, lambda_ideal)),
    `ARIMA Explícito (1,0,1)` = ARIMA(box_cox(Montante + 1, lambda_ideal) ~ pdq(1, 0, 1)),
    `SARIMA (1,0,1)(0,1,1)[52]` = ARIMA(box_cox(Montante + 1, lambda_ideal) ~ pdq(1, 0, 1) + PDQ(0, 1, 1)),
    
    `Holt-Winters Semanal (STL+ETS)` = decomposition_model(
      STL(box_cox(Montante + 1, lambda_ideal) ~ season(window = "periodic")),
      ETS(season_adjust ~ error("A") + trend("A") + season("N"))
    )
  )

# 2. Fazendo a previsão das 10 semanas (out-of-sample)
previsoes_completas <- modelos_completos |> forecast(h = 10)

# 3. Extraindo as métricas para preencher a tabela no LaTeX
metricas_completas <- accuracy(previsoes_completas, df_ts) |>
  dplyr::select(.model, RMSE, MAE) |>
  arrange(RMSE)

# Imprime o resultado final (n = Inf garante que não vai ocultar nenhuma linha)
print(metricas_completas, n = Inf)






# =====================
# 10. MODELO DE CROSTON PARA POTÊNCIA (SÉRIES INTERMITENTES)
# =====================
library(forecast)

cat("\n--- INICIANDO MODELAGEM CROSTON PARA POTÊNCIA ---\n")

ts_potencia <-df_model$Montante

modelo_croston <- croston(ts_potencia, h = 1)

df_model$Previsao_Croston <- as.numeric(fitted(modelo_croston))

grafico_croston <- ggplot(df_model, aes(x = Quarta_Correspondente)) +
  geom_line(aes(y = Montante, color = "Real"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Previsao_Croston, color = "Previsto Croston"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Real" = "black", "Previsto Croston" = "#ff7f0e")) +
  labs(
    title = "Gráfico de Modelagem: Real vs Croston (Montante)",
    subtitle = "Sudeste/Centro-Oeste",
    x = "Semana",
    y = "MWh"
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(grafico_croston)


df_residuos_croston <- data.frame(
  Data = df_model$Quarta_Correspondente,
  Residuo = df_model$Previsao_Croston - df_model$Potencia
) %>% drop_na()


acf_val_cros <- acf(df_residuos_croston$Residuo, na.action = na.pass, plot = FALSE)
df_acf_cros <- data.frame(lag = acf_val_cros$lag, acf = acf_val_cros$acf)
limite_ci_cros <- 1.96 / sqrt(nrow(df_residuos_croston))

p1_c <- ggplot(df_residuos_croston, aes(x = Data, y = Residuo)) +
  geom_line(color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "1. Resíduos no Tempo (Croston)", x = "Data", y = "Resíduo (MWh)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2_c <- ggplot(df_residuos_croston, aes(x = Residuo)) +
  geom_histogram(aes(y = ..density..), fill = "steelblue", color = "black", bins = 30, alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "2. Distribuição dos Resíduos", x = "Resíduo", y = "Densidade") +
  theme_minimal()

p3_c <- ggplot(df_acf_cros, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0), size = 1, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(-limite_ci_cros, limite_ci_cros), color = "blue", linetype = "dashed") +
  labs(title = "3. Autocorrelação (ACF)", x = "Lag", y = "ACF") +
  theme_minimal()

p4_c <- ggplot(df_residuos_croston, aes(sample = Residuo)) +
  stat_qq(color = "black", alpha = 0.6) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "4. QQ-Plot", x = "Quantis Teóricos", y = "Quantis da Amostra") +
  theme_minimal()

painel_residuos_croston <- (p1_c | p2_c) / (p3_c | p4_c) +
  plot_annotation(
    title = "Análise de Resíduos: Modelo de Croston (Potência)",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(painel_residuos_croston)

rmse_croston <- sqrt(mean((df_residuos_croston$Residuo)^2))
mae_croston <- mean(abs(df_residuos_croston$Residuo))

print(data.frame(
  Modelo = "Croston",
  RMSE = round(rmse_croston, 4),
  MAE = round(mae_croston, 4)
))









# =================================
# 11. SUAVIZAÇÃO EXPONENCIAL SIMPLES (SES - 1957)
# ================================

modelo_ses <- ses(ts_potencia, h = 1)

df_model$Previsao_SES <- as.numeric(fitted(modelo_ses))


grafico_ses <- ggplot(df_model, aes(x = Quarta_Correspondente)) +
  geom_line(aes(y = Montante, color = "Real"), linewidth = 1, alpha = 0.5) +
  geom_line(aes(y = Previsao_Croston, color = "Croston (1972)"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = Previsao_SES, color = "SES (1957)"), linetype = "dotted", linewidth = 1) +
  scale_color_manual(values = c(
    "Real" = "black", 
    "Croston (1972)" = "#ff7f0e", 
    "SES (1957)" = "red"
  )) +
  labs(
    title = "O Problema da Intermitência: SES vs Croston",
    subtitle = "O SES decai até zerar, enquanto o Croston distribui a demanda e nunca zera nos períodos inativos.",
    x = "Semana",
    y = "Potência (MWh)"
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(grafico_ses)

# ==============================
# ADICIONANDO ENVELOPES DE CONFIANÇA NO SES
# ==============================

sigma_ses <- sd(modelo_ses$residuals, na.rm = TRUE)

df_model <- df_model %>%
  mutate(
    Lim_Sup_SES = Previsao_SES + (1.96 * sigma_ses),
    
    Lim_Inf_SES = pmax(Previsao_SES - (1.96 * sigma_ses), 0)
  )

grafico_ses_envelope <- ggplot(df_model, aes(x = Quarta_Correspondente)) +
  geom_ribbon(aes(ymin = Lim_Inf_SES, ymax = Lim_Sup_SES), fill = "red", alpha = 0.2) +
  
  geom_line(aes(y = Previsao_SES, color = "SES (Fitted)"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = Montante, color = "Dado Real"), linewidth = 1, alpha = 0.8) +
  
  scale_color_manual(values = c("Dado Real" = "black", "SES (Fitted)" = "red")) +
  labs(
    title = "Ajuste SES (1957) com Envelopes de Confiança (95%)",
    subtitle = "Note que o envelope inferior foi artificialmente truncado em zero",
    x = "Semana",
    y = "Potência (MWh)",
    color = "Legenda"
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(grafico_ses_envelope)





# ===============================
# 11.2. RESÍDUOS QUANTÍLICOS DO SES
# ==============================

residuos_ses <- as.numeric(modelo_ses$residuals)
sigma_ses <- sd(residuos_ses, na.rm = TRUE)

df_residuos_ses <- df_model %>%
  dplyr::select(Quarta_Correspondente, Montante, Previsao_SES) %>%
  mutate(
    Residuo_Original = residuos_ses,
    
    cdf_prob = pnorm(Residuo_Original, mean = 0, sd = sigma_ses),
    
    cdf_prob = pmax(pmin(cdf_prob, 0.9999), 0.0001),
    
    Residuo_Quantilico = qnorm(cdf_prob)
  ) %>%
  drop_na()


p_acf_ses <- df_residuos_ses$Residuo_Quantilico %>% 
  acf(plot = FALSE) %>% 
  autoplot() +
  labs(
    title = "ACF dos Resíduos Quantílicos (SES)",
    x = "Lag", y = "ACF"
  ) +
  theme_minimal()

p_qq_ses <- ggplot(df_residuos_ses, aes(sample = Residuo_Quantilico)) +
  stat_qq(color = "black", alpha = 0.6) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "QQ-Plot dos Resíduos Quantílicos (SES)", 
    x = "Quantis Teóricos", y = "Quantis da Amostra"
  ) +
  theme_minimal()

painel_ses <- (p_acf_ses | p_qq_ses)
print(painel_ses)














# ARIMAX
df_model <- df_model[!duplicated(df_model), ]
lambda_guerrero <- 0.1535261

df_model$Montante_trans <- BoxCox(df_model$Montante + 1, lambda = lambda_guerrero)

modelo_completo <- lm(Montante_trans ~ CVU_MAX_Semana + Soma_Razao + Soma_Ordem + 
                        Motivo_Despacho + Preco_Max_Semana + Carga_razao_eletrica + 
                        Carga_Ordem_de_mérito + Soma_renovavel + Soma_gh + 
                        Soma_gt , 
                      data = df_model)

n <- nrow(df_model)
modelo_stepwise <- step(modelo_completo, direction = "both", k = log(n), trace = 0)
xreg_treino <- model.matrix(modelo_stepwise)[, -1]

modelo_arimax <- auto.arima(df_model$Montante + 1, 
                            xreg = xreg_treino, 
                            stepwise = TRUE,   
                            ic = "bic",        
                            lambda = lambda_guerrero,
                            approximation = FALSE)
modelo_arimax %>% summary()

lmtest::coeftest(modelo_arimax)

df_futuro <- df_model[rep(nrow(df_model), 4), ] 
xreg_futuro <- model.matrix(formula(modelo_stepwise), data = df_futuro)[, -1]

previsao <- forecast(modelo_arimax, h = 4, xreg = xreg_futuro)

previsao$mean <- previsao$mean - 1
previsao$lower <- previsao$lower - 1
previsao$upper <- previsao$upper - 1

y_real <- as.numeric(df_model$Montante)
y_fit <- as.numeric(fitted(modelo_arimax)) - 1
n_treino <- length(y_real)

sigma_modelo <- sqrt(modelo_arimax$sigma2)
y_real_trans <- as.numeric(BoxCox(y_real + 1, lambda_guerrero))
residuos_trans <- as.numeric(residuals(modelo_arimax))

y_fit_trans <- y_real_trans - residuos_trans
lower_trans <- y_fit_trans - 1.96 * sigma_modelo
upper_trans <- y_fit_trans + 1.96 * sigma_modelo

lower_insample <- as.numeric(InvBoxCox(lower_trans, lambda_guerrero)) - 1
upper_insample <- as.numeric(InvBoxCox(upper_trans, lambda_guerrero)) - 1

eixo_x_treino <- 1:n_treino
eixo_x_futuro <- (n_treino + 1):(n_treino + 4)

limites_x <- c(1, n_treino + 4)
limites_y <- range(c(y_real, y_fit, as.numeric(previsao$lower), as.numeric(previsao$upper), 
                     lower_insample, upper_insample), na.rm = TRUE)

par(mar = c(5, 4, 4, 2) + 0.1)

plot(eixo_x_treino, y_real, type = "n", xlim = limites_x, ylim = limites_y,
     main = "ARIMAX com IC In-Sample e Previsão (Box-Cox)",
     ylab = "Montante", xlab = "Períodos")

polygon(c(eixo_x_treino, rev(eixo_x_treino)), 
        c(lower_insample, rev(upper_insample)), 
        col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)

polygon(c(eixo_x_futuro, rev(eixo_x_futuro)), 
        c(previsao$lower[,2], rev(previsao$upper[,2])), 
        col = rgb(0.2, 0.6, 0.2, 0.2), border = NA) 

lines(eixo_x_treino, y_real, col = "blue", lwd = 2)
lines(eixo_x_treino, y_fit, col = "red", lty = 2, lwd = 2)

lines(eixo_x_futuro, previsao$mean, col = "darkgreen", lwd = 3)

legend("topleft", 
       legend = c("Real", "Ajustado", "Previsão", "IC In-Sample (95%)", "IC Previsão (95%)"), 
       col = c("blue", "red", "darkgreen", rgb(0.8, 0.8, 0.8, 0.5), rgb(0.2, 0.6, 0.2, 0.2)), 
       lty = c(1, 2, 1, 1, 1), 
       lwd = c(2, 2, 3, 8, 8),
       bty = "n", cex = 0.8)
