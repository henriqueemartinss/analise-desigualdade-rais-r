# ==============================================================================
# PROJETO: O Impacto da Escolaridade na Remuneração em Santa Catarina e Alagoas
# SUBTÍTULO: Uma análise da desigualdade brasileira pautada nos extremos (RAIS 2023)
# INSTITUIÇÃO: Instituto de Economia — UNICAMP (CE242)
# AUTORES: Henrique Abreu Vieira Martins, Ariel Fischer, Lorena Alessio da Silva,
#          Talles Rafael Oliveira Martins
# ==============================================================================
# DESCRIÇÃO DO PIPELINE:
#   1. Ingestão e saneamento dos microdados da RAIS 2023 (~29,2 milhões de linhas)
#   2. Diagnóstico de missings e mapeamento setorial de outliers (via CNAE 2.0)
#   3. Tratamento de valores extremos (corte metodológico em 30 SM)
#   4. Análise exploratória multivariada (Gênero, Escolaridade, Raça/Cor e Interseccional)
#   5. Implementação algorítmica da Curva de Lorenz e Índice de Gini (regra dos trapézios)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(flextable)
library(scales)

# Parâmetro global: Salário Mínimo nominal de referência (2023)
SM_2023 <- 1320

# ------------------------------------------------------------------------------
# 1. INGESTÃO E LIMPEZA DAS BASES (NORDESTE E SUL)
# ------------------------------------------------------------------------------

#' Limpa, filtra e padroniza os microdados brutos da RAIS para uma UF específica
#'
#' @param dados Data.frame ou data.table bruto da RAIS
#' @param codigo_uf Código IBGE da Unidade da Federação (27 = AL, 42 = SC)
#' @param sm_ref Valor do salário mínimo nominal de referência
#' @return Data.frame tratado e pronto para análise exploratória
limpar_base_rais <- function(dados, codigo_uf, sm_ref = SM_2023) {
  # Garante que não existam nomes de colunas duplicados (ex.: "Tipo Estab")
  names(dados) <- make.unique(names(dados))

  dados |>
    mutate(UF = floor(Município / 10000)) |>
    filter(UF == codigo_uf) |>
    select(
      `CNAE 2.0 Classe`,
      `Vínculo Ativo 31/12`,
      `Escolaridade após 2005`,
      Idade,
      `Raça Cor`,
      `V1 Remun Média Nom`,
      `Sexo Trabalhador`,
      `Tempo Emprego`,
      `Qtd Hora Contr`
    ) |>
    # Calcula remuneração em salários mínimos com base no SM de 2023
    mutate(remuneracao_sm = `V1 Remun Média Nom` / sm_ref) |>
    # Filtros de consistência e vínculos ativos
    filter(
      `Vínculo Ativo 31/12` == 1,
      remuneracao_sm >= 1,
      `Tempo Emprego` > 0,
      `Qtd Hora Contr` > 0,
      Idade >= 14
    ) |>
    rename(
      cnae                = `CNAE 2.0 Classe`,
      escolaridade        = `Escolaridade após 2005`,
      idade               = Idade,
      raca_cor            = `Raça Cor`,
      sexo                = `Sexo Trabalhador`,
      tempo_emprego_meses = `Tempo Emprego`,
      horas_semanais      = `Qtd Hora Contr`
    ) |>
    # Tradução dos códigos numéricos da RAIS para rótulos descritivos
    mutate(
      sexo = case_when(
        sexo == 1  ~ "Masculino",
        sexo == 2  ~ "Feminino",
        sexo == -1 ~ "Ignorado",
        TRUE       ~ "Não identificado"
      ),
      escolaridade = case_when(
        escolaridade == 1  ~ "Analfabeto",
        escolaridade == 2  ~ "Até 5ª Incompleto",
        escolaridade == 3  ~ "5ª Completo",
        escolaridade == 4  ~ "6ª a 9ª Incompleto",
        escolaridade == 5  ~ "Fundamental Completo",
        escolaridade == 6  ~ "Médio Incompleto",
        escolaridade == 7  ~ "Médio Completo",
        escolaridade == 8  ~ "Superior Incompleto",
        escolaridade == 9  ~ "Superior Completo",
        escolaridade == 10 ~ "Mestrado",
        escolaridade == 11 ~ "Doutorado",
        TRUE               ~ "Não identificado"
      ),
      raca_cor = case_when(
        raca_cor == 1  ~ "Indígena",
        raca_cor == 2  ~ "Branca",
        raca_cor == 4  ~ "Preta",
        raca_cor == 6  ~ "Amarela",
        raca_cor == 8  ~ "Parda",
        raca_cor == 9  ~ "Não declarado",
        raca_cor == -1 ~ "Ignorado",
        TRUE           ~ "Não identificado"
      )
    )
}

# --- Base Nordeste (14.347.406 obs. x 60 variáveis) -> Alagoas (UF == 27) ---
rais_nordeste <- readRDS("rais_nordeste.rds")
# str(rais_nordeste)

base_eda_alagoas <- limpar_base_rais(rais_nordeste, codigo_uf = 27)
saveRDS(base_eda_alagoas, "base_AL.rds")
rm(rais_nordeste)
gc()

# --- Base Sul (14.859.895 obs. x 60 variáveis) -> Santa Catarina (UF == 42) ---
rais_sul <- readRDS("rais_sul.rds")
# str(rais_sul)

base_eda_santacatarina <- limpar_base_rais(rais_sul, codigo_uf = 42)
saveRDS(base_eda_santacatarina, "base_SC.rds")
rm(rais_sul)
gc()

# ------------------------------------------------------------------------------
# 2. IDENTIFICAÇÃO DE MISSINGS E DIAGNÓSTICO DE OUTLIERS
# ------------------------------------------------------------------------------

# Verificação de estatísticas descritivas e valores ausentes
summary(base_eda_alagoas)
colSums(is.na(base_eda_alagoas))

summary(base_eda_santacatarina)
colSums(is.na(base_eda_santacatarina))

# --- Distribuição bruta (antes do corte de outliers) ---

# Alagoas: Boxplot e Histograma
p_box_al_bruto <- ggplot(base_eda_alagoas, aes(y = remuneracao_sm)) +
  geom_boxplot(fill = "#F8766D", color = "darkred", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Boxplot da Remuneração em Salários Mínimos - Alagoas",
    y     = "Remuneração (SM)",
    x     = ""
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_hist_al_bruto <- ggplot(base_eda_alagoas, aes(x = remuneracao_sm)) +
  geom_histogram(bins = 50, fill = "#F8766D", color = "darkred", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribuição da Remuneração - Alagoas",
    x     = "Remuneração (SM)",
    y     = "Frequência"
  )

# Santa Catarina: Boxplot e Histograma
p_box_sc_bruto <- ggplot(base_eda_santacatarina, aes(y = remuneracao_sm)) +
  geom_boxplot(fill = "#00BFC4", color = "darkcyan", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Boxplot da Remuneração em Salários Mínimos - Santa Catarina",
    y     = "Remuneração (SM)",
    x     = ""
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_hist_sc_bruto <- ggplot(base_eda_santacatarina, aes(x = remuneracao_sm)) +
  geom_histogram(bins = 50, fill = "#00BFC4", color = "darkcyan", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribuição da Remuneração - Santa Catarina",
    x     = "Remuneração (SM)",
    y     = "Frequência"
  )

print(p_box_al_bruto)
print(p_hist_al_bruto)
print(p_box_sc_bruto)
print(p_hist_sc_bruto)

# --- Análise Setorial dos "Super Salários" (> 30 SM) via CNAE 2.0 ---

get_top_cnae_nomeado <- function(dados) {
  dados |>
    filter(remuneracao_sm > 30) |>
    count(cnae, sort = TRUE) |>
    head(5) |>
    mutate(
      freq_relativa = n / sum(n),
      SETOR = case_when(
        cnae == 84116 ~ "Administração Pública em Geral",
        cnae == 84230 ~ "Justiça",
        cnae == 84248 ~ "Segurança e Ordem Pública",
        cnae == 84256 ~ "Defesa Civil",
        cnae == 86101 ~ "Atividades de Atendimento Hospitalar",
        cnae == 64221 ~ "Bancos Múltiplos (Com Carteira Comercial)",
        cnae == 10716 ~ "Fabricação de Açúcar em Bruto",
        cnae == 72100 ~ "Pesquisa e Desenv. em Ciências Físicas/Naturais",
        cnae == 94120 ~ "Atividades de Organizações Políticas",
        cnae == 94201 ~ "Atividades de Organizações Sindicais",
        cnae == 62015 ~ "Desenv. de Software Sob Encomenda",
        cnae == 50301 ~ "Comércio de Peças de Veículos",
        TRUE          ~ paste("Outro Setor (Cód:", cnae, ")")
      )
    ) |>
    select(SETOR, n, `FREQ. RELATIVA` = freq_relativa) |>
    mutate(`FREQ. RELATIVA` = scales::percent(`FREQ. RELATIVA`, accuracy = 0.1))
}

tabela_al_outliers <- get_top_cnae_nomeado(base_eda_alagoas)
tabela_sc_outliers <- get_top_cnae_nomeado(base_eda_santacatarina)

flextable(tabela_al_outliers) |>
  set_caption("Top 5 Setores com Salários > 30 SM em Alagoas") |>
  theme_zebra() |>
  autofit()

flextable(tabela_sc_outliers) |>
  set_caption("Top 5 Setores com Salários > 30 SM em Santa Catarina") |>
  theme_zebra() |>
  autofit()

# --- Aplicação do Corte Metodológico em 30 Salários Mínimos ---
base_al_final <- base_eda_alagoas |>
  filter(remuneracao_sm <= 30)

base_sc_final <- base_eda_santacatarina |>
  filter(remuneracao_sm <= 30)

# Distribuição pós-corte (Alagoas)
p_box_al_corte <- ggplot(base_al_final, aes(y = remuneracao_sm)) +
  geom_boxplot(fill = "#F8766D", color = "darkred", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Boxplot da Remuneração em Salários Mínimos\nAlagoas (Corte em 30 SM)",
    y     = "Remuneração (SM)",
    x     = ""
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_hist_al_corte <- ggplot(base_al_final, aes(x = remuneracao_sm)) +
  geom_histogram(bins = 30, fill = "#F8766D", color = "darkred", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribuição da Remuneração - Alagoas (Corte em 30 SM)",
    x     = "Remuneração (SM)",
    y     = "Frequência"
  )

# Distribuição pós-corte (Santa Catarina)
p_box_sc_corte <- ggplot(base_sc_final, aes(y = remuneracao_sm)) +
  geom_boxplot(fill = "#00BFC4", color = "darkcyan", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Boxplot da Remuneração em Salários Mínimos\nSanta Catarina (Corte em 30 SM)",
    y     = "Remuneração (SM)",
    x     = ""
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

p_hist_sc_corte <- ggplot(base_sc_final, aes(x = remuneracao_sm)) +
  geom_histogram(bins = 30, fill = "#00BFC4", color = "darkcyan", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribuição da Remuneração - Santa Catarina (Corte em 30 SM)",
    x     = "Remuneração (SM)",
    y     = "Frequência"
  )

print(p_box_al_corte)
print(p_hist_al_corte)
print(p_box_sc_corte)
print(p_hist_sc_corte)

# ------------------------------------------------------------------------------
# 3. PREPARAÇÃO DA BASE COMBINADA (ALAGOAS + SANTA CATARINA)
# ------------------------------------------------------------------------------
base_al_com_estado <- base_al_final |> mutate(estado = "Alagoas")
base_sc_com_estado <- base_sc_final |> mutate(estado = "Santa Catarina")

base_final <- bind_rows(base_al_com_estado, base_sc_com_estado) |>
  filter(!raca_cor %in% c("Não declarado", "Ignorado", "Não identificado"))

# Ordenação hierárquica dos níveis de escolaridade
ordem_escolaridade <- c(
  "Analfabeto",
  "Até 5ª Incompleto",
  "5ª Completo",
  "6ª a 9ª Incompleto",
  "Fundamental Completo",
  "Médio Incompleto",
  "Médio Completo",
  "Superior Incompleto",
  "Superior Completo",
  "Mestrado",
  "Doutorado"
)

base_final    <- base_final    |> mutate(escolaridade = factor(escolaridade, levels = ordem_escolaridade))
base_al_final <- base_al_final |> mutate(escolaridade = factor(escolaridade, levels = ordem_escolaridade))
base_sc_final <- base_sc_final |> mutate(escolaridade = factor(escolaridade, levels = ordem_escolaridade))

# ------------------------------------------------------------------------------
# 4. ANÁLISE EXPLORATÓRIA MULTIVARIADA
# ------------------------------------------------------------------------------

# --- 4.1 Disparidade Salarial por Gênero ---
p_genero <- ggplot(base_final, aes(x = estado, y = remuneracao_sm, fill = sexo)) +
  geom_boxplot() +
  scale_y_log10(
    breaks = c(1, 2, 5, 10, 20),
    labels = scales::number_format(accuracy = 1)
  ) +
  labs(
    title    = "Comparativo da Distribuição Salarial por Gênero",
    subtitle = "Alagoas e Santa Catarina | RAIS 2023",
    x        = "Estado",
    y        = "Remuneração (Salários Mínimos)",
    fill     = "Sexo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_genero)

# Medianas salariais por gênero
mediana_genero_al <- base_al_final |>
  group_by(sexo) |>
  summarise(mediana_salarial_sm = median(remuneracao_sm), .groups = "drop")

mediana_genero_sc <- base_sc_final |>
  group_by(sexo) |>
  summarise(mediana_salarial_sm = median(remuneracao_sm), .groups = "drop")

print(mediana_genero_al)
print(mediana_genero_sc)

# --- 4.2 Remuneração Média por Escolaridade ---
p_escolaridade <- ggplot(
  base_final,
  aes(x = escolaridade, y = remuneracao_sm, color = estado, group = estado)
) +
  stat_summary(fun = "mean", geom = "line", linewidth = 1) +
  stat_summary(fun = "mean", geom = "point", size = 2) +
  labs(
    title    = "Comparativo da Remuneração Média por Escolaridade",
    subtitle = "Alagoas x Santa Catarina | RAIS 2023",
    x        = "Nível de Escolaridade",
    y        = "Remuneração Média (Salários Mínimos)",
    color    = "Estado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_escolaridade)

# Tabela comparativa de remuneração média e prêmio salarial regional
tabela_clean <- base_final |>
  group_by(escolaridade, sexo, estado) |>
  summarise(Media_SM = mean(remuneracao_sm, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = estado, values_from = Media_SM) |>
  mutate(
    Diferenca_Pct       = (`Santa Catarina` - Alagoas) / Alagoas,
    Diferenca_Formatada = scales::percent(Diferenca_Pct, accuracy = 1)
  ) |>
  select(
    Escolaridade    = escolaridade,
    Sexo            = sexo,
    `Média AL (SM)` = Alagoas,
    `Média SC (SM)` = `Santa Catarina`,
    Diferença       = Diferenca_Formatada
  )

tabela_clean |>
  flextable() |>
  set_caption("Comparativo de Remuneração Média e Diferença Regional") |>
  colformat_double(digits = 2) |>
  theme_zebra() |>
  add_header_row(
    values    = c("", "", "Remuneração Média (SM)", ""),
    colwidths = c(1, 1, 2, 1)
  ) |>
  align(align = "center", part = "header") |>
  autofit()

# --- 4.3 Disparidade Salarial por Raça/Cor ---
p_raca <- ggplot(base_final, aes(x = raca_cor, y = remuneracao_sm, fill = raca_cor)) +
  geom_boxplot() +
  scale_y_log10(
    breaks = c(1, 2, 5, 10, 20),
    labels = scales::number_format(accuracy = 1)
  ) +
  facet_wrap(~ estado) +
  labs(
    title    = "Comparativo da Distribuição Salarial por Raça",
    subtitle = "Alagoas x Santa Catarina | RAIS 2023",
    x        = "Raça/Cor",
    y        = "Remuneração (Salários Mínimos)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

print(p_raca)

# Tabela de Mediana e Dispersão (Intervalo Interquartílico - IQ) por Raça/Cor
tabela_raca <- base_final |>
  group_by(estado, raca_cor) |>
  summarise(
    Mediana         = median(remuneracao_sm),
    Volatilidade_IQ = IQR(remuneracao_sm),
    .groups         = "drop"
  ) |>
  pivot_wider(
    names_from  = estado,
    values_from = c(Mediana, Volatilidade_IQ)
  ) |>
  mutate(
    Diferenca_Pct       = (`Mediana_Santa Catarina` - Mediana_Alagoas) / Mediana_Alagoas,
    Diferenca_Formatada = scales::percent(Diferenca_Pct, accuracy = 1)
  ) |>
  select(
    `Raça/Cor`              = raca_cor,
    `Mediana (AL)`          = Mediana_Alagoas,
    `Mediana (SC)`          = `Mediana_Santa Catarina`,
    Diferença               = Diferenca_Formatada,
    `Volatilidade (IQ - AL)` = Volatilidade_IQ_Alagoas,
    `Volatilidade (IQ - SC)` = `Volatilidade_IQ_Santa Catarina`
  )

tabela_raca |>
  flextable() |>
  set_caption("Comparativo de Mediana e Volatilidade Salarial por Raça/Cor") |>
  colformat_double(digits = 2) |>
  theme_zebra() |>
  add_header_row(
    values    = c("", "Remuneração Mediana", "Dispersão (Volatilidade)"),
    colwidths = c(1, 3, 2)
  ) |>
  align(align = "center", part = "header") |>
  autofit()

# --- 4.4 Cruzamento: Escolaridade x Gênero ---
p_esc_gen <- ggplot(base_final, aes(x = escolaridade, y = remuneracao_sm, fill = sexo)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_wrap(~ estado, ncol = 1) +
  labs(
    title    = "Comparativo da Remuneração Média por Escolaridade e Gênero",
    subtitle = "Alagoas x Santa Catarina | RAIS 2023",
    x        = "Nível de Escolaridade",
    y        = "Remuneração Média (Salários Mínimos)",
    fill     = "Sexo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_esc_gen)

# --- 4.5 Análise Interseccional (Raça x Gênero x Ensino Superior/Pós) ---
base_superior <- base_final |>
  filter(escolaridade %in% c("Superior Completo", "Mestrado", "Doutorado"))

p_interseccional <- ggplot(base_superior, aes(x = raca_cor, y = remuneracao_sm, fill = sexo)) +
  geom_boxplot() +
  scale_y_log10(
    breaks = c(2, 5, 10, 20),
    labels = scales::number_format(accuracy = 1)
  ) +
  facet_grid(estado ~ escolaridade) +
  labs(
    title    = "Análise Interseccional da Remuneração",
    subtitle = "Comparativo por Gênero e Raça para Níveis de Escolaridade Superiores",
    x        = "Raça/Cor",
    y        = "Remuneração (Salários Mínimos)",
    fill     = "Sexo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_interseccional)

# ------------------------------------------------------------------------------
# 5. MEDIDAS DE DESIGUALDADE E CONCENTRAÇÃO (CURVA DE LORENZ & ÍNDICE DE GINI)
# ------------------------------------------------------------------------------

#' Calcula a Curva de Lorenz (por decis) e o Índice de Gini via integração numérica
#' (Regra dos Trapézios)
#'
#' @param dados Data.frame contendo a coluna numérica `remuneracao_sm`
#' @param nome_estado Rótulo do estado para identificação na tabela final
#' @return Lista contendo a tabela acumulada (`tabela`) e o escalar `gini`
calcular_gini_lorenz <- function(dados, nome_estado) {
  dados <- dados |> filter(!is.na(remuneracao_sm))

  # Segmentação da distribuição em decis
  dados$classe <- cut(
    dados$remuneracao_sm,
    breaks         = quantile(dados$remuneracao_sm, probs = seq(0, 1, 0.1), na.rm = TRUE),
    include.lowest = TRUE,
    labels         = FALSE
  )

  # Frequências relativas por decil
  tabela <- dados |>
    filter(!is.na(classe)) |>
    group_by(classe) |>
    summarise(
      pop_tot   = n(),
      renda_tot = sum(remuneracao_sm, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    mutate(
      freq_pop   = pop_tot / sum(pop_tot),
      freq_renda = renda_tot / sum(renda_tot)
    )

  # Origem (0, 0) e frequências acumuladas
  linha_nula <- data.frame(
    classe     = 0,
    pop_tot    = 0,
    renda_tot  = 0,
    freq_pop   = 0,
    freq_renda = 0
  )

  tabela <- bind_rows(linha_nula, tabela) |>
    mutate(
      freq_pop_ac   = cumsum(freq_pop),
      freq_renda_ac = cumsum(freq_renda),
      Estado        = nome_estado
    )

  # Área sob a Curva de Lorenz (Beta) via Regra dos Trapézios
  tabela_gini_calc <- tabela |>
    mutate(
      base    = freq_pop_ac - lag(freq_pop_ac, default = 0),
      alturas = freq_renda_ac + lag(freq_renda_ac, default = 0),
      area    = (base * alturas) / 2
    )

  beta <- sum(tabela_gini_calc$area, na.rm = TRUE)
  gini <- 1 - (2 * beta)

  return(list(tabela = tabela, gini = abs(gini)))
}

# Execução dos cálculos
res_al <- calcular_gini_lorenz(base_al_final, "Alagoas")
res_sc <- calcular_gini_lorenz(base_sc_final, "Santa Catarina")

gini_al_val <- as.numeric(round(res_al$gini, 3)[1])
gini_sc_val <- as.numeric(round(res_sc$gini, 3)[1])

cat(sprintf("Índice de Gini (Alagoas):        %.3f\n", gini_al_val))
cat(sprintf("Índice de Gini (Santa Catarina): %.3f\n", gini_sc_val))

# Gráfico Comparativo da Curva de Lorenz
tabela_plot <- bind_rows(res_al$tabela, res_sc$tabela)

p_lorenz <- ggplot(
  data = tabela_plot,
  aes(x = freq_pop_ac, y = freq_renda_ac, color = Estado)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title    = "Curva de Lorenz Comparativa",
    subtitle = "Distribuição da Remuneração (RAIS 2023)",
    x        = "% Acumulado da População",
    y        = "% Acumulado da Renda",
    color    = "Estado"
  ) +
  coord_fixed() +
  theme(legend.position = "bottom")

print(p_lorenz)
