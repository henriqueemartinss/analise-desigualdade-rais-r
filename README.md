# Análise da Desigualdade Salarial no Brasil (RAIS 2023)

Pipeline em **R (`tidyverse`, `data.table`, `ggplot2`, `flextable`)** desenvolvido para ingestão, saneamento, diagnóstico de anomalias e modelagem distributiva a partir dos microdados da **RAIS 2023**, comparando dois estados em extremos educacionais: **Alagoas** e **Santa Catarina**.

## Arquitetura do Pipeline
1. **Ingestão e Saneamento em Escala:** Processamento das bases regionais brutas (~29,2 milhões de registros combinados e 60 variáveis), filtragem de vínculos ativos, padronização categórica e tratamento de colunas duplicadas.
2. **Diagnóstico e Tratamento de Outliers:** Identificação de forte assimetria na cauda superior da remuneração, mapeamento setorial dos supersalários (`> 30 SM`) via códigos **CNAE 2.0** (evidenciando concentração na Administração Pública e Judiciário) e aplicação de corte metodológico consistente em 30 salários mínimos.
3. **Análise Exploratória Multivariada:** Cruzamentos de remuneração por gênero, escolaridade, raça/cor, volatilidade interna (Intervalo Interquartílico — IQ) e análise interseccional para o ensino superior e pós-graduação.
4. **Modelagem Distributiva:** Implementação própria da **Curva de Lorenz** (por decis) e cálculo do **Índice de Gini** via integração numérica (Regra dos Trapézios).

## Principais Resultados
- **Índice de Gini (Alagoas):** `0.377`
- **Índice de Gini (Santa Catarina):** `0.355`
- Apesar de Santa Catarina apresentar patamares salariais médios e medianos significativamente superiores em todos os recortes, as Curvas de Lorenz quase sobrepostas evidenciam que a estrutura interna de concentração de renda é semelhante entre os dois estados.

## Arquivos do Repositório
- `analise_desigualdade_rais_2023.R` — Código-fonte completo do pipeline de dados, visualizações e cálculos.
- `Entrega Final - Grupo 05.docx.pdf` — Relatório técnico completo com gráficos, tabelas e interpretação econômica.

> **Nota sobre os dados:** Os arquivos brutos `.rds` da RAIS (~29M de linhas) não estão versionados neste repositório devido ao limite de tamanho do GitHub.
