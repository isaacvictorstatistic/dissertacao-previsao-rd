# Previsão do Montante Total de Energia Elétrica Reduzido pelo Programa de Resposta da Demanda no Submercado Sudeste e Centro-Oeste

Repositório oficial contendo os códigos em R, análises exploratórias, modelagens univariadas/multivariadas e a documentação preliminar da dissertação de mestrado desenvolvida no Departamento de Engenharia de Produção da **Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio).

* **Autor:** Isaac Victor Silva Rodrigues
* **Orientadora:** Profa. Paula Medina Maçaira Louro
* **Coorientador:** Prof. Fernando Cyrino
* **Coorientador:** Carlos Saboia
* **Programa:** Mestrado em Engenharia de Produção

---

## Sobre o Projeto

Com o crescimento de fontes renováveis intermitentes, o Sistema Interligado Nacional (SIN) enfrenta desafios logísticos diários no suprimento de eletricidade. O programa de Resposta da Demanda (RD) atua como um mecanismo estratégico de flexibilização para mitigar restrições e equilibrar a oferta e a demanda a curto prazo. 

Contudo, a modelagem estatística desses dados esbarra em uma forte característica estocástica: a  intermitência severa (elevada frequência de valores nulos/zeros correspondentes a dias ou semanas sem acionamento de contrato). Este repositório documenta a investigação estatística para prever o montante semanal reduzido no submercado Sudeste/Centro-Oeste (região com maior concentração de acionamentos do país).

---

## Modelos e Abordagens Metodológicas Implementadas

Os scripts em R realizam o tratamento de dados a partir de relatórios públicos do Operador Nacional do Sistema Elétrico (ONS - REPDOE) e aplicam as seguintes famílias de modelos:

1. **Modelos de Base (Baselines):** Naïve, Média, Random Walk com Drift e SNaïve.
2. **Suavização Exponencial:** Suavização Exponencial Simples (SES), Holt-Winters e o framework ETS (Error, Trend, Seasonality) estruturado via Máxima Verossimilhança.
3. **Família Box-Jenkins:** Modelos ARIMA, SARIMA e ARIMAX (incorporando covariáveis operacionais como preço máximo, geração renovável e custo variável unitário - CVU).
4. **Séries Intermitentes:** Modelo de Croston e a correção SBA (Syntetos-Boylan Approximation) para lidar com picos esparsos e excesso de zeros.
5. **Transformações e Diagnósticos:** Estabilização de variância via Transformação de Box-Cox com parâmetro $\lambda$ otimizado pelo método de Guerrero, além de testes de estacionariedade (KPSS, Phillips-Perron) e análise de resíduos quantílicos.

---

## Estrutura do Repositório

```text
├── analise_rd.R             # Script principal em R contendo toda a pipeline de análise e modelagens
├── texto_formal_da_dissertação.pdf # Proposta/Versão textual formal da dissertação em PDF
└── .gitignore               # Regras para ignorar bases de dados brutas e ficheiros temporários
