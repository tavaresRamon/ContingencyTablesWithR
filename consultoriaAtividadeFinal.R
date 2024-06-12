pacman::p_load("tidyverse", "readxl")

dados <- read_excel("C:/Users/tavar/Downloads/dadosNaoParametrica.xlsx")

str(dados)
# Definir os intervalos para as variáveis numéricas
intervalos_desemprego <- cut(as.numeric(dados$`Unemployment rate`), breaks = c(-Inf, 5, 10, 15, Inf), labels = c("Baixa", "Média-Baixa", "Média-Alta", "Alta"))
intervalos_inflacao <- cut(as.numeric(dados$`Inflation rate`), breaks = c(-Inf, 0, 2, 5, Inf), labels = c("Baixa", "Média-Baixa", "Média-Alta", "Alta"))
intervalos_pib <- cut(as.numeric(dados$GDP), breaks = c(-Inf, 0, 2, 5, Inf), labels = c("Baixo", "Médio-Baixo", "Médio-Alto", "Alto"))

dados <- dados %>%
  mutate(
    Estado_Civil = as.factor(`Marital status`),
    Nacionalidade = as.factor(Nacionality),
    Modo_Inscricao = as.factor(`Application mode`),
    Curso = as.factor(Course),
    Qualificacao_Previa = as.factor(`Previous qualification`),
    Escolaridade_Mae = as.factor(`Mother's qualification`),
    Escolaridade_Pai = as.factor(`Father's qualification`),
    Ocupacao_Mae = as.factor(`Mother's occupation`),
    Ocupacao_Pai = as.factor(`Father's occupation`),
    Genero = as.factor(Gender),
    Atendimento_dia_noite = as.factor(`Daytime/evening attendance`),
    Deslocado = as.factor(Displaced),
    Necessidades_Especiais = as.factor(`Educational special needs`),
    Devedor = as.factor(Debtor),
    Mensalidades_Em_Dia = as.factor(`Tuition fees up to date`),
    Bolsista = as.factor(`Scholarship holder`),
    Internacional = as.factor(International),
    Status_Final = factor(Target, levels = c("Dropout", "Enrolled", "Graduate"), labels = c("Desistente", "Matriculado", "Formado")),
    Taxa_Desemprego = as.factor(intervalos_desemprego),
    Taxa_Inflacao = as.factor(intervalos_inflacao),
    PIB = as.factor(intervalos_pib)
  )
# Calcular a porcentagem de cada status
resumo_status <- dados %>%
  count(Status_Final) %>%
  mutate(Percentual = n / sum(n) * 100)

# Exibir o resumo
print(resumo_status)

# Criar o gráfico de barras com cores específicas
ggplot(resumo_status, aes(x = Status_Final, y = Percentual, fill = Status_Final)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percentual)), vjust = -0.5) +
  labs(title = "Distribuição do Status dos Alunos", x = "Status", y = "Percentual") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red", "darkgreen"))

tipoVariavel <- sapply(dados,class)
variaveisFatores <- tipoVariavel[tipoVariavel == 'factor']
dadosProntos <- dados %>%
  select(all_of(names(variaveisFatores)))

head(dadosProntos)
colnames(dadosProntos)

# Calcular os testes de associação para cada variável em relação a Status_Final
resultados_contingencia <- lapply(dadosProntos[-ncol(dadosProntos)], function(variavel) {
  tabela <- table(variavel, dadosProntos$Status_Final)
  resultado <- fisher.test(tabela, simulate.p.value = TRUE)
  return(resultado)
})

# Extrair os p-valores e os nomes das variáveis
p_values <- sapply(resultados_contingencia, function(res) res$p.value)
var_names <- names(resultados_contingencia)

# Criar um dataframe com os resultados
df <- data.frame(Variable = var_names, p_value = p_values)

# Ordenar o dataframe pelo valor-p
df <- df[order(df$p_value), ]

# Definir um limiar de significância (por exemplo, 0.05)
significance_threshold <- 0.05

# Criar um vetor de tradução
traducao_status <- c("Graduate" = "Formado",
                     "Enrolled" = "Matriculado",
                     "Dropout" = "Desistente")

# Criar o gráfico de barras com as categorias traduzidas e destacando as categorias de Status_Final
p <- ggplot(df, aes(x = reorder(Variable, p_value), y = p_value)) +
  geom_bar(stat = "identity", fill = ifelse(df$p_value < significance_threshold, "red", "blue")) +
  geom_text(aes(label = round(p_value, 3)), vjust = -0.5, size = 3, color = "black") +
  geom_text(data = data.frame(Status_Final = names(traducao_status), 
                              p_value = rep(0.04, length(traducao_status)),  # Defina a posição de cada rótulo
                              label = names(traducao_status)), 
            aes(x = length(df$Variable) + 1, y = p_value, label = label), 
            vjust = -0.5, size = 3, color = "black", hjust = 1.2) +  # Adicionar rótulos para as categorias de Status_Final
  geom_hline(yintercept = significance_threshold, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Valores-p dos Testes de Associação",
       x = "Variável",
       y = "Valor-p") +
  theme_minimal()

# Exibir o gráfico
print(p)

# Lista de variáveis significativas
variaveis_significativas <- c("Estado_Civil", "Modo_Inscricao", "Curso", "Qualificacao_Previa",
                              "Escolaridade_Mae", "Escolaridade_Pai", "Ocupacao_Mae", "Ocupacao_Pai",
                              "Genero", "Atendimento_dia_noite", "Deslocado", "Devedor",
                              "Mensalidades_Em_Dia", "Bolsista","PIB","Taxa_Desemprego")

# Filtrar o dataframe original para incluir apenas as variáveis significativas
dados_significativos <- dadosProntos[, c(variaveis_significativas, "Status_Final")]

# Lista para armazenar as tabelas de frequência
freq_tables <- list()

# Criar tabelas de frequência para cada variável significativa em relação a Status_Final
for (variavel in variaveis_significativas) {
  # Calcular contagem de cada combinação de níveis
  tabela_contingencia <- table(dados_significativos[[variavel]], dados_significativos$Status_Final)
  
  # Adicionar a tabela à lista
  freq_tables[[variavel]] <- tabela_contingencia
}

# Exibir as tabelas de frequência
freq_tables

# Função para criar gráficos de barras para cada variável
criar_graficos <- function(freq_tables, variavel) {
  # Transformar a tabela de frequência em um data frame
  df <- as.data.frame.matrix(freq_tables[[variavel]])
  
  # Adicionar a coluna da variável ao data frame
  df[[variavel]] <- rownames(df)
  
  # Transformar os nomes das colunas em uma variável
  df_long <- tidyr::pivot_longer(df, cols = c(Dropout, Enrolled, Graduate), names_to = "Status_Final", values_to = "Frequencia")
  
  # Ordenar as categorias no eixo x
  df_long <- df_long %>%
    mutate(!!sym(variavel) := factor(!!sym(variavel), levels = unique(!!sym(variavel)))) 
  
  # Traduzir os nomes das variáveis "Status_Final"
  df_long$Status_Final <- factor(df_long$Status_Final, labels = c("Desistente", "Matriculado", "Formado"))
  
  # Criar o gráfico de barras
  p <- ggplot(df_long, aes(x = !!rlang::sym(variavel), y = Frequencia, fill = Status_Final)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Frequencia), vjust = -0.5, position = position_dodge(0.9)) +
    scale_fill_manual(values = c("Desistente" = "red", "Matriculado" = "blue", "Formado" = "darkgreen")) +
    labs(title = paste("Distribuição de Status Final por", variavel), x = variavel, y = "Frequência") +
    theme_minimal()
  
  # Imprimir o gráfico
  print(p)
}

# Aplicar a função para todas as variáveis exceto "Estado_Civil"
for (var in variaveis_significativas[1]) {
  criar_graficos(freq_tables, var)
}







# Função para criar gráficos de barras para cada variável
criar_graficos <- function(freq_tables, variavel) {
  # Transformar a tabela de frequência em um data frame
  df <- as.data.frame.matrix(freq_tables[[variavel]])
  
  # Adicionar a coluna da variável ao data frame
  df[[variavel]] <- rownames(df)
  
  # Transformar os nomes das colunas em uma variável
  df_long <- tidyr::pivot_longer(df, cols = c(Dropout, Enrolled, Graduate), names_to = "Status_Final", values_to = "Frequencia")
  
  # Ordenar as categorias no eixo x
  df_long <- df_long %>%
    mutate(!!sym(variavel) := factor(!!sym(variavel), levels = unique(!!sym(variavel))))
  
  # Traduzir os nomes das variáveis "Status_Final"
  df_long$Status_Final <- factor(df_long$Status_Final, labels = c("Desistente", "Matriculado", "Formado"))
  
  # Calcular as somas das frequências por categoria da variável
  df_summary <- df_long %>%
    group_by(!!rlang::sym(variavel)) %>%
    summarise(Total_Frequencia = sum(Frequencia))
  
  # Selecionar as 5 categorias com as maiores frequências
  top_5 <- df_summary %>%
    arrange(desc(Total_Frequencia)) %>%
    head(5)
  
  # Filtrar o data frame original para manter apenas as categorias selecionadas
  df_filtered <- df_long %>%
    filter(!!rlang::sym(variavel) %in% top_5[[variavel]])
  
  # Criar o gráfico de barras
  p <- ggplot(df_filtered, aes(x = !!rlang::sym(variavel), y = Frequencia, fill = Status_Final)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Frequencia), vjust = -0.5, position = position_dodge(0.9)) +
    scale_fill_manual(values = c("Desistente" = "red", "Matriculado" = "blue", "Formado" = "darkgreen")) +
    labs(title = paste("Distribuição de Status Final por", variavel), x = variavel, y = "Frequência") +
    theme_minimal()
  
  # Imprimir o gráfico
  print(p)
}

# Aplicar a função para todas as variáveis exceto "Estado_Civil"
for (var in variaveis_significativas[-1]) {
  criar_graficos(freq_tables, var)
}


# Função para criar gráficos de barras para cada variável
criar_graficos <- function(freq_tables, variavel) {
  # Transformar a tabela de frequência em um data frame
  df <- as.data.frame.matrix(freq_tables[[variavel]])
  
  # Adicionar a coluna da variável ao data frame
  df[[variavel]] <- rownames(df)
  
  # Transformar os nomes das colunas em uma variável
  df_long <- tidyr::pivot_longer(df, cols = c(Dropout, Enrolled, Graduate), names_to = "Status_Final", values_to = "Frequencia")
  
  # Ordenar as categorias no eixo x
  df_long <- df_long %>%
    mutate(!!sym(variavel) := factor(!!sym(variavel), levels = unique(!!sym(variavel))))
  
  # Traduzir os nomes das variáveis "Status_Final"
  df_long$Status_Final <- factor(df_long$Status_Final, labels = c("Desistente", "Matriculado", "Formado"))
  
  # Calcular as somas das frequências por categoria da variável
  df_summary <- df_long %>%
    group_by(!!rlang::sym(variavel)) %>%
    summarise(Total_Frequencia = sum(Frequencia))
  
  # Selecionar as 5 categorias com as menores frequências
  bottom_5 <- df_summary %>%
    arrange(Total_Frequencia) %>%
    head(5)
  
  # Filtrar o data frame original para manter apenas as categorias selecionadas
  df_filtered <- df_long %>%
    filter(!!rlang::sym(variavel) %in% bottom_5[[variavel]])
  
  # Criar o gráfico de barras
  p <- ggplot(df_filtered, aes(x = !!rlang::sym(variavel), y = Frequencia, fill = Status_Final)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Frequencia), vjust = -0.5, position = position_dodge(0.9)) +
    scale_fill_manual(values = c("Desistente" = "red", "Matriculado" = "blue", "Formado" = "darkgreen")) +
    labs(title = paste("Distribuição de Status Final por", variavel), x = variavel, y = "Frequência") +
    theme_minimal()
  
  # Imprimir o gráfico
  print(p)
}

# Aplicar a função para todas as variáveis exceto "Estado_Civil"
for (var in variaveis_significativas[-1]) {
  criar_graficos(freq_tables, var)
}


