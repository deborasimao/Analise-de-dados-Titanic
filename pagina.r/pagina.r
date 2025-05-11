# Carregando pacotes necessários
library(ggplot2)

# Passo 1: Criando o conjunto de dados simulado
cat("Criando o conjunto de dados simulado...\n")
set.seed(123)  # Para reprodutibilidade

n <- 100  # Número de observações
vendas <- round(runif(n, 100, 1000), 2)  # Vendas entre 100 e 1000 dólares
desconto <- round(runif(n, 0, 30), 2)  # Desconto entre 0% e 30%
categoria <- sample(c("A", "B", "C"), n, replace = TRUE)  # Categorias de produtos
data_venda <- sample(seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by="day"), n, replace = TRUE)

# Criando o data frame
dados <- data.frame(ID = 1:n, Vendas = vendas, Desconto = desconto, Categoria = categoria, Data_Venda = data_venda)

# Pausar para o usuário continuar
cat("\nConjunto de dados criado. Pressione Enter para continuar...\n")
readline()

# Passo 2: Análise Exploratória de Dados (AED)
cat("\nIniciando a Análise Exploratória de Dados...\n")
cat("\nSumário dos dados:\n")
summary(dados)

# Pausar para o usuário continuar
cat("\nPressione Enter para continuar com a visualização gráfica...\n")
readline()

# Visualizando a distribuição das vendas
ggplot(dados, aes(x = Vendas)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black") + 
  labs(title = "Distribuição das Vendas", x = "Vendas ($)", y = "Frequência")

# Pausar para o usuário continuar
cat("\nGráfico das Vendas exibido. Pressione Enter para continuar...\n")
readline()

# Boxplot para visualizar a relação entre desconto e vendas
ggplot(dados, aes(x = Categoria, y = Vendas, fill = Categoria)) +
  geom_boxplot() +
  labs(title = "Distribuição de Vendas por Categoria", x = "Categoria", y = "Vendas ($)")

# Pausar para o usuário continuar
cat("\nBoxplot exibido. Pressione Enter para continuar com a análise de correlação...\n")
readline()

# Verificando a correlação entre vendas e desconto
cat("\nCorrelação entre Vendas e Desconto: ", cor(dados$Vendas, dados$Desconto), "\n")

# Pausar para o usuário continuar
cat("\nCorrelação exibida. Pressione Enter para continuar com a modelagem...\n")
readline()

# Passo 3: Modelagem (Regressão Linear)
cat("\nIniciando a modelagem (Regressão Linear)...\n")

# Convertendo a variável "Categoria" para fator
dados$Categoria <- as.factor(dados$Categoria)

# Modelo de regressão linear
modelo <- lm(Vendas ~ Desconto + Categoria, data = dados)

# Sumário do modelo
cat("\nSumário do Modelo de Regressão:\n")
summary(modelo)

# Pausar para o usuário continuar
cat("\nModelo exibido. Pressione Enter para continuar com as previsões...\n")
readline()

# Passo 4: Previsões com o modelo
dados$previsao <- predict(modelo)

# Visualizando os resultados
ggplot(dados, aes(x = Desconto, y = Vendas)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relação entre Desconto e Vendas com Regressão Linear", x = "Desconto (%)", y = "Vendas ($)")

# Pausar para o usuário continuar
cat("\nGráfico com previsões exibido. Pressione Enter para continuar com a avaliação do modelo...\n")
readline()

# Passo 5: Avaliação do Modelo
cat("\nIniciando a avaliação do modelo...\n")

# Calculando os resíduos
residuos <- modelo$residuals

# Calculando as métricas de avaliação
mae <- mean(abs(residuos))  # Erro médio absoluto
rmse <- sqrt(mean(residuos^2))  # Erro quadrático médio
r2 <- summary(modelo)$r.squared  # R²

# Exibindo as métricas de avaliação
cat("\nMétricas de Avaliação do Modelo:\n")
cat("MAE (Erro Médio Absoluto): ", mae, "\n")
cat("RMSE (Erro Quadrático Médio): ", rmse, "\n")
cat("R²: ", r2, "\n")

cat("\nAnálise concluída. Pressione Enter para finalizar...\n")
readline()
