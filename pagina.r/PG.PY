import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, accuracy_score

# Carregando o dataset Titanic
url = "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic = pd.read_csv(url)

# Preparando dados
titanic = titanic.dropna(subset=['Age', 'Embarked'])  # Removendo valores nulos importantes
titanic['Sex'] = titanic['Sex'].map({'male': 0, 'female': 1})
titanic['Embarked'] = titanic['Embarked'].map({'C': 0, 'Q': 1, 'S': 2})

# Features e target
X = titanic[['Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked']]
y = titanic['Survived']

# Dividindo em treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Modelo Random Forest
model = RandomForestClassifier(n_estimators=100, random_state=42)
model.fit(X_train, y_train)

# Previsões
y_pred = model.predict(X_test)

# Configuração para gráficos
plt.figure(figsize=(18, 12))

# 1. Distribuição de Idade
plt.subplot(2, 3, 1)
sns.histplot(titanic['Age'].dropna(), kde=True, color='skyblue')
plt.title('Distribuição de Idade')

# 2. Contagem de Sobreviventes
plt.subplot(2, 3, 2)
sns.countplot(x='Survived', data=titanic, palette='pastel')
plt.title('Contagem de Sobreviventes')

# 3. Sobrevivência por Classe Social
plt.subplot(2, 3, 3)
sns.countplot(x='Pclass', hue='Survived', data=titanic, palette='coolwarm')
plt.title('Sobrevivência por Classe Social')

# 4. Mapa de Correlação (excluindo colunas não numéricas)
plt.subplot(2, 3, 4)
corr = titanic.select_dtypes(include=['float64', 'int64']).corr()
sns.heatmap(corr, annot=True, cmap='coolwarm', center=0, fmt='.2f', linewidths=0.5, annot_kws={"size": 10})
plt.title('Mapa de Correlação entre Variáveis')

# 5. Acurácia do Modelo
plt.subplot(2, 3, 5)
accuracy = accuracy_score(y_test, y_pred)
sns.barplot(x=[accuracy], y=["Acurácia"], color='green')
plt.title(f'Acurácia do Modelo: {accuracy:.2f}')

# 6. Importância das Variáveis para o Modelo
plt.subplot(2, 3, 6)
sns.barplot(x=model.feature_importances_, y=X.columns)
plt.title('Importância das Variáveis para o Modelo')

# Ajuste do layout para melhor visualização
plt.subplots_adjust(hspace=0.4, wspace=0.4)

# Exibindo os gráficos
plt.show()

# Relatório de Classificação
print("Relatório de Classificação:\n")
print(classification_report(y_test, y_pred))
