# solow growth model - momento 3

library(ggplot2)
library(gridExtra)
library(ggrepel)


# params

alpha <- 1/3
delta <- 0.08
s <- 1/3

n_atual <- 0.01
n_B <- 0
g_A <- 0.005


# base

k_vals <- seq(0, 12, length.out = 500)

y_base <- k_vals^alpha
y_tec <- (1 + g_A) * k_vals^alpha

inv_atual <- s * y_base
inv_A <- s * y_tec
inv_B <- s * y_base

dep_atual <- (delta + n_atual) * k_vals
dep_B <- delta * k_vals

df <- data.frame(
  k = k_vals,
  y = y_base,
  inv_atual = inv_atual,
  inv_A = inv_A,
  inv_B = inv_B,
  dep_atual = dep_atual,
  dep_B = dep_B
)


# equilibrios

df_eq <- data.frame(
  k = c(7.13, 6.57, 8.51),
  y = c(0.64, 0.62, 0.68),  # investimento (i*)
  Label = c(
    "EE atual\nk*=7,13\ny*=1,92\nc*=1,28",
    "Política A\nk*=6,57\ny*=1,87\nc*=1,25",
    "Política B\nk*=8,51\ny*=2,04\nc*=1,36"
  ),
  Cor = c("Atual", "Tecnologia", "População")
)


# grafico classico

grafico_classico <- ggplot(df, aes(x = k)) +
  
  geom_line(aes(y = y, color = "Produção f(k)", linetype = "solid"), linewidth = 1.2) +
  
  geom_line(aes(y = inv_atual, color = "Investimento atual", linetype = "solid"), linewidth = 1.2) +
  geom_line(aes(y = inv_A, color = "Investimento (Política A)", linetype = "dashed"), linewidth = 1.1) +
  geom_line(aes(y = inv_B, color = "Investimento (Política B)", linetype = "solid"), linewidth = 1.2) +
  
  geom_line(aes(y = dep_atual, color = "Reposição (δ+n)", linetype = "solid"), linewidth = 1.2) +
  geom_line(aes(y = dep_B, color = "Reposição (δ)", linetype = "dashed"), linewidth = 1.1) +
  
  geom_point(data = df_eq, aes(x = k, y = y, color = Cor),
             size = 4, show.legend = FALSE) +
  
  geom_segment(data = df_eq,
               aes(x = k, y = 0, xend = k, yend = y, color = Cor),
               linetype = "dotted", show.legend = FALSE) +
  
  geom_label_repel(data = df_eq,
                   aes(x = k, y = y, label = Label),
                   box.padding = 0.5,
                   point.padding = 0.3,
                   size = 3.5,
                   fontface = "bold") +
  
  scale_color_manual(
    values = c(
      "Produção f(k)" = "#1f77b4",
      "Investimento atual" = "#2ca02c",
      "Investimento (Política A)" = "#9467bd",
      "Investimento (Política B)" = "#ff7f0e",
      "Reposição (δ+n)" = "#d62728",
      "Reposição (δ)" = "#8c564b",
      "Atual" = "#2ca02c",
      "Tecnologia" = "#9467bd",
      "População" = "#ff7f0e"
    )
  ) +
  
  scale_linetype_manual(values = c("solido" = "solid", "tracejado" = "dashed")) +
  
  labs(
    title = "solow growth model - momento 3",
    subtitle = "Comparação entre EE atual, política tecnológica e política populacional",
    x = "Estoque de Capital por Trabalhador (k)",
    y = "Investimento e Reposição per capita",
    color = "Curvas",
    linetype = "Estilo"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )


# grafico de transicao (politica B)

t_max <- 150
k_t <- numeric(t_max)
k_t[1] <- 7.13

for(t in 1:(t_max - 1)) {
  k_t[t+1] <- k_t[t] + (s * k_t[t]^alpha) - (delta * k_t[t])
}

df_tempo <- data.frame(
  Tempo = 1:t_max,
  Capital = k_t
)

grafico_tempo <- ggplot(df_tempo, aes(x = Tempo, y = Capital)) +
  geom_line(color = "#1f77b4", linewidth = 1.3) +

  geom_hline(yintercept = 7.13, linetype = "dashed", color = "#2ca02c") +
  geom_hline(yintercept = 8.51, linetype = "dashed", color = "#ff7f0e") +

  annotate("text", x = Inf, y = 7.13, label = "EE atual",
           hjust = 1.1, vjust = -0.5, color = "#2ca02c", fontface = "bold") +
  annotate("text", x = Inf, y = 8.51, label = "Política B",
           hjust = 1.1, vjust = -0.5, color = "#ff7f0e", fontface = "bold") +

  labs(
    title = "Transição do Capital - Momento 3",
    subtitle = "Convergência para o novo estado estacionário (Política B)",
    x = "Tempo",
    y = "Capital por trabalhador (k)"
  ) +

  theme_minimal(base_size = 11)

p <- grid.arrange(grafico_classico, grafico_tempo, ncol = 2)
ggsave("momento3.png", p, width = 16, height = 7, dpi = 300)
