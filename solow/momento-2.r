# solow growth model - momento 2
# entrada de crescimento populacional n = 1%

library(ggplot2)
library(gridExtra)
library(ggrepel)

# params

alpha <- 1/3
delta <- 0.08
n <- 0.01
s <- 1/3


# grafico classico

k_vals <- seq(0, 12, length.out = 500)
y_vals <- k_vals^alpha
break_even_old <- delta * k_vals
break_even_new <- (delta + n) * k_vals
inv_vals <- s * y_vals

df_solow <- data.frame(
  k = k_vals,
  y = y_vals,
  inv = inv_vals,
  be_old = break_even_old,
  be_new = break_even_new
)

# estados estacionarios

k_star1 <- 8.505172718    # antes: n = 0
inv_star1 <- s * (k_star1^alpha)

k_star2 <- 7.127781101    # depois: n = 1%
inv_star2 <- s * (k_star2^alpha)

df_equilibrio <- data.frame(
  k = c(k_star1, k_star2),
  y = c(inv_star1, inv_star2),
  Label = c("k* = 8,51\n(n=0%)", "k* = 7,13\n(n=1%)"),
  Cor = c("Inicial", "Novo")
)

grafico_classico <- ggplot(df_solow, aes(x = k)) +
  geom_line(aes(y = y, color = "Produção f(k)", linetype = "solid"), linewidth = 1.2) +
  geom_line(aes(y = inv, color = "Investimento s·f(k)", linetype = "solid"), linewidth = 1.2) +
  geom_line(aes(y = be_new, color = "Reposição (δ+n)k", linetype = "dashed"), linewidth = 1.2) +
  geom_line(aes(y = be_old, color = "Reposição δk", linetype = "Tracejado"), linewidth = 1.1) +
  
  geom_point(data = df_equilibrio, aes(x = k, y = y, color = Cor),
             size = 4, show.legend = FALSE) +
  
  geom_segment(data = df_equilibrio,
               aes(x = k, y = 0, xend = k, yend = y, color = Cor),
               linetype = "dotted", show.legend = FALSE) +
  
  geom_label_repel(data = df_equilibrio,
                   aes(x = k, y = y, label = Label),
                   box.padding = 0.5, point.padding = 0.3,
                   size = 3.5, fontface = "bold") +
  
  scale_color_manual(
    values = c("Produção f(k)" = "#1f77b4",
               "Investimento s·f(k)" = "#2ca02c",
               "Reposição (δ+n)k" = "#d62728",
               "Reposição δk" = "#9467bd",
               "Inicial" = "#9467bd",
               "Novo" = "#d62728"),
    breaks = c("Produção f(k)", "Investimento s·f(k)", "Reposição (δ+n)k", "Reposição δk")
  ) +
  
  scale_linetype_manual(values = c("Solid" = "solid", "Dashed" = "dashed")) +
  
  labs(
    title = "Solow Growth Model - Momento 2",
    subtitle = "Impacto do crescimento populacional: deslocamento de k*=8,51 para k*=7,13",
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


# grafico de transicao no tempo

t_max <- 150
k_t <- numeric(t_max)
y_t <- numeric(t_max)
k_t[1] <- 8.505172718

for(t in 1:(t_max - 1)) {
  y_t[t] <- k_t[t]^alpha
  k_t[t+1] <- k_t[t] + (s * y_t[t]) - ((delta + n) * k_t[t])
}
y_t[t_max] <- k_t[t_max]^alpha

df_tempo <- data.frame(
  Tempo = 1:t_max,
  Capital = k_t
)

k_star1_transicao <- 8.505172718
k_star2_transicao <- 7.127781101

grafico_tempo <- ggplot(df_tempo, aes(x = Tempo, y = Capital)) +
  geom_line(color = "#1f77b4", linewidth = 1.3) +
  
  geom_hline(yintercept = k_star1_transicao, linetype = "dashed",
             color = "#9467bd", linewidth = 1, alpha = 0.7) +
  geom_hline(yintercept = k_star2_transicao, linetype = "dashed",
             color = "#d62728", linewidth = 1, alpha = 0.7) +
  
  annotate("text", x = Inf, y = k_star1_transicao, label = "k*=8,51 (inicial)",
           vjust = -0.5, hjust = 1.1, size = 3.5, color = "#9467bd", fontface = "bold") +
  annotate("text", x = Inf, y = k_star2_transicao, label = "k*=7,13 (novo)",
           vjust = -0.5, hjust = 1.1, size = 3.5, color = "#d62728", fontface = "bold") +
  
  geom_point(data = df_tempo[1, ], aes(x = Tempo, y = Capital),
             color = "#9467bd", size = 4, shape = 21, fill = "#d9b3ff") +
  
  labs(
    title = "Trajetória de Transição do Capital - Momento 2",
    subtitle = "Convergência para o novo estado estacionário com crescimento populacional",
    x = "Tempo (Períodos)",
    y = "Capital por Trabalhador (k)"
  ) +
  
  scale_y_continuous(limits = c(6.8, 8.8)) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

grid.arrange(grafico_classico, grafico_tempo, ncol = 2)
