# Funciones del ejercicio de Analisis de datos
# Cada funcion recibe la columna a analizar como argumento (prod_gas o prod_pet),
# asi el mismo codigo sirve para el gas y para el petroleo.

# Produccion total y market share de cada empresa en todo el periodo,
# ordenado de mayor a menor.
share_total <- function(datos, variable) {datos %>% group_by(empresa) %>% summarise(produccion = sum({{ variable }})) %>%
mutate(market_share = produccion / sum(produccion)) %>% arrange(desc(produccion))}

# Produccion y market share de cada empresa, año por año.
share_anual <- function(datos, variable) {datos %>% group_by(anio, empresa) %>%
summarise(produccion = sum({{ variable }}), .groups = "drop_last") %>%mutate(market_share = produccion / sum(produccion)) %>% ungroup()
}

# Produccion anual como indice: (produccion en t / produccion en el año base) * 100.
indice_base <- function(datos, variable, anio_base = 2014) {datos %>% group_by(anio) %>% summarise(produccion = sum({{ variable }})) %>%
mutate(crecimiento = produccion / produccion[anio == anio_base] * 100)}

# Evolucion del market share de las empresas del top, una linea por empresa.
# `shares` es la salida de share_anual().
grafico_share_lineas <- function(shares, top, titulo) {
  shares %>% filter(empresa %in% top) %>% ggplot(aes(x = anio, y = market_share, color = empresa)) +
    geom_line() + labs(x = "Año", y = "Market share", color = "Empresa", title = titulo)
}

# Composicion del mercado como stacked area: las empresas del top por separado
# y el resto agrupado en "Otros". Cada año suma 1.
# complete() pone share 0 a las empresas sin produccion en un año; sin eso,
# geom_area deja huecos y el stacked area no llega a 1 en esos años.
# Cada empresa tiene siempre el mismo color (en gas y en petroleo) y "Otros" va en gris.
# El orden de apilado sigue al top (YPF abajo, "Otros" arriba).
colores_empresas <- c("YPF S.A." = "#2a78d6", "PAN AMERICAN ENERGY SL" = "#eb6834",
  "VISTA ENERGY ARGENTINA SAU" = "#1baf7a", "SHELL ARGENTINA S.A." = "#eda100",
  "PLUSPETROL S.A." = "#e87ba4", "TECPETROL S.A." = "#008300", "TOTAL AUSTRAL S.A." = "#6250d6",
  "PAMPA ENERGIA S.A." = "#e34948", "Otros" = "#c9c8c2")
nombres_cortos <- c("YPF S.A." = "YPF", "PAN AMERICAN ENERGY SL" = "Pan American",
  "VISTA ENERGY ARGENTINA SAU" = "Vista", "SHELL ARGENTINA S.A." = "Shell",
  "PLUSPETROL S.A." = "Pluspetrol", "TECPETROL S.A." = "Tecpetrol",
  "TOTAL AUSTRAL S.A." = "Total Austral", "PAMPA ENERGIA S.A." = "Pampa Energia", "Otros" = "Otros")

grafico_share_area <- function(shares, top, titulo) {
  shares %>% mutate(empresa = if_else(empresa %in% top, empresa, "Otros")) %>%
    group_by(anio, empresa) %>% summarise(market_share = sum(market_share), .groups = "drop") %>%
    complete(anio, empresa, fill = list(market_share = 0)) %>%
    mutate(empresa = factor(empresa, levels = c(top, "Otros"))) %>%
    ggplot(aes(x = anio, y = market_share, fill = empresa)) +
    geom_area(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.2) +
    scale_fill_manual(values = colores_empresas, labels = function(x) nombres_cortos[x]) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(2014, 2025, 2), expand = c(0, 0)) +
    labs(x = NULL, y = "Market share", fill = NULL, title = titulo,
         caption = "Fuente: Secretaria de Energia. Share por volumen producido, empresa operadora.") +
    theme_minimal() + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
}
