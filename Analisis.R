
# 0. SETUP

options(scipen = 999) 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, stringr, readr, readxl, 
  quanteda, quanteda.textstats, 
  future, furrr, progressr,
  sjPlot, vegan, car, broom, effectsize, estimatr, performance, 
  modelsummary, flextable, ggplot2, patchwork
)

# Configuración Paralela
plan(multisession, workers = availableCores() - 1)
handlers(global = TRUE)

# --- CONFIGURACIÓN DE GRÁFICOS PERSONALIZADA (ALTO CONTRASTE + FUENTE 16) ---
windowsFonts(Times = windowsFont("Times New Roman"))
theme_set(theme_minimal(base_size = 16, base_family = "Times"))
theme_update(
  legend.position = "bottom",
  plot.title = element_text(face = "bold"),
  axis.text = element_text(color = "black") # Mejor contraste en ejes
)

# Paleta Alto Contraste (Okabe-Ito)
cols_contrast <- c("Ley 20.911" = "#D55E00", "ICCS" = "#0072B2", "Total" = "#444444")
cols_profundidad <- c("Formal" = "#D55E00", "Discursiva" = "#009E73") # Rojo/Naranja vs Verde
vec_color_cob <- function(fuente) if_else(fuente == "ICCS", "#0072B2", "#D55E00")


# 1. INPUTS


# Cargar datos
procdata_raw <- read_csv("1_tidy/procdata_v3.csv")
establecimientos <- read_xlsx("0_raw/Establecimientos_2024.xlsx")
hits_iccs <- read_csv("1_tidy/SBERT/hits_iccs.csv")
hits_ley  <- read_csv("1_tidy/SBERT/hits_ley.csv")

# Procesar procdata (Base Maestra)
procdata <- procdata_raw %>% 
  na.omit() %>% 
  mutate(Dependencia = factor(case_when(
           COD_DEPE2 %in% c(1, 5) ~ "Municipal",
           COD_DEPE2 == 2 ~ "Particular subvencionado",
           COD_DEPE2 == 3 ~ "Particular pagado"),
           levels = c("Municipal", "Particular subvencionado", "Particular pagado"))) %>% 
  select(RBD, doc_id, FORMAL_LEY, FORMAL_ICCS, DISCURSO_LEY, DISCURSO_ICCS, Dependencia)

N_total <- nrow(procdata)


# 2. ANÁLISIS REGEX Y JACCARD


# --- A. Definición de Regex ---
regex_ley <- tibble::tibble(
  objective_id = sprintf("OBJ%02d", 1:9),
  objective_name = c("Ciudadanía y deberes", "Ciudadanía crítica", "Estado de derecho", "Derechos humanos", 
                     "Diversidad y cultura", "Participación pública", "Cultura democrática", "Transparencia", "Tolerancia"),
  patterns = list(
    c("\\bderechos_ciudadanos?\\b", "\\bdeberes_ciudadanos?\\b", "\\brep[úu]blica_democr[áa]tica(s)?\\b"),
    c("\\bciudadan[íi]a_cr[íi]tica(s)?\\b", "\\bciudadan[íi]a_responsable(s)?\\b"),
    c("\\bestado_de_derecho(s)?\\b", "\\binstitucionalidad(es)?\\b"),
    c("\\bderechos_humanos?\\b", "\\bconstituci[oó]n_pol[íi]tica(s)?\\b"),
    c("\\bdiversidad_social(es)?\\b", "\\bvaloraci[oó]n_de_la_diversidad(es)?\\b"),
    c("\\binter[eé]s_p[úu]blicos?\\b"),
    c("\\bcultura_democr[áa]tica(s)?\\b", "\\b[éé]tica(s)?\\b"),
    c("\\btransparencia(s)?\\b", "\\bprobidad(es)?\\b"),
    c("\\btolerancia(s)?\\b", "\\bpluralismo(s)?\\b")
  )
)

regex_iccs <- tibble::tibble(
  domain_id = c("I_INST_ESTAT", "II_SIS_ECON", "III_SOC_CIVIL", "IV_EQUIDAD", "V_LIBERTAD", "VI_EDERECHO", 
                "VII_SOST", "VIII_SOLID", "IX_TOMA_DEC", "X_INFLUENCIA", "XI_PART_COM", "XII_ROLES_C", 
                "XIII_AUTOIMG", "XIV_CONEXION"),
  domain_name = c("Instituciones Estatales", "Sistemas Económicos", "Sociedad Civil", "Equidad", "Libertad", 
                  "Estado de Derecho ICCS", "Sostenibilidad", "Solidaridad", "Toma de Decisiones", "Influencia", 
                  "Participación Comunitaria", "Roles del Ciudadano", "Autoimagen Cívica", "Conexión Cívica"),
  patterns = list(
    c("\\binstituciones_estatales?\\b"), c("\\bsistemas?_econ[oó]micos?\\b"), c("\\bsociedad(es)?_civiles?\\b"),
    c("\\bequidad(es)?\\b"), c("\\blibertad(es)?\\b"), c("\\bsupremac[íi]a_de_la_ley(es)?\\b"),
    c("\\bsostenibilidad(es)?\\b"), c("\\bsolidaridad(es)?\\b"), c("\\btoma(s)?_de_decisiones?\\b"),
    c("\\binfluencia(s)?_p[úu]blicas?\\b"), c("\\bparticipacion_comunitaria\\b"), c("\\broles?_del_ciudadano\\b"),
    c("\\bautoimagen\\b"), c("\\bconexion_civica\\b")
  ) # (Usa tu lista completa aquí)
)

# --- B. Detección Regex ---
detect_patterns <- function(df, regex_df, col_text) {
  text_vec <- df[[col_text]]
  cols <- if("objective_id" %in% names(regex_df)) regex_df$objective_id else regex_df$domain_id
  
  with_progress({
    p <- progressor(steps = length(cols))
    future_map_dfc(seq_along(cols), function(i) {
      p()
      as.integer(str_detect(text_vec, paste(regex_df$patterns[[i]], collapse = "|")))
    }, .options = furrr_options(seed = TRUE))
  }) %>% setNames(cols)
}

MATRIZ_FORMAL_LEY <- detect_patterns(procdata, regex_ley, "FORMAL_LEY")
MATRIZ_FORMAL_ICCS <- detect_patterns(procdata, regex_iccs, "FORMAL_ICCS")

# Agregar Métricas a Procdata
procdata <- procdata %>% 
  bind_cols(
    LOC = rowSums(MATRIZ_FORMAL_LEY),
    NDC = rowSums(MATRIZ_FORMAL_ICCS),
    CoberturaLEY = rowSums(MATRIZ_FORMAL_LEY > 0),
    CoberturaICCS = rowSums(MATRIZ_FORMAL_ICCS > 0)
  )

# --- C. Cálculo Jaccard ---
dfm_ley <- as.dfm(bind_cols(doc_id = procdata$doc_id, MATRIZ_FORMAL_LEY))
dfm_iccs <- as.dfm(bind_cols(doc_id = procdata$doc_id, MATRIZ_FORMAL_ICCS))

sim_ley  <- textstat_simil(dfm_ley, method = "jaccard", margin = "documents")
sim_iccs <- textstat_simil(dfm_iccs, method = "jaccard", margin = "documents")

procdata$AvgJaccard_Ley  <- rowMeans(as.matrix(sim_ley), na.rm = TRUE)
procdata$AvgJaccard_Iccs <- rowMeans(as.matrix(sim_iccs), na.rm = TRUE)


# 3. PREPARACIÓN DE DATOS PARA GRÁFICOS


# --- Datos para P_ContenidosGenerales_F ---
proc_prof_disc_all <- bind_rows(
  hits_iccs %>% select(doc_id, tema = objective_id, sem_max) %>% mutate(Fuente = "ICCS", id_fuente = "B"),
  hits_ley  %>% rename(tema = objective_id) %>% mutate(Fuente = "Ley 20.911", id_fuente = "A")
) 

# Integrar datos formales
proc_prof_formal_all <- bind_rows(
  bind_cols(doc_id = procdata$doc_id, MATRIZ_FORMAL_LEY) %>% pivot_longer(-doc_id, names_to = "tema", values_to = "prof_formal") %>% mutate(Fuente = "Ley 20.911"),
  bind_cols(doc_id = procdata$doc_id, MATRIZ_FORMAL_ICCS) %>% pivot_longer(-doc_id, names_to = "tema", values_to = "prof_formal") %>% mutate(Fuente = "ICCS")
)

# Unir todo
proc_prof_general <- left_join(proc_prof_disc_all, proc_prof_formal_all, by = c("doc_id", "tema", "Fuente")) %>% 
  mutate(
    # Limpieza de nombres de temas (simplificada para el ejemplo)
    TemaLimpio = str_replace_all(tema, "OBJ0|Dom_|D_|I_|IV_|V_|VI_|VII_|VIII_|IX_|X_|XI_|XII_|XIII_|XIV_", ""),
    TemaLimpio = str_replace_all(TemaLimpio, "_", " "),
    Profundidad = case_when(sem_max >= 0.5 ~ "Discursiva", sem_max < 0.5 & prof_formal == 1 ~ "Formal", TRUE ~ "Ausente")
  )

# Datos agrupados para gráficos
proc_cob <- proc_prof_general %>% 
  filter(Profundidad != "Ausente") %>% 
  group_by(Fuente, TemaLimpio) %>% 
  summarise(frecuencia = n(), .groups = "drop") %>% 
  mutate(porcentaje = round(frecuencia/87.88, 1)) # Ajustar divisor según tu N real

# --- Datos para P_Profundidad_F (Ponderado y Ordenado) ---
proc_prof_GroupProfTema <- proc_prof_general %>% 
  filter(Profundidad != "Ausente") %>% 
  group_by(Fuente, TemaLimpio, Profundidad) %>% 
  summarise(Conteo = n(), .groups = "drop")

MaxConteo_Tema <- proc_prof_GroupProfTema %>% group_by(TemaLimpio) %>% summarise(NMax = sum(Conteo))
Orden <- proc_prof_GroupProfTema %>% left_join(MaxConteo_Tema, by = "TemaLimpio") %>% arrange(desc(NMax)) %>% mutate(Orden = dense_rank(desc(NMax)))

proc_prof_ponderado <- proc_prof_GroupProfTema %>% 
  left_join(MaxConteo_Tema, by = "TemaLimpio") %>% 
  left_join(Orden %>% select(TemaLimpio, Orden) %>% distinct(), by = "TemaLimpio") %>% 
  mutate(
    PorConteo = round((Conteo / NMax) * 100, 1),
    # Lógica de posición de etiquetas y colores del original
    Pos_Y = case_when(Profundidad == "Formal" ~ 30, Profundidad == "Discursiva" ~ 70, TRUE ~ 50),
    Color_Texto = ifelse(Profundidad == "Discursiva", "white", "black") # Ajuste para contraste
  )

# --- Datos para P_ProfundidadCoseno_F ---
proc_prof_CosenoTotal <- proc_prof_general %>% 
  filter(sem_max >= 0.5) %>% 
  select(doc_id, Fuente, sem_max) %>% 
  bind_rows(mutate(., Fuente = "Total")) %>% 
  mutate(Fuente = factor(Fuente, levels = c("Ley 20.911", "ICCS", "Total")))


# 4. PLOTS


# 1. P_ContenidosGenerales_F
P_ContenidosGenerales_F <- ggplot(proc_cob, aes(x = reorder(TemaLimpio, frecuencia), y = frecuencia, fill = Fuente)) +
  geom_col(color = "black", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = cols_contrast) +
  geom_text(aes(label = paste0(frecuencia, " | ", porcentaje, "%")), 
            hjust = -0.1, family = "Times", size = 5, fontface = "bold") +
  labs(x = NULL, y = "Frecuencia | Porcentaje de Cobertura", fill = NULL, 
       caption = "Fuente: Elaboración propia") +
  theme(
    axis.text.y = element_text(color = ifelse(proc_cob$Fuente == "ICCS", "#0072B2", "#D55E00")),
    panel.grid.major.y = element_blank()
  )

# 2. p_jaccardV2_F
dat_jaccard <- procdata %>% 
  select(Ley = AvgJaccard_Ley, ICCS = AvgJaccard_Iccs) %>% 
  pivot_longer(everything(), names_to = "Fuente", values_to = "Similitud") %>% 
  mutate(Fuente = ifelse(Fuente == "Ley", "Ley 20.911", "ICCS"))

p_jaccardV2_F <- ggplot(dat_jaccard, aes(x = Fuente, y = Similitud, fill = Fuente)) +
  geom_violin(alpha = 0.8, trim = FALSE, color = "black", size = 0.8) + # Bordes más gruesos
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  scale_fill_manual(values = cols_contrast) +
  labs(x = NULL, y = "Similitud Jaccard (0 - 1)", caption = "Fuente: Elaboración propia") +
  theme(legend.position = "none")

# 3. P_Profundidad_F
P_Profundidad_F <- ggplot(proc_prof_ponderado, aes(x = reorder(TemaLimpio, -Orden), y = PorConteo, fill = Profundidad)) +
  geom_col(color = "black", width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = cols_profundidad) +
  geom_label(
    aes(label = paste0(Conteo, " | ", PorConteo, "%"), y = Pos_Y, color = Profundidad),
    fill = "white", label.size = 0, size = 4, show.legend = FALSE, family = "Times", fontface = "bold"
  ) +
  scale_color_manual(values = cols_profundidad) + # Texto del mismo color que la barra para consistencia visual
  labs(x = NULL, y = "Porcentaje Relativo (Intra-Tema)", fill = NULL, caption = "Fuente: Elaboración propia") +
  theme(legend.position = "bottom")

# 4. P_ProfundidadCoseno_F
P_ProfundidadCoseno_F <- ggplot(proc_prof_CosenoTotal, aes(x = Fuente, y = sem_max, fill = Fuente)) +
  geom_violin(trim = FALSE, alpha = 0.9, color = "black", size = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  scale_fill_manual(values = cols_contrast) +
  labs(x = NULL, y = "Similitud de Coseno (SBERT)", caption = "Fuente: Elaboración propia") +
  theme(legend.position = "none")


# 5. TABLAS T-TEST (Objetos Flextable)


# Datos base para pruebas T
t_datos <- procdata %>% 
  mutate(PorLEY = CoberturaLEY/0.09, PorICCS = CoberturaICCS/0.14) # Normalización

t_datos_prof <- proc_prof_general %>% 
  group_by(doc_id) %>% 
  summarise(
    NFormal = sum(Profundidad == "Formal"),
    NDiscursiva = sum(Profundidad == "Discursiva"),
    Total = NFormal + NDiscursiva,
    PorFormal = ifelse(Total > 0, (NFormal*100)/Total, 0),
    PorDiscursiva = ifelse(Total > 0, (NDiscursiva*100)/Total, 0)
  )

# Funciones auxiliares para formato
crear_tabla_t <- function(t_obj, nombre1, nombre2, media1, media2, sd1, sd2, tipo_tabla) {
  df_res <- data.frame(
    Contenido = c(nombre1, nombre2),
    Media = c(media1, media2),
    SD = c(sd1, sd2),
    PruebaT = t_obj$statistic,
    GL = as.integer(t_obj$parameter),
    P_fmt = ifelse(t_obj$p.value < 0.001, "< .001", sub("0.", ".", formatC(t_obj$p.value, format = "f", digits = 3))),
    IC_Inf = t_obj$conf.int[1],
    IC_Sup = t_obj$conf.int[2]
  ) %>% 
  mutate(across(c(Media, SD, PruebaT, IC_Inf, IC_Sup), ~ formatC(.x, format = "f", digits = 2)))
  
  flextable(df_res) %>% 
    set_header_labels(Media = "Media", SD = "DE", PruebaT = "t", GL = "gl", P_fmt = "p", IC_Inf = "LL 95%", IC_Sup = "UL 95%") %>% 
    merge_v(j = c("PruebaT", "GL", "P_fmt", "IC_Inf", "IC_Sup")) %>% 
    theme_apa() %>% 
    font(fontname = "Times New Roman", part = "all") %>% 
    fontsize(size = 12, part = "all") %>% 
    autofit()
}

# 1. T-Test Contenidos (Cobertura)
t_contenidos <- t.test(t_datos$PorLEY, t_datos$PorICCS, paired = TRUE)
tabla_t_contenidos_exp <- crear_tabla_t(t_contenidos, "Legal", "Extra-Legal", 
                                        mean(t_datos$PorLEY), mean(t_datos$PorICCS), 
                                        sd(t_datos$PorLEY), sd(t_datos$PorICCS))

# 2. T-Test Profundidad Ideal (Conteos absolutos)
t_prof_ideal <- t.test(t_datos_prof$NFormal, t_datos_prof$NDiscursiva, paired = TRUE)
tabla_t_prof_ideal_exp <- crear_tabla_t(t_prof_ideal, "Formal (N)", "Discursiva (N)", 
                                        mean(t_datos_prof$NFormal), mean(t_datos_prof$NDiscursiva), 
                                        sd(t_datos_prof$NFormal), sd(t_datos_prof$NDiscursiva))

# 3. T-Test Profundidad Empírica (Porcentajes)
t_prof_emp <- t.test(t_datos_prof$PorFormal, t_datos_prof$PorDiscursiva, paired = TRUE)
tabla_t_prof_empirico_exp <- crear_tabla_t(t_prof_emp, "Formal (%)", "Discursiva (%)", 
                                           mean(t_datos_prof$PorFormal), mean(t_datos_prof$PorDiscursiva), 
                                           sd(t_datos_prof$PorFormal), sd(t_datos_prof$PorDiscursiva))

