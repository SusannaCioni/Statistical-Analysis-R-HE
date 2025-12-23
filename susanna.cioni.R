# Installo le Librerie ----
install.packages("tidyverse")    
install.packages("haven")        
install.packages("gt")
install.packages("officer")
install.packages("flextable")
install.packages("broom")
install.packages("readstata13")
library(tidyverse)
library(haven)
library(gt)
library(officer)
library(flextable)
library(estimatr)
library(gt)
library(dplyr)
library(estimatr)
library(broom)
library(stringr)
library(readstata13)

#Replico Table A1----

df <- read_dta("C:/Users/susanna.cioni/Downloads/Dati_paper_IJSE.dta") |>
  mutate(across(where(is.labelled), as_factor))

# Controllo struttura e valori unici
df |> glimpse()
df |> map(~ unique(.[!is.na(.)]))

# Ricodifica "Residential status" aggregata per Table A1
df <- df |> 
  mutate(condd_A1 = case_when(
    condd == "Away" ~ "Away from home",
    condd %in% c("Commuter", "Home-town") ~ "Hometown (and commuter)",
    TRUE ~ NA_character_
  ))

# Etichette leggibili per ciascuna variabile
label_maps_a1 <- list(
  dum_genere1 = c("0" = "Male", "1" = "Female"),
  ciclolaurea = c(
    "Laurea Triennale" = "Bachelor’s",
    "Laurea Magistrale, Specialistica, V.O." = "Master’s",
    "Laurea Ciclo Unico" = "Unique cycle"
  ),
  field = c(
    "Humanities" = "Humanities",
    "Social Sciences" = "Social Sciences",
    "Stem" = "STEM",
    "Health" = "Health"
  ),
  sedecdl2 = c(
    "Bologna" = "Bologna",
    "Other cities" = "Other campuses"
  ),
  condd_A1 = c(
    "Away from home" = "Away from home",
    "Hometown (and commuter)" = "Hometown (and commuter)"
  )
)

# Etichette delle sezioni per la colonna "Variable"
var_labels_a1 <- c(
  dum_genere1 = "Sex",
  ciclolaurea = "Type of degree",
  field = "Field of study",
  sedecdl2 = "Course location (Campus)",
  condd_A1 = "Residential status"
)

# Funzione per tabella pulita
table_a1_clean <- function(data, var, label_map = NULL) {
  tab <- data |>
    filter(!is.na(!!sym(var))) |> 
    count(!!sym(var)) |>
    mutate(Percent = round(n / sum(n) * 100, 1)) |> 
    rename(Category_raw = !!sym(var)) |> 
    select(Category_raw, Percent)
  
  tab$Category <- if (!is.null(label_map)) {
    recode(tab$Category_raw, !!!label_map)
  } else {
    tab$Category_raw
  }
  
  tab$Variable <- var_labels_a1[[var]]
  tab <- tab |> select(Variable, Category, Percent)
  tab$Variable[-1] <- ""  # Mostra solo la prima riga della sezione
  
  return(tab)
}

# Tabella finale
table_a1_final <- map_dfr(
  names(label_maps_a1),
  ~ table_a1_clean(df, .x, label_map = label_maps_a1[[.x]])
)

# Visualizzazione in Viewer
table_a1_final |>
  gt() |>
  tab_header(title = "Table A1 – Descriptive statistics on sociodemographic characteristics") |>
  cols_label(
    Variable = "Main variables",
    Category = "Category",
    Percent = "HousINgBO sample (%)"
  ) |>
  fmt_number(columns = Percent, decimals = 1)

#course location è binaria, per questo aggrega i campus, nel dataset non abbiamo accesso al dettaglio dei vari campus

# Replico Table A2----
# Etichette leggibili
label_maps_a2 <- list(
  didattica_preferenza = c(
    "In person" = "Face-to-face",
    "Blended" = "Blended",
    "Online" = "Fully online"
  ),
  edufam = c(
    "Tertiary" = "Tertiary degree",
    "Upper secondary" = "Upper secondary diploma",
    "Compulsory" = "Compulsory education"
  ),
  employ3 = c(
    "Full/part-time job" = "Full-time and part-time jobs",
    "Occasional job" = "Occasional jobs",
    "No job" = "No job"
  ),
  condd = c(
    "Away" = "Away from home",
    "Commuter" = "Commuter",
    "Home-town" = "Hometown"
  ),
  cleta2 = c(
    "Less than 24" = "Less than or equal to 24",
    "Greater than 25" = "Greater than or equal to 25"
  )
)

# Etichette delle variabili
var_labels <- c(
  didattica_preferenza = "Teaching mode",
  edufam = "Parental education",
  employ3 = "Employment",
  condd = "Students' condition",
  cleta2 = "Age group"
)

# Funzione per creare blocchi di tabella escludendo gli NA
table_a2_clean <- function(data, var, label_map = NULL) {
  df_clean <- data
  
  # Escludi NA e "Foreign" per edufam
  if (var == "edufam") {
    df_clean <- df_clean |> filter(!is.na(edufam), edufam != "Foreign")
  } else {
    df_clean <- df_clean |> filter(!is.na(!!sym(var)))
  }
  
  tab <- df_clean |> 
    count(!!sym(var)) |> 
    mutate(Percent = round(n / sum(n) * 100, 1)) |> 
    rename(Category_raw = !!sym(var)) |> 
    select(Category_raw, Percent)
  
  if (!is.null(label_map)) {
    tab$Category <- recode(tab$Category_raw, !!!label_map)
  } else {
    tab$Category <- tab$Category_raw
  }
  
  tab$Variable <- var_labels[[var]]
  tab <- tab |> select(Variable, Category, Percent)
  tab$Variable[-1] <- ""  # Mostra nome variabile solo nella prima riga
  
  return(tab)
}
# Costruzione tabella finale unificata
table_rows <- map_dfr(
  names(label_maps_a2),
  ~ table_a2_clean(df, .x, label_map = label_maps_a2[[.x]])
)

# Visualizza la tabella nel Viewer
table_rows |>
  gt() |>
  tab_header(title = "Table A2 – Descriptive statistics for the outcome and main independent variables") |>
  cols_label(
    Variable = "Main variables",
    Category = "Category",
    Percent = "HousINgBO sample (%)"
  ) |>
  fmt_number(columns = Percent, decimals = 1)

#Replico Table A5 ----
# STEP 1 – Carica e prepara i dati
df <- read_dta("C:/Users/susanna.cioni/Downloads/Dati_paper_IJSE.dta") |>
  mutate(across(where(is.labelled), as_factor)) |>
  mutate(didattica2 = as.numeric(didattica_preferenza == "In person"))

# STEP 2 – Filtro per studenti under 25 e livelli corretti
df_a5 <- df |>
  filter(cleta2 == "Less than 24") |> 
  droplevels()

cat("Numero di osservazioni finali:", nrow(df_a5), "\n")

# STEP 3 – Stima modello con interazioni
model_a5 <- lm_robust(
  didattica2 ~ edufam * condd * employ3 + area_nascita,
  data = df_a5,
  se_type = "HC1"
)

# STEP 4 – Tabella con risultati ordinati
table_a5_clean <- tidy(model_a5) |>
  filter(term != "(Intercept)", !is.na(estimate)) |>
  mutate(
    coeff = round(estimate, 3),
    se    = round(std.error, 3),
    label = paste0(coeff, " (", se, ")")
  ) |>
  # Rinomina le variabili in etichette leggibili
  mutate(
    term_readable = case_when(
      term == "edufamUpper secondary" ~ "Upper secondary",
      term == "edufamTertiary"        ~ "Tertiary",
      term == "conddCommuter"         ~ "Commuter",
      term == "conddHome-town"        ~ "Home-town",
      term == "employ3No job"         ~ "No job",
      term == "employ3Occasional job" ~ "Occasional job",
      term == "area_nascitaCentre"    ~ "Centre",
      term == "area_nascitaSouth e Isole" ~ "South & Islands",
      term == "area_nascitaAbroad"        ~ "Abroad",
      TRUE ~ str_replace_all(term, ":", " × ")  # interazioni
    )
  )

# Ordine personalizzato
ordine <- c(
  # Main effects
  "Upper secondary", "Tertiary",
  "Commuter", "Home-town",
  "Occasional job", "No job",
  "Centre", "South & Islands", "Abroad",
  # 2-way interactions
  "edufamUpper secondary × conddCommuter",
  "edufamTertiary × conddCommuter",
  "edufamUpper secondary × conddHome-town",
  "edufamTertiary × conddHome-town",
  "edufamUpper secondary × employ3Occasional job",
  "edufamTertiary × employ3Occasional job",
  "edufamUpper secondary × employ3No job",
  "edufamTertiary × employ3No job",
  "conddCommuter × employ3Occasional job",
  "conddHome-town × employ3Occasional job",
  "conddCommuter × employ3No job",
  "conddHome-town × employ3No job",
  # 3-way interactions
  "edufamUpper secondary × conddCommuter × employ3Occasional job",
  "edufamTertiary × conddCommuter × employ3Occasional job",
  "edufamUpper secondary × conddHome-town × employ3Occasional job",
  "edufamTertiary × conddHome-town × employ3Occasional job",
  "edufamUpper secondary × conddCommuter × employ3No job",
  "edufamTertiary × conddCommuter × employ3No job",
  "edufamUpper secondary × conddHome-town × employ3No job",
  "edufamTertiary × conddHome-town × employ3No job"
)

# Rimuove eventuali elementi mancanti
ordine <- intersect(ordine, table_a5_clean$term)

table_a5_clean <- table_a5_clean |>
  mutate(term = factor(term, levels = ordine)) |>
  arrange(term) |>
  select(Covariate = term_readable, `Model 6` = label)

# STEP 5 – Visualizzazione gt
table_a5_clean |> 
  gt() |>
  tab_header(
    title = md("**Table A5**"),
    subtitle = md("*Linear regression with full interactions – Students under 25*")
  ) |>
  tab_footnote(
    footnote = "Standard errors in parentheses.",
    locations = cells_column_labels(columns = `Model 6`)
  )

#Replico Table 1 ----
install.packages("nnet")
library(nnet)
library(tidyr)

# STEP 1 – Carica e prepara il dataset 
df <- read_dta("C:/Users/susanna.cioni/Downloads/Dati_paper_IJSE.dta") |> 
  mutate(across(where(is.labelled), as_factor)) |> 
  mutate(
    didattica2 = as.numeric(didattica_preferenza == "In person")
  )

# STEP 2 – Filtro casi validi e conversione fattori 
df_model <- df |> 
  filter(
    didattica_preferenza %in% c("In person", "Blended", "Online"),
    edufam %in% c("Compulsory", "Upper secondary", "Tertiary"),
    employ3 %in% c("Full/part-time job", "Occasional job", "No job"),
    condd %in% c("Away", "Commuter", "Home-town"),
    cleta2 %in% c("Less than 24", "Greater than 25"),
    sedecdl2 %in% c("Bologna", "Other cities"),
    !is.na(field),
    !is.na(anno_corso2),
    !is.na(track),
    !is.na(cumula),
    !is.na(area_nascita)
  ) |> 
  filter(edufam != "Foreign") |> 
  mutate(across(c(
    didattica_preferenza, edufam, condd, employ3, cleta2, sedecdl2,
    dum_genere1, track, field, cumula, anno_corso2, ciclolaurea, area_nascita
  ), as.factor)) |> 
  droplevels()

# Reimposta riferimento outcome
df_model$didattica_preferenza <- relevel(df_model$didattica_preferenza, ref = "Online")

cat("Numero osservazioni finali:", nrow(df_model), "\n")

# STEP 3 – Modello multinomiale 
library(nnet)
model_multinom <- multinom(
  didattica_preferenza ~ edufam + condd + employ3 + cleta2 +
    dum_genere1 + area_nascita + track + field + cumula + anno_corso2 + sedecdl2 + ciclolaurea,
  data = df_model
)

# STEP 4 – Estrazione dei risultati 
library(broom)
tidy_multinom <- tidy(model_multinom) |> 
  mutate(
    RRR = round(exp(estimate), 3),
    SE  = round(std.error, 3)
  )

# STEP 5 – Ristrutturazione in formato largo 
table1_clean <- tidy_multinom |> 
  select(y.level, term, RRR, SE) |> 
  pivot_wider(
    names_from = y.level,
    values_from = c(RRR, SE),
    names_glue = "{.value}_{y.level}"
  ) |> 
  rename_with(~ gsub(" ", "_", .x)) |>  # per evitare problemi con spazi
  rename(
    `RRR (Face-to-face)` = RRR_In_person,
    `SE (Face-to-face)`  = SE_In_person,
    `RRR (Blended)`      = RRR_Blended,
    `SE (Blended)`       = SE_Blended
  ) |> 
  rename(Covariate = term)

# STEP 6 – Visualizzazione con gt 
library(gt)
table1_clean |> 
  gt() |>
  tab_header(
    title = md("**Table 1**"),
    subtitle = md("*Multinomial logistic regression on teaching mode. RRR and SE.*")
  ) |>
  cols_label(
    Covariate           = "Covariate",
    `RRR (Face-to-face)`= "RRR (Face-to-face)",
    `SE (Face-to-face)` = "SE",
    `RRR (Blended)`     = "RRR (Blended)",
    `SE (Blended)`      = "SE"
  ) |>
  fmt_number(columns = starts_with("RRR"), decimals = 3) |>
  fmt_number(columns = starts_with("SE"), decimals = 3) |>
  tab_footnote(
    footnote = "Standard errors in parentheses.",
    locations = cells_column_labels(columns = starts_with("SE"))
  )

# Replico Table 2 ----
install.packages("margins")
install.packages("estimatr")
library(margins)
library(estimatr)

# STEP 1 – Carica e prepara i dati
df <- read_dta("C:/Users/susanna.cioni/Downloads/Dati_paper_IJSE.dta") |> 
  mutate(across(where(is.labelled), as_factor)) |>
  mutate(
    didattica2 = as.numeric(didattica_preferenza == "Online"),
    edufam     = as_factor(edufam),
    condd      = as_factor(condd),
    employ3    = as_factor(employ3),
    cleta2     = as_factor(cleta2),
    sedecdl2   = as_factor(sedecdl2)
  )

# STEP 2 – Filtro osservazioni coerenti con il paper
df_model <- df |> 
  filter(
    didattica2 %in% c(0,1),
    edufam %in% c("Compulsory", "Upper secondary", "Tertiary"),
    condd %in% c("Away", "Commuter", "Home-town"),
    employ3 %in% c("Full/part-time job", "Occasional job", "No job"),
    cleta2 %in% c("Less than 24", "Greater than 25"),
    sedecdl2 %in% c("Bologna", "Other cities")
  ) |> 
  filter(edufam != "Foreign") |> 
  droplevels()

cat("N osservazioni finali:", nrow(df_model), "\n")

# MACROFUNZIONE – Calcolo AME per un modello logit
calcola_ame <- function(formula, model_name) {
  mod <- glm(formula, data = df_model, family = binomial)
  margins::margins(mod) |> 
    summary() |> 
    as_tibble() |> 
    filter(factor != "(Intercept)") |>
    mutate(
      Model = model_name,
      AME = round(AME, 3),
      SE  = round(SE, 3),
      Var = str_replace(factor, ".*\\.", "")  # edufam.Tertiary → Tertiary
    ) |>
    select(Model, Var, AME, SE)
}

# STEP 3 – Crea i 5 modelli incrementali
ame1 <- calcola_ame(didattica2 ~ edufam, "Model1")
ame2 <- calcola_ame(didattica2 ~ edufam + condd, "Model2")
ame3 <- calcola_ame(didattica2 ~ edufam + condd + employ3, "Model3")
ame4 <- calcola_ame(didattica2 ~ edufam + condd + employ3 + cleta2, "Model4")
ame5 <- calcola_ame(didattica2 ~ edufam + condd + employ3 + cleta2 + sedecdl2, "Model5")

# STEP 4 – Unisci in tabella larga
table2_all <- bind_rows(ame1, ame2, ame3, ame4, ame5) |> 
  pivot_wider(
    names_from = Model,
    values_from = c(AME, SE),
    names_glue = "{.value}_{Model}"
  ) |> 
  mutate(
    Group = case_when(
      Var %in% c("Upper secondary", "Tertiary") ~ "Parental education",
      Var %in% c("Commuter", "Home-town") ~ "Residential status",
      Var %in% c("Occasional job", "No job") ~ "Employment",
      Var %in% c("Less than 24") ~ "Age group",
      TRUE ~ NA
    )
  )

# STEP 5 – Visualizza Table 2 con gt
table2_all |> 
  select(Group, Var, starts_with("AME_"), starts_with("SE_")) |> 
  gt() |> 
  tab_row_group("Parental education", rows = Group == "Parental education") |>
  tab_row_group("Residential status", rows = Group == "Residential status") |>
  tab_row_group("Employment", rows = Group == "Employment") |>
  tab_row_group("Age group", rows = Group == "Age group") |>
  cols_label(
    Var = "Covariate",
    AME_Model1 = "AME", SE_Model1 = "SE",
    AME_Model2 = "AME", SE_Model2 = "SE",
    AME_Model3 = "AME", SE_Model3 = "SE",
    AME_Model4 = "AME", SE_Model4 = "SE",
    AME_Model5 = "AME", SE_Model5 = "SE"
  ) |> 
  cols_hide(columns = Group) |> 
  tab_spanner(label = "Model 1", columns = c(AME_Model1, SE_Model1)) |>
  tab_spanner(label = "Model 2", columns = c(AME_Model2, SE_Model2)) |>
  tab_spanner(label = "Model 3", columns = c(AME_Model3, SE_Model3)) |>
  tab_spanner(label = "Model 4", columns = c(AME_Model4, SE_Model4)) |>
  tab_spanner(label = "Model 5", columns = c(AME_Model5, SE_Model5)) |>
  tab_header(
    title = md("**Table 2**"),
    subtitle = md("*Binomial logistic regression on teaching mode. AME and standard errors (SE) for selected variables.*")
  ) |> 
  fmt_number(columns = starts_with("AME"), decimals = 3) |> 
  fmt_number(columns = starts_with("SE"), decimals = 3)