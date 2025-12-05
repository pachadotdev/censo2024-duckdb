# 1: packages ----

if (!require(archive)) install.packages("archive", repos = "http://cran.r-project.org")
if (!require(arrow)) install.packages("arrow", repos = "http://cran.r-project.org")
if (!require(duckdb)) install.packages("duckdb", repos = "http://cran.r-project.org")
if (!require(readxl)) install.packages("readxl", repos = "http://cran.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.r-project.org")
if (!require(dbplyr)) install.packages("dbplyr", repos = "http://cran.r-project.org")
if (!require(janitor)) install.packages("janitor", repos = "http://cran.r-project.org")
if (!require(stringr)) install.packages("stringr", repos = "http://cran.r-project.org")
if (!require(purrr)) install.packages("purrr", repos = "http://cran.r-project.org")

library(archive)
library(arrow)
library(duckdb)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(purrr)

# 2: raw data ----

url <- "https://storage.googleapis.com/bktdescargascenso2024/viv_hog_per_censo2024.zip"
zip <- paste0("", basename(url))

if (!file.exists(zip)) {
  download.file(url, zip, mode = "wb")
}

archive_extract(zip, dir = dirname(zip))

read_dic <- function(file = "diccionario_variables_censo2024.xlsx", sheet) {
  read_excel(file, sheet = sheet) |>
    clean_names()
}

codigos_personas <- read_dic(sheet = "tabla_personas")

codigos_personas |>
  filter(nombre_variable == "cine11")

codigos_hogares <- read_dic(sheet = "tabla_hogares")

codigos_viviendas <- read_dic(sheet = "tabla_viviendas")

codigos_territoriales <- read_dic(sheet = "codigos_territoriales")

codigos_territoriales_especificos <- read_dic(sheet = "cod_territoriales_especificos")

personas <- read_parquet("personas_censo2024.parquet")

hogares <- read_parquet("hogares_censo2024.parquet")

viviendas <- read_parquet("viviendas_censo2024.parquet")

# for all chr-type columns, replace multiple spaces / new lines with single space, trim leading/trailing spaces
clean_chr_cols <- function(df) {
  chr_cols <- names(df)[sapply(df, is.character)]
  df <- df |> mutate(across(all_of(chr_cols), ~ str_squish(str_trim(.))))
  return(df)
}

codigos_personas <- clean_chr_cols(codigos_personas) |>
  select(-entidad)
codigos_hogares <- clean_chr_cols(codigos_hogares) |>
  select(-entidad)
codigos_viviendas <- clean_chr_cols(codigos_viviendas) |>
  select(-entidad)

personas <- clean_chr_cols(personas)
hogares <- clean_chr_cols(hogares)
viviendas <- clean_chr_cols(viviendas)

# 3: problem with "region" ----

glimpse(personas)

personas_educacion <- personas |>
  group_by(region, cine11) |>
  summarise(total = n(), .groups = "drop") |>
  collect() |>

  inner_join(
    codigos_personas |>
        filter(nombre_variable == "cine11") |>
        select(cine11 = valor, nivel_educacional = etiqueta_de_categoria) |>
        mutate(cine11 = as.integer(cine11))
  ) |>

  inner_join(
    codigos_territoriales |>
        select(region = codigo_territorial, nombre_region = territorio) |>
        mutate(region = as.integer(region))
  )

codigos_territoriales |>
    filter(codigo_territorial %in% 1:16) |>
    group_by(codigo_territorial) |>
    filter(n() > 1)

personas_educacion <- personas |>
  group_by(comuna, cine11) |>
  summarise(total = n(), .groups = "drop") |>
  collect() |>

  inner_join(
    codigos_personas |>
        filter(nombre_variable == "cine11") |>
        select(cine11 = valor, nivel_educacional = etiqueta_de_categoria) |>
        mutate(cine11 = as.integer(cine11))
  ) |>

  inner_join(
    codigos_territoriales |>
        select(comuna = codigo_territorial, nombre_comuna = territorio) |>
        mutate(comuna = as.integer(comuna))
  )

personas_educacion <- personas_educacion |>
  mutate(
    comuna = str_pad(comuna, width = 5, side = "left", pad = "0"),
    region = str_sub(comuna, 1, 2),
    nombre_region = case_when(
      region == "15" ~ "Región de Arica y Parinacota",
      region == "01" ~ "Región de Tarapacá",
      region == "02" ~ "Región de Antofagasta",
      region == "03" ~ "Región de Atacama",
      region == "04" ~ "Región de Coquimbo",
      region == "05" ~ "Región de Valparaíso",
      region == "13" ~ "Región Metropolitana de Santiago",
      region == "06" ~ "Región del Libertador General Bernardo O'Higgins",
      region == "07" ~ "Región del Maule",
      region == "16" ~ "Región de Ñuble",
      region == "08" ~ "Región del Bío Bío",
      region == "09" ~ "Región de La Araucanía",
      region == "14" ~ "Región de Los Ríos",
      region == "10" ~ "Región de Los Lagos",
      region == "11" ~ "Región de Aysén del General Carlos Ibáñez del Campo",
      region == "12" ~ "Región de Magallanes y de la Antártica Chilena",
      TRUE ~ "Desconocida"
    )
  )

personas_educacion |> 
    filter(nombre_comuna == "Vitacura") |>
    group_by(nivel_educacional) |>
    summarise(n_vitacura = sum(total)) |>
    inner_join(
      personas_educacion |> 
        filter(nombre_comuna == "La Pintana") |>
        group_by(nivel_educacional) |>
        summarise(n_la_pintana = sum(total))
    )

# 4: quick verification: N per region ----

personas_educacion |> 
  group_by(nombre_region) |>
  summarise(n = sum(total)) |>
  arrange(n) |>
  mutate(p = n / sum(n) * 100)

# 5: proper fix ----

personas <- personas |>
  mutate(
    comuna = str_pad(comuna, width = 5, side = "left", pad = "0"),
    # NOTE: region and provincia removed - derive with substr(comuna, 1, 2) and substr(comuna, 1, 3)
    # Convert to VARCHAR and pad to 5 digits for FK to codigos_territoriales_especificos
    # First clean, then only pad codes of length 4 (comunas that need leading zero)
    # Replace NA with "-999" for consistency with census data and FK constraints
    p24_lug_resid5_esp = case_when(
      is.na(p24_lug_resid5_esp) ~ "-999",
      TRUE ~ str_squish(str_trim(as.character(p24_lug_resid5_esp)))
    ),
    p24_lug_resid5_esp = case_when(
      p24_lug_resid5_esp == "-999" ~ "-999",
      str_detect(p24_lug_resid5_esp, "^[0-9]+$") & str_length(p24_lug_resid5_esp) == 4 ~ str_pad(p24_lug_resid5_esp, width = 5, side = "left", pad = "0"),
      TRUE ~ p24_lug_resid5_esp
    ),
    p25_lug_nacimiento_esp = case_when(
      is.na(p25_lug_nacimiento_esp) ~ "-999",
      TRUE ~ str_squish(str_trim(as.character(p25_lug_nacimiento_esp)))
    ),
    p25_lug_nacimiento_esp = case_when(
      p25_lug_nacimiento_esp == "-999" ~ "-999",
      str_detect(p25_lug_nacimiento_esp, "^[0-9]+$") & str_length(p25_lug_nacimiento_esp) == 4 ~ str_pad(p25_lug_nacimiento_esp, width = 5, side = "left", pad = "0"),
      TRUE ~ p25_lug_nacimiento_esp
    ),
    p27_nacionalidad_esp = case_when(
      is.na(p27_nacionalidad_esp) ~ "-999",
      TRUE ~ str_squish(str_trim(as.character(p27_nacionalidad_esp)))
    ),
    p27_nacionalidad_esp = case_when(
      p27_nacionalidad_esp == "-999" ~ "-999",
      str_detect(p27_nacionalidad_esp, "^[0-9]+$") & str_length(p27_nacionalidad_esp) == 4 ~ str_pad(p27_nacionalidad_esp, width = 5, side = "left", pad = "0"),
      TRUE ~ p27_nacionalidad_esp
    ),
    p44_lug_trab_esp = case_when(
      is.na(p44_lug_trab_esp) ~ "-999",
      TRUE ~ str_squish(str_trim(as.character(p44_lug_trab_esp)))
    ),
    p44_lug_trab_esp = case_when(
      p44_lug_trab_esp == "-999" ~ "-999",
      str_detect(p44_lug_trab_esp, "^[0-9]+$") & str_length(p44_lug_trab_esp) == 4 ~ str_pad(p44_lug_trab_esp, width = 5, side = "left", pad = "0"),
      TRUE ~ p44_lug_trab_esp
    )
  ) |>
  select(-any_of(c("region", "provincia")))

codigos_territoriales <- codigos_territoriales |>
  mutate(
    codigo_territorial = case_when(
      codigo_territorial <= 16 ~ str_pad(codigo_territorial, width = 2, side = "left", pad = "0"),
      codigo_territorial > 16 & codigo_territorial <= 99 ~ str_pad(codigo_territorial, width = 3, side = "left", pad = "0"),
      str_length(codigo_territorial) == 4 ~ str_pad(codigo_territorial, width = 5, side = "left", pad = "0"),
      TRUE ~ as.character(codigo_territorial)
    ),
    codigo_territorial = case_when(
      territorio == "Arica y Parinacota" & str_length(codigo_territorial) == 2 ~ "15",
      territorio == "Tarapacá" & str_length(codigo_territorial) == 2 ~ "01",
      territorio == "Iquique" & str_length(codigo_territorial) == 2 ~ "011",
      territorio == "Del Tamarugal" & str_length(codigo_territorial) == 2 ~ "014",
      territorio == "Antofagasta" & str_length(codigo_territorial) == 2 ~ "02",
      territorio == "Atacama" & str_length(codigo_territorial) == 2 ~ "03",
      territorio == "Coquimbo" & str_length(codigo_territorial) == 2 ~ "04",
      territorio == "Valparaíso" & str_length(codigo_territorial) == 2 ~ "05",
      territorio == "Metropolitana de Santiago" & str_length(codigo_territorial) == 2 ~ "13",
      territorio == "Libertador General Bernardo O'Higgins" & str_length(codigo_territorial) == 2 ~ "06",
      territorio == "Maule" & str_length(codigo_territorial) == 2 ~ "07",
      territorio == "Ñuble" & str_length(codigo_territorial) == 2 ~ "16",
      territorio == "Biobío" & str_length(codigo_territorial) == 2 ~ "08",
      territorio == "La Araucanía" & str_length(codigo_territorial) == 2 ~ "09",
      territorio == "Los Ríos" & str_length(codigo_territorial) == 2 ~ "14",
      territorio == "Los Lagos" & str_length(codigo_territorial) == 2 ~ "10",
      territorio == "Aysén del General Carlos Ibáñez del Campo" & str_length(codigo_territorial) == 2 ~ "11",
      territorio == "Magallanes y de la Antártica Chilena" & str_length(codigo_territorial) == 2 ~ "12",
      TRUE ~ codigo_territorial
    )
  )

codigos_territoriales |>
  filter(str_length(codigo_territorial) == 2) |>
  arrange(codigo_territorial) |>
  print(n = 30)

codigos_territoriales |>
  distinct()

# similar for codigos_territoriales_especificos
# len 4 -> add 0, and ensure character type

codigos_territoriales_especificos <- codigos_territoriales_especificos |>
  mutate(
    codigo_especifico = as.character(codigo_especifico),
    codigo_especifico = case_when(
      str_length(codigo_especifico) == 4 ~ str_pad(codigo_especifico, width = 5, side = "left", pad = "0"),
      TRUE ~ codigo_especifico
    )
  )

# The p24, p25, p27, p44 columns contain BOTH comuna codes (from codigos_territoriales)
# AND country/special codes (from codigos_territoriales_especificos)
# We need to add all comuna codes to codigos_territoriales_especificos for FK to work
# Also add special codes: -66 (anonimizado), -99 (no respuesta)
codigos_territoriales_especificos <- codigos_territoriales_especificos |>
  bind_rows(
    codigos_territoriales |>
      filter(str_length(codigo_territorial) == 5) |>
      select(codigo_especifico = codigo_territorial, territorio_especifico = territorio)
  ) |>
  bind_rows(
    tibble(
      codigo_especifico = c("-66", "-99", "-999"),
      territorio_especifico = c("Anonimizado", "No respuesta", "No aplica")
    )
  ) |>
  distinct(codigo_especifico, .keep_all = TRUE)

# Verify comuna codes are in the reference table
stopifnot("13104" %in% codigos_territoriales_especificos$codigo_especifico)
stopifnot("07101" %in% codigos_territoriales_especificos$codigo_especifico)

codigos_territoriales_especificos |>
  filter(codigo_especifico == "13104")

codigos_territoriales_especificos |>
  filter(codigo_especifico == "07101")

# 5: organize in SQL

# same fix as for personas

personas

hogares <- hogares |>
  mutate(
    comuna = str_pad(comuna, width = 5, side = "left", pad = "0")
  ) |>
  select(-any_of(c("region", "provincia")))

viviendas <- viviendas |>
  mutate(
    comuna = str_pad(comuna, width = 5, side = "left", pad = "0")
  ) |>
  select(-any_of(c("region", "provincia")))

# persona: id_hogar -> hogar: id_hogar
# persona: id_vivienda -> vivienda: id_vivienda

length(unique(personas$id_hogar)) # -> add comuna to deambiguate

codigos_territoriales |>
  filter(codigo_territorial == "13104")

codigos_territoriales_especificos |>
  filter(codigo_especifico == "13104")

unlink("censo2024_corregido.duckdb")
unlink("censo2024_corregido.duckdb.wal")

con <- dbConnect(duckdb::duckdb(), "censo2024_corregido.duckdb", read_only = FALSE)

dbWriteTable(con, "personas", personas, overwrite = TRUE)
dbWriteTable(con, "hogares", hogares, overwrite = TRUE)
dbWriteTable(con, "viviendas", viviendas, overwrite = TRUE)

dbWriteTable(con, "codigos_personas", codigos_personas, overwrite = TRUE)
dbWriteTable(con, "codigos_hogares", codigos_hogares, overwrite = TRUE)
dbWriteTable(con, "codigos_viviendas", codigos_viviendas, overwrite = TRUE)

dbWriteTable(con, "codigos_territoriales", codigos_territoriales, overwrite = TRUE)
dbWriteTable(con, "codigos_territoriales_especificos", codigos_territoriales_especificos, overwrite = TRUE)

# rm(personas, hogares, viviendas,
#    codigos_personas, codigos_hogares, codigos_viviendas,
#    codigos_territoriales, codigos_territoriales_especificos)

gc()

# DuckDB doesn't support ALTER TABLE for adding constraints
# We need to recreate tables with constraints using CREATE TABLE ... AS

# First, add primary keys by recreating tables
dbExecute(con, "
  CREATE OR REPLACE TABLE codigos_territoriales_new (
    codigo_territorial VARCHAR PRIMARY KEY,
    territorio VARCHAR
  )
")

# Note: codigo_territorial uses VARCHAR because it has variable length (2, 3, or 5 chars)
# comuna columns in main tables will be converted to CHAR(5) for efficiency
dbExecute(con, "INSERT INTO codigos_territoriales_new SELECT * FROM codigos_territoriales")
dbExecute(con, "DROP TABLE codigos_territoriales")
dbExecute(con, "ALTER TABLE codigos_territoriales_new RENAME TO codigos_territoriales")

dbExecute(con, "
  CREATE OR REPLACE TABLE codigos_territoriales_especificos_new (
    codigo_especifico VARCHAR PRIMARY KEY,
    territorio_especifico VARCHAR
  )
")
dbExecute(con, "INSERT INTO codigos_territoriales_especificos_new SELECT * FROM codigos_territoriales_especificos")
dbExecute(con, "DROP TABLE codigos_territoriales_especificos")
dbExecute(con, "ALTER TABLE codigos_territoriales_especificos_new RENAME TO codigos_territoriales_especificos")

# now we copy in a similar way to provide foreign keys
# CREATE TABLE t1 (id INTEGER PRIMARY KEY, j VARCHAR);
# CREATE TABLE t2 (
#     id INTEGER PRIMARY KEY,
#     t1_id INTEGER,
#     FOREIGN KEY (t1_id) REFERENCES t1 (id)
# );

# use CREATE TABLE ... AS, then add constraints via a new table
# Get all column names and types from hogares
viviendas_schema <- dbGetQuery(con, "DESCRIBE viviendas")
hogares_schema <- dbGetQuery(con, "DESCRIBE hogares")
personas_schema <- dbGetQuery(con, "DESCRIBE personas")

# Build the column definitions dynamically
# Convert comuna from VARCHAR to CHAR(5) for fixed-width efficiency
viviendas_cols <- paste(
  sapply(1:nrow(viviendas_schema), function(i) {
    col_name <- viviendas_schema$column_name[i]
    col_type <- viviendas_schema$column_type[i]
    # Use CHAR(5) for comuna - fixed width is more efficient
    if (col_name == "comuna") col_type <- "CHAR(5)"
    paste(col_name, col_type)
  }),
  collapse = ",\n    "
)

hogares_cols <- paste(
  sapply(1:nrow(hogares_schema), function(i) {
    col_name <- hogares_schema$column_name[i]
    col_type <- hogares_schema$column_type[i]
    if (col_name == "comuna") col_type <- "CHAR(5)"
    paste(col_name, col_type)
  }),
  collapse = ",\n    "
)

personas_cols <- paste(
  sapply(1:nrow(personas_schema), function(i) {
    col_name <- personas_schema$column_name[i]
    col_type <- personas_schema$column_type[i]
    if (col_name == "comuna") col_type <- "CHAR(5)"
    paste(col_name, col_type)
  }),
  collapse = ",\n    "
)

# Create new table with PK and FK
dbExecute(con, paste0("
  CREATE OR REPLACE TABLE viviendas_new (
    ", viviendas_cols, ",
    FOREIGN KEY (comuna) REFERENCES codigos_territoriales (codigo_territorial),
    PRIMARY KEY (id_vivienda, comuna)
  )
"))

dbExecute(con, paste0("
  CREATE OR REPLACE TABLE hogares_new (
    ", hogares_cols, ",
    FOREIGN KEY (comuna) REFERENCES codigos_territoriales (codigo_territorial),
    PRIMARY KEY (id_hogar, id_vivienda, comuna)
  )
"))

# NULLs replaced with "-999" to maintain FK constraints
dbExecute(con, paste0("
  CREATE OR REPLACE TABLE personas_new (
    ", personas_cols, ",
    FOREIGN KEY (comuna) REFERENCES codigos_territoriales (codigo_territorial),
    FOREIGN KEY (p24_lug_resid5_esp) REFERENCES codigos_territoriales_especificos (codigo_especifico),
    FOREIGN KEY (p25_lug_nacimiento_esp) REFERENCES codigos_territoriales_especificos (codigo_especifico),
    FOREIGN KEY (p27_nacionalidad_esp) REFERENCES codigos_territoriales_especificos (codigo_especifico),
    FOREIGN KEY (p44_lug_trab_esp) REFERENCES codigos_territoriales_especificos (codigo_especifico),
    PRIMARY KEY (id_persona, id_hogar, id_vivienda, comuna)
  )
"))

dbExecute(con, "INSERT INTO viviendas_new SELECT * FROM viviendas")
dbExecute(con, "DROP TABLE viviendas")
dbExecute(con, "ALTER TABLE viviendas_new RENAME TO viviendas")

tbl(con, "codigos_territoriales_especificos") |>
  filter(codigo_especifico == "13104")

tbl(con, "codigos_territoriales_especificos") |>
  filter(codigo_especifico == "07101")

dbExecute(con, "INSERT INTO hogares_new SELECT * FROM hogares")
dbExecute(con, "DROP TABLE hogares")
dbExecute(con, "ALTER TABLE hogares_new RENAME TO hogares")

# Find all unique codes in p24/p25/p27/p44 that are NOT in codigos_territoriales_especificos
# These are likely response codes (e.g., "works at home", "abroad", etc.)
missing_codes <- dbGetQuery(con, "
  SELECT DISTINCT code FROM (
    SELECT DISTINCT p24_lug_resid5_esp AS code FROM personas
    UNION
    SELECT DISTINCT p25_lug_nacimiento_esp FROM personas
    UNION
    SELECT DISTINCT p27_nacionalidad_esp FROM personas
    UNION
    SELECT DISTINCT p44_lug_trab_esp FROM personas
  ) t
  WHERE code NOT IN (SELECT codigo_especifico FROM codigos_territoriales_especificos)
  ORDER BY code
")

# code 10 is not valid, replace with -999 in personas
dbSendQuery(con, "
  UPDATE personas
  SET p24_lug_resid5_esp = '-999'
  WHERE p24_lug_resid5_esp = '10'
")
dbSendQuery(con, "
  UPDATE personas
  SET p25_lug_nacimiento_esp = '-999'
  WHERE p25_lug_nacimiento_esp = '10'
")
dbSendQuery(con, "
  UPDATE personas
  SET p27_nacionalidad_esp = '-999'
  WHERE p27_nacionalidad_esp = '10'
")
dbSendQuery(con, "
  UPDATE personas
  SET p44_lug_trab_esp = '-999'
  WHERE p44_lug_trab_esp = '10'
")

# tbl(con, "personas_new")

# comunas <- dbGetQuery(con, "SELECT DISTINCT comuna FROM personas ORDER BY comuna")$comuna

# walk(comunas, function(com) {
#   tryCatch({
#     dbExecute(con, sprintf("INSERT INTO personas_new SELECT * FROM personas WHERE comuna = '%s'", com))
#   }, error = function(e) {
#     message(sprintf("Error in comuna %s: %s", com, e$message))
#     # Debug: find the offending values
#     debug_query <- sprintf("
#       SELECT DISTINCT p24_lug_resid5_esp AS code, 'p24' AS col FROM personas WHERE comuna = '%s' AND p24_lug_resid5_esp NOT IN (SELECT codigo_especifico FROM codigos_territoriales_especificos)
#       UNION ALL
#       SELECT DISTINCT p25_lug_nacimiento_esp, 'p25' FROM personas WHERE comuna = '%s' AND p25_lug_nacimiento_esp NOT IN (SELECT codigo_especifico FROM codigos_territoriales_especificos)
#       UNION ALL
#       SELECT DISTINCT p27_nacionalidad_esp, 'p27' FROM personas WHERE comuna = '%s' AND p27_nacionalidad_esp NOT IN (SELECT codigo_especifico FROM codigos_territoriales_especificos)
#       UNION ALL
#       SELECT DISTINCT p44_lug_trab_esp, 'p44' FROM personas WHERE comuna = '%s' AND p44_lug_trab_esp NOT IN (SELECT codigo_especifico FROM codigos_territoriales_especificos)
#     ", com, com, com, com)
#     missing <- dbGetQuery(con, debug_query)
#     print(missing)
#     stop(sprintf("Stopping at comuna %s", com))
#   })
# })

regiones <- dbGetQuery(con, "SELECT DISTINCT SUBSTR(comuna, 1, 2) AS region FROM personas ORDER BY region")$region

walk(regiones, function(reg) {
  tryCatch({
    message(sprintf("Processing region %s", reg))
    dbExecute(con, sprintf("INSERT INTO personas_new SELECT * FROM personas WHERE SUBSTR(comuna, 1, 2) = '%s'", reg))
  }, error = function(e) {
    message(sprintf("Error in region %s: %s", reg, e$message))
    stop(sprintf("Stopping at region %s", reg))
  })
})

dbExecute(con, "DROP TABLE personas")
dbExecute(con, "ALTER TABLE personas_new RENAME TO personas")

tbl(con, "personas")
tbl(con, "hogares")
tbl(con, "viviendas")

# Compact the database by exporting and reimporting
dbDisconnect(con, shutdown = TRUE)

# Export to parquet, then reimport to a fresh database
con <- dbConnect(duckdb::duckdb(), "censo2024_corregido.duckdb", read_only = FALSE)

# Create export directory
export_dir <- "censo_export_temp"
unlink(export_dir, recursive = TRUE)
dir.create(export_dir)

dbExecute(con, sprintf("EXPORT DATABASE '%s' (FORMAT PARQUET, COMPRESSION ZSTD)", export_dir))
dbDisconnect(con, shutdown = TRUE)

# Remove old database and create fresh one
unlink("censo2024_corregido.duckdb")
unlink("censo2024_corregido.duckdb.wal")

con <- dbConnect(duckdb::duckdb(), "censo2024_corregido.duckdb", read_only = FALSE)
dbExecute(con, sprintf("IMPORT DATABASE '%s'", export_dir))
dbDisconnect(con, shutdown = TRUE)

# Clean up export directory
unlink(export_dir, recursive = TRUE)

# Show final database size
db_size <- file.info("censo2024_corregido.duckdb")$size / 1024^2
message(sprintf("Database size: %.1f MB", db_size))

# Example ----

# How many Venezuelan nationals (p27_nacionalidad_esp) are there per region and which % of the population do they represent?
# How to these compare to the Peruvian nationals diaspora that moved to Chile in the 90s?

con <- dbConnect(duckdb::duckdb(), "censo2024_corregido.duckdb", read_only = TRUE)

cod_venezuela <- tbl(con, "codigos_territoriales_especificos") |>
  filter(territorio_especifico == "Venezuela (República Bolivariana de)") |>
  select(codigo_especifico) |>
  collect() |>
  pull()

cod_peru <- tbl(con, "codigos_territoriales_especificos") |>
  filter(territorio_especifico == "Perú") |>
  select(codigo_especifico) |>
  collect() |>
  pull()

venezolanos_region <- tbl(con, "personas") |>
  filter(p27_nacionalidad_esp == cod_venezuela) |>
  group_by(comuna) |>
  summarise(n_venezolanos = n(), .groups = "drop") |>  
  inner_join(
    tbl(con, "codigos_territoriales") |>
      select(comuna = codigo_territorial, nombre_comuna = territorio)
  ) |>
  mutate(
    region = substr(comuna, 1, 2)
  ) |>
  group_by(region) |>
  summarise(n_venezolanos = sum(n_venezolanos), .groups = "drop") |>

  inner_join(
    tbl(con, "personas") |>
      filter(p27_nacionalidad_esp == cod_peru) |>
      group_by(comuna) |>
      summarise(n_peruanos = n(), .groups = "drop") |>  
      inner_join(
        tbl(con, "codigos_territoriales") |>
          select(comuna = codigo_territorial, nombre_comuna = territorio)
      ) |>
      mutate(
        region = substr(comuna, 1, 2)
      ) |>
      group_by(region) |>
      summarise(n_peruanos = sum(n_peruanos), .groups = "drop") |>
      inner_join(
        tbl(con, "codigos_territoriales") |>
          select(region = codigo_territorial, nombre_region = territorio) |>
          filter(str_length(region) == 2)
      )
  ) |>

  inner_join(
    tbl(con, "personas") |>
      mutate(region = substr(comuna, 1, 2)) |>
      group_by(region) |>
      summarise(total_personas = n(), .groups = "drop")
  ) |>

  inner_join(
    tbl(con, "codigos_territoriales") |>
      select(region = codigo_territorial, nombre_region = territorio) |>
      filter(str_length(region) == 2)
  ) |>

  mutate(
    pct_venezolanos = n_venezolanos / total_personas * 100,
    pct_peruanos = n_peruanos / total_personas * 100
  ) |>
  
  select(
    region,
    nombre_region,
    n_venezolanos,
    pct_venezolanos,
    n_peruanos,
    pct_peruanos,
    total_personas
  ) |>
  
  collect()

dbDisconnect(con, shutdown = TRUE)
