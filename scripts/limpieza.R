library(data.table)
dt <- data.table::fread("./datos/pbp_matches_ch_main_archive.csv")

temp <- dt[, data.table::tstrsplit(pbp, split = "[;\\.]")]
temp <- cbind(dt[, !"pbp"], temp)

dt <- melt(temp, id.vars = c("pbp_id","date","tour","draw", "tny_name", "server1", "server2", "winner"),
           measure.vars = paste0("V", seq.int(1:39)))

# Remover las filas que quedan con NA en value porque son menores a la cantidad máxima de puntos jugados en otros partidos
dt <- dt[complete.cases(dt), ]
dt[, numero := data.table::tstrsplit(variable, split = "V", fixed = TRUE)[2]
   ][, variable := NULL
     ][, numero := as.integer(numero)]
dt[, par_impar := "impar"
   ][numero %% 2 == 0, par_impar := "par"]

# remuevo tiebreak (habría que agregarlo luego)
dt <- dt[!dt[,which(stringi::stri_detect(value, regex = "/"))]]

# Itero en los pares
dt[, pct_saques := NA_real_]
dt[par_impar == "par", pct_saques := {
     strsplit(value, "") %>% 
        lapply(., function(x) {
            x <- x[!is.na(x)]
            largo = length(x)
            sum(x %in% "S")/largo
        }) %>% 
        transpose(.)
    }
    ]
# Itero en impares
dt[par_impar == "impar", pct_saques := {
    strsplit(value, "") %>% 
        lapply(., function(x) {
            x <- x[!is.na(x)]
            largo = length(x)
            sum(x %in% "S")/largo
        }) %>% 
        transpose(.)
}
]
dt[pbp_id == 2233907,]
