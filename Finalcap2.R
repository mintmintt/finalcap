
library(tm)
library(RSQLite)
source('Finalcap.R')

db <- dbConnect(SQLite(), dbname="shiny-app/train.db")
dbSendQuery(conn=db,
            "CREATE TABLE NGram
            (pre TEXT,
            word TEXT,
            freq INTEGER,
            n INTEGER)")  # ROWID automatically created with SQLite dialect

# Get word frequencies
freq_5 <- tdmToFreq(tdm_5)
freq_4 <- tdmToFreq(tdm_4)
freq_3 <- tdmToFreq(tdm_3)
freq_2 <- tdmToFreq(tdm_2)

# Process with pre and current word
processGram(freq_5)
processGram(freq_4)
processGram(freq_3)
processGram(freq_2)

# Insert into SQLite database
sql_5 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 5)"
bulk_insert(sql_5, freq_5)
sql_4 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 4)"
bulk_insert(sql_4, freq_4)
sql_3 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 3)"
bulk_insert(sql_3, freq_3)
sql_2 <- "INSERT INTO NGram VALUES ($pre, $cur, $freq, 2)"
bulk_insert(sql_2, freq_2)

dbDisconnect(db)
