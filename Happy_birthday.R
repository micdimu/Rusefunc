happy_birthday <- function(name = "you",
                           cake_color = "red",
                           age = 30,
                           play_sound = TRUE,
                           animate = TRUE) {
        
        # ---------------------------------------------------------
        # Pacchetti
        # ---------------------------------------------------------
        needed <- c("plotrix", "rdist", "tuneR")
        
        for (pkg in needed) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                        install.packages(pkg, dependencies = TRUE)
                }
        }
        
        # ---------------------------------------------------------
        # Controlli
        # ---------------------------------------------------------
        if (!is.character(name) || length(name) != 1) {
                stop("'name' deve essere una stringa.")
        }
        if (!is.character(cake_color) || length(cake_color) != 1) {
                stop("'cake_color' deve essere una stringa.")
        }
        if (!is.numeric(age) || length(age) != 1 || age <= 0) {
                stop("'age' deve essere un numero positivo.")
        }
        
        age <- as.integer(age)
        
       
        # ---------------------------------------------------------
        # Funzioni grafiche
        # ---------------------------------------------------------
        draw_cake <- function() {
                plot(
                        c(0, 10), c(0, 10),
                        type = "n", bty = "n",
                        xaxt = "n", yaxt = "n",
                        xlab = "", ylab = "",
                        main = paste("Happy birthday to", name)
                )
                
                # Ombra
                plotrix::draw.ellipse(
                        x = 5, y = 1.45,
                        a = 4.2, b = 0.45,
                        col = "grey85", border = NA
                )
                
                # Corpo torta
                rect(1.3, 2.0, 8.7, 4.6, col = cake_color, border = NA)
                
                # Lati verticali
                segments(1.3, 2.0, 1.3, 4.6, lwd = 1.2, col = "grey35")
                segments(8.7, 2.0, 8.7, 4.6, lwd = 1.2, col = "grey35")
                
                # Base frontale
                plotrix::draw.ellipse(
                        x = 5, y = 2.0,
                        a = 3.7, b = 0.8,
                        col = cake_color, border = "grey35"
                )
                
                # Top torta
                plotrix::draw.ellipse(
                        x = 5, y = 4.6,
                        a = 3.7, b = 0.8,
                        col = cake_color, border = "grey35"
                )
                
                # Strato di panna sopra
                plotrix::draw.ellipse(
                        x = 5, y = 4.75,
                        a = 3.4, b = 0.55,
                        col = "ivory", border = "grey80"
                )
                
                # Colature di panna davanti
                drip_x <- c(2.0, 2.8, 3.8, 5.0, 6.1, 7.0, 7.9)
                drip_h <- c(0.45, 0.70, 0.50, 0.85, 0.55, 0.75, 0.45)
                
                for (i in seq_along(drip_x)) {
                        x <- drip_x[i]
                        h <- drip_h[i]
                        polygon(
                                x = c(x - 0.18, x - 0.12, x - 0.05, x, x + 0.05, x + 0.12, x + 0.18),
                                y = c(4.45, 4.10, 3.95 - h/3, 3.90 - h, 3.95 - h/3, 4.10, 4.45),
                                col = "ivory",
                                border = "grey80"
                        )
                }
                
                # Ciuffi di panna sul bordo superiore
                ciuffi_x <- seq(1.9, 8.1, length.out = 11)
                for (x in ciuffi_x) {
                        plotrix::draw.ellipse(
                                x = x, y = 4.95,
                                a = 0.22, b = 0.16,
                                col = "white", border = "grey85"
                        )
                        plotrix::draw.ellipse(
                                x = x, y = 5.08,
                                a = 0.12, b = 0.09,
                                col = "white", border = "grey85"
                        )
                }
                
                # Ciliegine/frutti decorativi
                cherry_x <- c(2.3, 3.8, 5.0, 6.2, 7.7)
                points(cherry_x, rep(4.95, length(cherry_x)), pch = 19, cex = 1.2, col = "firebrick3")
        }
        
        draw_candle <- function(x, y, flame_shift = 0, flame_scale = 1) {
                # candela più corta
                candle_w <- 0.10
                candle_h <- 1.15
                
                rect(
                        x - candle_w/2, y,
                        x + candle_w/2, y + candle_h,
                        col = "tomato",
                        border = "firebrick4",
                        lwd = 0.8
                )
                
                # righe decorative
                segments(x - candle_w/2, y + 0.18, x + candle_w/2, y + 0.28, col = "gold", lwd = 1)
                segments(x - candle_w/2, y + 0.48, x + candle_w/2, y + 0.58, col = "gold", lwd = 1)
                segments(x - candle_w/2, y + 0.78, x + candle_w/2, y + 0.88, col = "gold", lwd = 1)
                
                # stoppino
                segments(x, y + candle_h, x, y + candle_h + 0.08, col = "black", lwd = 1)
                
                # fiamma esterna
                fx <- x + flame_shift
                fy <- y + candle_h + 0.05
                
                polygon(
                        x = c(fx, fx - 0.07 * flame_scale, fx, fx + 0.07 * flame_scale, fx),
                        y = c(fy, fy + 0.18 * flame_scale, fy + 0.42 * flame_scale,
                              fy + 0.18 * flame_scale, fy),
                        col = "orange",
                        border = NA
                )
                
                # fiamma interna
                polygon(
                        x = c(fx, fx - 0.035 * flame_scale, fx, fx + 0.035 * flame_scale, fx),
                        y = c(fy + 0.05, fy + 0.16 * flame_scale, fy + 0.28 * flame_scale,
                              fy + 0.16 * flame_scale, fy + 0.05),
                        col = "yellow",
                        border = NA
                )
        }
        
        
        # ---------------------------------------------------------
        # Posizione candeline
        # ---------------------------------------------------------
        candle_grid <- expand.grid(
                x = seq(1.8, 8.2, by = 0.12),
                y = seq(4.45, 5.00, by = 0.05)
        )
        
        inside_top <- ((candle_grid$x - 5)^2 / 3.25^2) + ((candle_grid$y - 4.75)^2 / 0.48^2) <= 1
        candle_grid <- candle_grid[inside_top, ]
        
        if (age > nrow(candle_grid)) {
                age <- nrow(candle_grid)
                warning("Età ridotta al massimo numero di candeline disponibili.")
        }
        
        idx <- rdist::farthest_point_sampling(stats::dist(candle_grid), k = age)
        candles <- candle_grid[idx, ]
        candles <- candles[order(candles$y, decreasing = TRUE), ]
        
        # ---------------------------------------------------------
        # Audio: parser note
        # ---------------------------------------------------------
        notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
        
        melody <- "D D E D G F# D D E D A G D D D5 B G F# E C5 C5 B G A G"
        duration <- c(
                rep(c(0.75, 0.25, 1, 1, 1, 2), 2),
                0.75, 0.25, 1, 1, 1, 1, 1, 0.75, 0.25, 1, 1, 1, 2
        )
        
        pitch <- strsplit(melody, " ")[[1]]
        
        octave <- suppressWarnings(as.numeric(substring(pitch, nchar(pitch))))
        octave[is.na(octave)] <- 4
        
        note <- notes[substr(pitch, 1, 1)]
        note <- note + grepl("#", pitch) - grepl("b", pitch) + octave * 12 + 12 * (note < 3)
        freq <- 2 ^ ((note - 60) / 12) * 440
        
        # ---------------------------------------------------------
        # Sintesi "piano-like"
        # ---------------------------------------------------------
        tempo <- 112
        sample_rate <- 44100
        
        make_piano_tone <- function(freq, duration) {
                dur_sec <- duration / tempo * 60
                t <- seq(0, dur_sec, by = 1 / sample_rate)
                
                # armoniche semplici
                wave <- (
                        1.00 * sin(2 * pi * freq * t) +
                                0.50 * sin(2 * pi * 2 * freq * t) +
                                0.25 * sin(2 * pi * 3 * freq * t) +
                                0.12 * sin(2 * pi * 4 * freq * t)
                )
                
                # inviluppo tipo piano: attacco veloce + decadimento
                attack <- 0.015
                decay_rate <- 4.5
                
                env <- ifelse(
                        t < attack,
                        t / attack,
                        exp(-(t - attack) * decay_rate)
                )
                
                wave <- wave * env
                
                # riduce click finali
                n_fade <- min(200, floor(length(wave) / 2))
                if (n_fade > 1) {
                        wave[1:n_fade] <- wave[1:n_fade] * seq(0, 1, length.out = n_fade)
                        wave[(length(wave) - n_fade + 1):length(wave)] <-
                                wave[(length(wave) - n_fade + 1):length(wave)] * seq(1, 0, length.out = n_fade)
                }
                
                wave
        }
        
        bday_wave <- unlist(
                mapply(make_piano_tone, freq, duration, SIMPLIFY = FALSE),
                use.names = FALSE
        )
        
        # normalizza
        bday_wave <- bday_wave / max(abs(bday_wave)) * 0.6
        
        # ---------------------------------------------------------
        # Scrittura WAV robusta
        # ---------------------------------------------------------
        write_wave <- function(wave, file, sample_rate = 44100) {
                wave_obj <- tuneR::Wave(
                        left = as.integer(wave * 32767),
                        samp.rate = sample_rate,
                        bit = 16
                )
                tuneR::writeWave(wave_obj, filename = file)
        }
        
        play_wav <- function(file) {
                if (Sys.which("paplay") != "") {
                        system2("paplay", shQuote(file), wait = FALSE)
                } else if (Sys.which("aplay") != "") {
                        system2("aplay", shQuote(file), wait = FALSE)
                } else if (Sys.which("ffplay") != "") {
                        system2("ffplay", c("-nodisp", "-autoexit", shQuote(file)), wait = FALSE)
                } else {
                        message("Nessun player trovato. File salvato in: ", file)
                }
        }
        
        # ---------------------------------------------------------
        # Disegno iniziale
        # ---------------------------------------------------------
        draw_cake()
        for (i in seq_len(age)) {
                draw_candle(candles$x[i], candles$y[i], flame_shift = 0, flame_scale = 1)
        }
        
        # ---------------------------------------------------------
        # Audio
        # ---------------------------------------------------------
        if (isTRUE(play_sound)) {
                file_name <- paste0("happy_birthday_", gsub("[^A-Za-z0-9_]", "_", name), ".wav")
                write_wave(bday_wave, file_name, sample_rate = sample_rate)
                play_wav(file_name)
        }
        
        # ---------------------------------------------------------
        # Animazione candeline
        # ---------------------------------------------------------
        if (isTRUE(animate)) {
                for (frame in 1:40) {
                        draw_cake()
                        
                        for (i in seq_len(age)) {
                                flame_shift <- runif(1, -0.015, 0.015)
                                flame_scale <- runif(1, 0.9, 1.12)
                                draw_candle(candles$x[i], candles$y[i],
                                            flame_shift = flame_shift,
                                            flame_scale = flame_scale)
                        }
                        
                        Sys.sleep(0.08)
                }
        }
        
        invisible(candles)
}
