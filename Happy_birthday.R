Happy_birthday <- function(Bname="you", cake_colour="red"){

if(!require(dplyr)){install.packages("dplyr")}
if(!require(audio)){install.packages("audio")}
if(!require(plotrix)){install.packages("plotrix")}
require(dplyr)
require(audio)
require(plotrix)
  
candle = function(pos){
    x=pos[1]
    y=pos[2]
    rect(x,y,x+.2,y+2,col="red")
    #  polygon(c(x-.2,x+.4,x+.1,x-.2), c(y+2,y+2,y+2.4,y+2),col="orange")
    polygon(c(x+.05,x-.1,x+.1,x+.3,x+.15,x+0.05), c(y+2,y+2.3,y+2.6,y+2.3,y+2,y+2),col="orange")}
  
plot(c(0,10), c(0,10),type="n", bty="n",xaxt="n",yaxt="n", main=paste("Happy birthday to", Bname), xlab="",ylab="")
draw.ellipse(5,2,col=cake_colour,a=4.4,b=1.7,border=1)
draw.ellipse(5,2,col=cake_colour,a=4,b=1.4,border=1)
rect(1,2,9,5,col=cake_colour,border=cake_colour)
lines(c(1,1),c(2,5))
lines(c(9,9),c(2,5))
draw.ellipse(5,5,col=cake_colour,a=4,b=1.4)
candle(c(2.5,4.5))
candle(c(3,5))
candle(c(4,4.5))
candle(c(5,5))
candle(c(6,4.5))
candle(c(7,5.2))

set_driv<-audio::audio.drivers()

if(length(set_driv$name)==0) {stop("set the audio drivers using audio::set.audio.driver()")}

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
pitch <- "D D E D G F# D D E D A G D D D5 B G F# E C5 C5 B G A G"
duration <- c(rep(c(0.75, 0.25, 1, 1, 1, 2), 2),
              0.75, 0.25, 1, 1, 1, 1, 1, 0.75, 0.25, 1, 1, 1, 2)
bday <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                   duration = duration)

bday <-
  bday %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
  {suppressWarnings(as.numeric(.))} %>%
    ifelse(is.na(.), 4, .),
  note = notes[substr(pitch, 1, 1)],
  note = note + grepl("#", pitch) -
    grepl("b", pitch) + octave * 12 +
    12 * (note < 3),
  freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 120
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

bday_wave <-
  mapply(make_sine, bday$freq, bday$duration) %>%
  do.call("c", .)

audio::play(bday_wave)
}

