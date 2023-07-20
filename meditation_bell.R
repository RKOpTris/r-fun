library(beepr)

meditation_bell <- function(meditation_duration = 20, 
                     bell_interval = 5,
                     bell = NULL){
  if(!is.null(bell)){
    beep(bell)
    for(i in 1:(meditation_duration/bell_interval)){
      Sys.sleep(bell_interval * 60)
      beep(bell)
    }
    Sys.sleep(10)
    beep(bell)
  } else {
    stop("You need to set 'bell' to the full path of a .wav file. If you have no suitable .wav files, you could download and/or convert one from a file or from websites such as https://orangefreesounds.com/meditation-bell-sound/ or https://freesound.org/people/keston/sounds/60489/\n\nAlternatively, you can use a sound from the beepr package; see help('beepr')")
  }
  # e.g. med_bell(bell_interval = 4, bell = <full-.wav-file-path>)
}
