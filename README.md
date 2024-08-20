# Functions to calculate Leaf Area Index from various softwares

## calcLAIfromLP80.R   
Calculate LAI and canopy transmittance from LAI meter data. This was written for the LP-80 device but could be adapted for other devices provided columns and variable names are ajusted.  

## ellipDisParam.R
Provide either mean angle or parameter Xi and get the other back. Can be use to get Xi for `calcLAIfromLP80` if you know the mean leaf angle distribution. follows Campbell et al. (1990).   

## Hemisfer_process
Legacy code for processing hemispherical pictures with the [hemisfer software](https://www.schleppi.ch/patrick/hemisfer/) Hemisfer provides a .dat file with all pictures process in separate text files tables. This function collects all the information in a single data frame. 
