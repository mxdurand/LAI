### Convert the output of Hemisfer (.dat) into an R workable data frame and saves it
datToTxtLAI <- function(path, outName, ext = ".tif")
{
  O <- getwd()
  setwd(path)
  
  mydatfilenames <- list.files(pattern = ".dat") # look for .dat files (names, without the path)
  
  if(substring(path,nchar(path),nchar(path)) == "/"){
    outputfilenameText <- paste0(path, outName, ".txt") 
  } else {
    outputfilenameText <- paste0(path, "/", outName, ".txt") 
  }
  
  output<-NULL #variable for storing all the info (will become a data frame)
  for(i in 1:length(mydatfilenames))#loop over all dat files
  {
    myfilename<-mydatfilenames[i]
    
    print(myfilename)
    
    SKIP <- 0 #how many lines to skip before expecting a new image+info
    lineimagename <- scan(myfilename, what = "character", skip = SKIP + 0, nlines = 1, quiet = TRUE)
    
    #cond= either TRUE or FALSE (TRUE means that there is an image info to read)
    cond <- substring(lineimagename[2], nchar(lineimagename[2]) - 3, nchar(lineimagename[2])) == ext
    
    while(cond) #while there is a new image info to collect  
    {
      lineimagename<-scan(myfilename,what="character",skip=SKIP+0,nlines=1,quiet=TRUE)
      linedatetime<-scan(myfilename,what="character",skip=SKIP+1,nlines=1,quiet=TRUE)
      linesite<-scan(myfilename,what="character",skip=SKIP+3,nlines=1,quiet=TRUE)
      linethreshold<-scan(myfilename,what="character",skip=SKIP+7,nlines=1,quiet=TRUE)
      linegamma<-scan(myfilename,what="character",skip=SKIP+8,nlines=1,quiet=TRUE)
      linewhite<-scan(myfilename,what="character",skip=SKIP+23,nlines=1,quiet=TRUE)
      lineblack<-scan(myfilename,what="character",skip=SKIP+24,nlines=1,quiet=TRUE)
      linetransmission<-scan(myfilename,what="character",skip=SKIP+25,nlines=1,quiet=TRUE)
      linegaps<-scan(myfilename,what="character",skip=SKIP+26,nlines=1,quiet=TRUE)
      lineopenness<-scan(myfilename,what="character",skip=SKIP+27,nlines=1,quiet=TRUE)
      linegaps2<-scan(myfilename,what="character",skip=SKIP+28,nlines=1,quiet=TRUE)
      
      lineT1<-scan(myfilename,what="character",skip=SKIP+34,nlines=1,quiet=TRUE)
      lineT2<-scan(myfilename,what="character",skip=SKIP+35,nlines=1,quiet=TRUE)
      lineT3<-scan(myfilename,what="character",skip=SKIP+36,nlines=1,quiet=TRUE)
      lineT4<-scan(myfilename,what="character",skip=SKIP+37,nlines=1,quiet=TRUE)
      lineT5<-scan(myfilename,what="character",skip=SKIP+38,nlines=1,quiet=TRUE)
      lineT6<-scan(myfilename,what="character",skip=SKIP+39,nlines=1,quiet=TRUE)
      lineT9<-scan(myfilename,what="character",skip=SKIP+43,nlines=1,quiet=TRUE)
      lineT10<-scan(myfilename,what="character",skip=SKIP+44,nlines=1,quiet=TRUE)
      
      print(lineimagename[2])
      
      imagename<-lineimagename[2]
      date<-linedatetime[4]
      time<-linedatetime[5]
      site<-linesite[2]
      threshold<-linethreshold[2]
      gamma<-linegamma[2]
      white<-linewhite[2]
      black<-lineblack[2]
      transmission<-linetransmission[2]
      transmission<-substring(transmission,1,nchar(transmission)-1)#get rid of %
      gap<-linegaps[2]
      gap<-substring(gap,1,nchar(gap)-1)#get rid of %
      openness<-lineopenness[2]
      openness<-substring(openness,1,nchar(openness)-1)#get rid of %
      gap2<-linegaps2[2]
      gap2<-substring(gap2,1,nchar(gap2)-1)#get rid of %
      
      NoMillerLAI<-lineT1[3]
      NoLAI2000LAI<-lineT2[3]
      NoLAI2000angle<-lineT2[4]
      NoLangLAI<-lineT3[3]
      NoGLAI<-lineT4[5]
      NoNCLAI<-lineT5[5]
      NoNCangle<-lineT5[6]
      NoNCFmv<-lineT9[5]
      NoTLAI<-lineT6[5]
      NoTangle<-lineT6[6]
      NoTFmv<-lineT10[5]
      
      NoNCFmv<-substring(NoNCFmv,1,nchar(NoNCFmv)-1)#get rid of %
      NoTFmv<-substring(NoTFmv,1,nchar(NoTFmv)-1)#get rid of %
      
      
      SMillerLAI<-lineT1[5]
      SLAI2000LAI<-lineT2[5]
      SLAI2000angle<-lineT2[6]
      SLangLAI<-lineT3[5]
      SGLAI<-lineT4[7]
      SNCLAI<-lineT5[7]
      SNCangle<-lineT5[8]
      SNCFmv<-lineT9[7]
      STLAI<-lineT6[7]
      STangle<-lineT6[8]
      STFmv<-lineT10[7]
      
      SNCFmv<-substring(SNCFmv,1,nchar(SNCFmv)-1)#get rid of %
      STFmv<-substring(STFmv,1,nchar(STFmv)-1)#get rid of %
      
      CMillerLAI<-lineT1[7]
      CLAI2000LAI<-lineT2[7]
      CLAI2000angle<-lineT2[8]
      CLangLAI<-lineT3[7]
      CGLAI<-lineT4[9]
      CNCLAI<-lineT5[9]
      CNCangle<-lineT5[10]
      CNCFmv<-lineT9[9]
      CNCFrv<-lineT9[10]
      CTLAI<-lineT6[9]
      CTangle<-lineT6[10]
      CTFmv<-lineT10[9]
      CTFrv<-lineT10[10]
      
      CNCFmv<-substring(CNCFmv,1,nchar(CNCFmv)-1)#get rid of %
      CTFmv<-substring(CTFmv,1,nchar(CTFmv)-1)#get rid of %
      
      CNCFrv<-substring(CNCFrv,1,nchar(CNCFrv)-1)#get rid of %
      CTFrv<-substring(CTFrv,1,nchar(CTFrv)-1)#get rid of %
      
      SCMillerLAI<-lineT1[9]
      SCLAI2000LAI<-lineT2[9]
      SCLAI2000angle<-lineT2[10]
      SCLangLAI<-lineT3[9]
      SCGLAI<-lineT4[11]
      SCNCLAI<-lineT5[11]
      SCNCangle<-lineT5[12]
      SCNCFmv<-lineT9[11]
      SCNCFrv<-lineT9[12]
      SCTLAI<-lineT6[11]
      SCTangle<-lineT6[12]
      SCTFmv<-lineT10[11]
      SCTFrv<-lineT10[12]
      
      SCNCFmv<-substring(SCNCFmv,1,nchar(SCNCFmv)-1)#get rid of %
      SCTFmv<-substring(SCTFmv,1,nchar(SCTFmv)-1)#get rid of %
      
      SCNCFrv<-substring(SCNCFrv,1,nchar(SCNCFrv)-1)#get rid of %
      SCTFrv<-substring(SCTFrv,1,nchar(SCTFrv)-1)#get rid of %
      
      laimethod<-rep(c("Miller","LAI2000","Lang","NC","T","G"),4) # 4 types of correction
      correction<-rep(c("no","S","C","SC"),each=6) # 6 methods
      lai<-c(NoMillerLAI,NoLAI2000LAI,NoLangLAI,NoNCLAI,NoTLAI,NoGLAI,
             SMillerLAI,SLAI2000LAI,SLangLAI,SNCLAI,STLAI,SGLAI,
             CMillerLAI,CLAI2000LAI,CLangLAI,CNCLAI,CTLAI,CGLAI,
             SCMillerLAI,SCLAI2000LAI,SCLangLAI,SCNCLAI,SCTLAI,SCGLAI)
      angle<-c(NA,NoLAI2000angle,NA,NoNCangle,NoTangle,NA,
               NA, SLAI2000angle,NA, SNCangle, STangle,NA, 
               NA, CLAI2000angle,NA, CNCangle, CTangle,NA, 
               NA,SCLAI2000angle,NA,SCNCangle,SCTangle,NA)
      fmv<-c(NA,NA,NA,NoNCFmv,NoTFmv,NA,
             NA,NA,NA, SNCFmv, STFmv,NA,
             NA,NA,NA, CNCFmv, CTFmv,NA,
             NA,NA,NA,SCNCFmv,SCTFmv,NA)
      frv<-c(NA,NA,NA,NA,NA,NA,
             NA,NA,NA,NA,NA,NA,
             NA,NA,NA, CNCFrv, CTFrv,NA,
             NA,NA,NA,SCNCFrv,SCTFrv,NA)
      filename <- rep(myfilename,24)
      
      #convert to numeric
      threshold<-as.numeric(gsub(",", ".", threshold))
      white<-as.numeric(gsub(",", ".", white))
      black<-as.numeric(gsub(",", ".", black))
      transmission<-as.numeric(gsub(",", ".", transmission))
      gap<-as.numeric(gsub(",", ".", gap))
      openness<-as.numeric(gsub(",", ".", openness))
      gap2<-as.numeric(gsub(",", ".", gap2))
      lai<-as.numeric(gsub(",", ".", lai))
      angle<-as.numeric(gsub(",", ".", angle))
      fmv<-as.numeric(gsub(",", ".", fmv))
      frv<-as.numeric(gsub(",", ".", frv))
      
      #convert 'date' to a date format
      date<-as.Date(date,"%d.%m.%Y")
      
      #collect all the current info in a data frame
      outputtemp<-data.frame(Picture=imagename,Site=site,Date=date,Time=time,
                             Threshold=threshold,LAImethod=laimethod,Correction=correction,
                             White=white,Black=black,Trans=transmission,Gap=gap,Openness=openness,gap2=gap2,
                             LAI=lai,Angle=angle,Fmv=fmv,Frv=frv,file=filename) 
      
      output<-rbind(output,outputtemp)  #append the current image info (data frame) to the total
      
      #IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT 
      
      SKIP<-SKIP+47 #move 40 lines down to look for a new .jpg file+info
      lineimagename<-scan(myfilename,what="character",skip=SKIP+0,nlines=1,quiet=TRUE)
      
      #cond= either TRUE or FALSE (TRUE means that there is an image info to read)
      cond<-substring(lineimagename[2],nchar(lineimagename[2])-3,nchar(lineimagename[2]))==ext
      
    }#end of 'while' 
  }#end of i loop for files
  
  write.table(x=output,file=outputfilenameText,row.names=FALSE,col.names=TRUE) #too big to save to Excel
  
  setwd(O)
}

### Extract factors from picture name
extractFactors <- function(data = dfB)
{
  STR <- unlist(strsplit(data$Picture, ".", fixed = T))[rep(c(T,F),nrow(data))]
  STR2 <- strsplit(STR, "_", fixed  = T) 
  M <- t(sapply(STR2, function(x) x[1:6]))
  
  data$MP <- M[,2]
  data$Height <- as.numeric(substring(M[,3],1,nchar(M[,3])-1))
  data$Proc <- M[,6]
  data[is.na(data$Proc),"Proc"] <- "Sharp"
  return(data)
}

