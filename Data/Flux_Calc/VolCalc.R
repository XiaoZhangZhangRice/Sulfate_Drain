library(writexl)
library(openxlsx)
library(readxl)
print("hello")
args = commandArgs(trailingOnly=TRUE)

#init file
#data <- read.table(file =args[1], header = FALSE,sep=',')

files <- c(list.files (path = file.path(getwd(), "ImportFiles")))
print( length(files))

batch <- 1
for (rowname in files) {
  

  print(file.path(getwd(),"ImportFiles",rowname))
  df <- read_excel(file.path(getwd(), "ImportFiles",rowname))
  headspace_mean <- rowMeans(df[,c("Headspace1_cm", "Headspace2_cm","Headspace3_cm","Headspace4_cm")], na.rm=TRUE)
  extension_cm <- df$Extension_ft*30.48
  chamber_Height<- extension_cm+7.62+headspace_mean 
  rad <- df["Chamber_Radius"]
  baseArea <- pi*rad**2
  ChamberVolCm3 <- baseArea*chamber_Height
  ChamberVolL <- ChamberVolCm3/1000
  BaseAreaM3 <- baseArea/10000
  VolAreaRatio <- ChamberVolL /BaseAreaM3 
  df2 <- cbind(df,headspace_mean,extension_cm,chamber_Height, ChamberVolCm3,ChamberVolL,baseArea,BaseAreaM3,VolAreaRatio)
  
  colnames(df2)<- c("Std_CH4_Peak","Std_N2O_Peak","Std_CH4_PPM","Std_N2O_PPM","CH4_Sample_Peak","N2O_Sample_Peak","Sample_Type","GC_Run","Date","Plot","Time","Chamber_Temp_C","Headspace1_cm","Headspace2_cm","Headspace3_cm","Headspace4_cm","Extension_ft","Chamber_Radius","HeadspaceMean_cm","Extension_cm","Chamber_Height","Total_Volume_cm3","Total_Volume_L","Base_Area","Base_Area_m3","VolumeRatio")
  
  ##write_xlsx(df2,file.path(getwd(), "VolumeExport.xlsx"))
  #generating linear relation
  CH4model <- lm(formula =Std_CH4_PPM ~ Std_CH4_Peak, data=df)
  N2Omodel <- lm(formula =Std_N2O_PPM ~ Std_N2O_Peak, data=df)
  CH4_c <- CH4model$coefficients[1]
  CH4_m <- CH4model$coefficients[2]
  CH4_r2 <- summary(CH4model)$r.squared
  N2O_r2 <- summary(N2Omodel)$r.squared
  #CH4_r2
  #N2O_r2
  # print alert is r2 value is below 0.95
  N2O_c <- N2Omodel$coefficients[1]
  N2O_m <- N2Omodel$coefficients[2]
  
  #getting output values from linear relation 
  CH4_Output <-CH4_m*df["CH4_Sample_Peak"]+CH4_c
  N2O_Output <-N2O_m*df["N2O_Sample_Peak"]+N2O_c
  
  
  
  #generating both curves 
  plot(x = df$Std_CH4_Peak,                          
       y = df$Std_CH4_PPM,
       type = "o",
       xlab = "CH4 Peak",
       ylab = "CH4 PPM",
       main = "CH4 Callibration Curve")
  abline(b = 1, a = 0) 
  plot(x = df$Std_N2O_Peak, # True values on x-axis
       y = df$Std_N2O_PPM,
       type = "o",
       xlab = "N2O Peak",
       ylab = "N2O PPM",
       main = "N2O Callibration Curve")
  abline(b = 1, a = 0)
  
  #volume and concentration calculations 
  
  N2O_Vol <-(((760*22.4)*(273+df$Chamber_Temp_C))/(760*273))
  N2O_Conc <- ((N2O_Output/N2O_Vol*(0.044014)))
  
  CH4_Vol <-(((760*22.4)*(273+df$Chamber_Temp_C))/(760*273))
  CH4_Conc <- ((CH4_Output/CH4_Vol*(0.016043)))
  
  
  
  df_out <- cbind(df2, CH4_Output,N2O_Output,N2O_Vol,N2O_Conc,CH4_Vol,CH4_Conc)
  
  colnames(df_out)[27] <- "CH4_Output"
  colnames(df_out)[28] <- "N2O_Output"
  colnames(df_out)[29] <- "N2O_Volume"
  colnames(df_out)[30] <- "N2O_Concentration"
  colnames(df_out)[31] <- "CH4_Volume"
  colnames(df_out)[32] <- "CH4_Concentration"

  

  #write_xlsx(df_out,file.path(getwd(), "MainExport.xlsx"))
  
  

  
  CH4Conc <- df_out["CH4_Concentration"]
  N20Conc <- df_out["N2O_Concentration"]
  Plot <- df_out["Plot"]
  Date <- df_out["Date"]
  VolumeRatio <- df_out["VolumeRatio"]
  
  
  fluxna <- data.frame(N20Conc, CH4Conc,Plot,Date,VolumeRatio)
  flux <- fluxna[!is.na(fluxna$Plot),]
  colnames(flux)[1] <- "N20Conc"
  colnames(flux)[2] <- "CH4Conc"
  
  
  counter = 0
  
  results = data.frame(matrix(ncol = 12, nrow = 0))
  colnames(results) = c("Plot","Date","N20Detection","CH4Detection","N20_Rsq","CH4_Rsq","N20_Linearity","CH4_Linearity","N20_Slope","CH4_Slope","N20_Flux","CH4_Flux")
  leftboundN = 0 
  leftboundC = 0 
  Nres = "PASS"
  Cres = "PASS"
  NLin= "MISS"
  CLin ="MISS"
  vectorN <- c()
  vectorC <- c()
  
  DetectionBoolN = FALSE
  DetectionBoolC = FALSE
  
  
  time <- c(0,21,42,63)
  
  for (row in 1:nrow(flux)) {
    
    vectorN <- c(vectorN, flux[row, "N20Conc"])
    vectorC <- c(vectorC, flux[row, "CH4Conc"])
    
   
    if ( counter ==0 ){
      leftboundN = flux[row, "N20Conc"]
      leftboundC = flux[row, "CH4Conc"]
      
    }
    
    # Detection Test for 0,21
    if ( counter == 1 ){
      N21 = abs( leftboundN - flux[row, "N20Conc"])
      C21 = abs( leftboundC - flux[row, "CH4Conc"])
      if(N21 <0.000183){
        DetectionBoolN= TRUE
      }
      if(C21 <0.000183){
        DetectionBoolC= TRUE
      }
      
    }
    
    # Detection Test for 0,42
    if ( counter == 2 ){
      N42 = abs( leftboundN - flux[row, "N20Conc"])
      C42 = abs( leftboundC - flux[row, "CH4Conc"])
      
      if(N42 <0.000183){
        DetectionBoolN= TRUE
      }
      if(C42 <0.000183){
        DetectionBoolC= TRUE
      }
      
    }
    
    
    # Detection Test for 0,63
    # Hit the end of the row
    if ( counter ==3 ){
      
      
      finalN = abs( leftboundN - flux[row, "N20Conc"])
      finalC = abs( leftboundC - flux[row, "CH4Conc"])
      if(finalN <0.000183){
        DetectionBoolN= TRUE
      }
      if(finalC <0.000183){
        DetectionBoolC= TRUE
      }
      
      
      
      
      counter =0
      
      #4 Point Linearity Test
    
      
      Nrsq<-  summary(lm(vectorN~time))$r.squared
      Crsq<-  summary(lm(vectorC~time))$r.squared
      
      Ngrad <-coef(lm(vectorN~time))[2]
      Cgrad <-coef(lm(vectorC~time))[2]
      
   
      if(Nrsq >0.845){
        NLin ="LIN"
      }else{
        
        #If 4pt Lin test fail
        #START 3 POINT LINEARITY TEST
        
        #0,1,2
        
        vectorN1 <- vectorN[-c(4)]
        time1 <- time[-c(4)]
        Nrsq1<-  summary(lm(vectorN1~time1))$r.squared
        Ngrad1 <- coef(lm(vectorN1~time1))[2]
        
        
        #1,2,3
        vectorN2 <- vectorN[-c(1)]
        time2 <- time[-c(1)]
        Nrsq2<-  summary(lm(vectorN2~time2))$r.squared
        Ngrad2 <- coef(lm(vectorN2~time2))[2]
        
        #0,1,3
        vectorN3 <- vectorN[-c(2)]
        time3 <- time[-c(2)]
        Nrsq3<-  summary(lm(vectorN3~time3))$r.squared
        Ngrad3 <- coef(lm(vectorN3~time3))[2]
        
        #0,2,3
        
        vectorN4 <- vectorN[-c(3)]
        time4 <- time[-c(3)]
        Nrsq4<-  summary(lm(vectorN4~time4))$r.squared
        Ngrad4 <- coef(lm(vectorN4~time4))[2]
       
        
        Ngrads <- c(Ngrad1,Ngrad2,Ngrad3,Ngrad4)
        Nrsqs <- c(Nrsq1,Nrsq2,Nrsq3,Nrsq4)
        MaxrsqN <- max(Nrsqs)
        
        #Get Highest RSQ Index
        MaxrsqNindex <- which.max(Nrsqs)
        #Take Highest RSQ, if fail again, then truly failed Linearity Test
        if(MaxrsqN > 0.8545){
          
          NLin= "LIN"
          Nrsq <- MaxrsqN
          Ngrad <- Ngrads[MaxrsqNindex]
        }else{
          NLin ="MISS"
          Ngrad=0
        }
        
      }
      
      
      if(Crsq >0.845){
        CLin ="LIN"
      }else{
        #START 3 POINT LINEARITY TEST
        
        #0,1,2
     
        vectorC1 <- vectorC[-c(4)]
        time1 <- time[-c(4)]
        Crsq1<-  summary(lm(vectorC1~time1))$r.squared
        Cgrad1 <- coef(lm(vectorC1~time1))[2]

        #1,2,3
        vectorC2 <- vectorC[-c(1)]
        time2 <- time[-c(1)]
        Crsq2<-  summary(lm(vectorC2~time2))$r.squared
        Cgrad2 <- coef(lm(vectorC2~time2))[2]
        
        #0,1,3
        vectorC3 <- vectorC[-c(2)]
        time3 <- time[-c(2)]
        Crsq3<-  summary(lm(vectorC3~time3))$r.squared
        Cgrad3 <- coef(lm(vectorC3~time3))[2]
        
        #0,2,3
        
        vectorC4 <- vectorC[-c(3)]
        time4 <- time[-c(3)]
        Crsq4<-  summary(lm(vectorC4~time4))$r.squared
        Cgrad4 <- coef(lm(vectorC4~time4))[2]
        
        
        
        Cgrads <- c(Cgrad1,Cgrad2,Cgrad3,Cgrad4)
        
        Crsqs <- c(Crsq1,Crsq2,Crsq3,Crsq4)
        MaxrsqC <- max(Crsqs)
        
        MaxrsqCindex <- which.max(Crsqs)
        
        
        if(MaxrsqC > 0.8545){
          CLin= "LIN"
          Crsq <- MaxrsqC
          print("EDITED GRADIENT")
          
          Cgrad <- Cgrads[MaxrsqCindex]
          print(Cgrad)
          print ("INDEX THAT SUCCEEDED IS ")
          print(MaxrsqCindex)

        }else{
          CLin ="MISS"
          Cgrad=0
        }
      }
      
      plotno <- flux[row, "Plot"]
      dateno <- format(flux[row, "Date"])
      volumeratio <-flux[row,"VolumeRatio"]
      
      
      
      if (DetectionBoolN == TRUE){
        Ngrad =0
      }
      
      if (DetectionBoolC == TRUE){
        Cgrad=0
      }
      
      
      
      Cflux <- Cgrad*volumeratio*240*60
      Nflux <- Ngrad*volumeratio*240*60
      
      
      
      results[nrow(results) + 1,] = c(plotno,dateno,Nres,Cres,Nrsq,Crsq,NLin,CLin,Ngrad,Cgrad,Nflux,Cflux)
      
      
      
      
      
      
      
      
      
      vectorN <- c()
      vectorC <- c()
      DetectionBoolN= FALSE
      DetectionBoolC= FALSE
      
    }else{
      counter = counter + 1
      
    }
    
    
    
    
    
    
    
  }
  

  
  rowname<- tools::file_path_sans_ext(rowname)
  print(rowname)
  name <- paste0(rowname, "_results", ".xlsx")
  write_xlsx(results,file.path(getwd(),"Results", name))
  
  
  
  batch= batch+1
  
  
}









