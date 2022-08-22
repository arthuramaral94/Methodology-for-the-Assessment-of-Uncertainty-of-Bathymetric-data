# Methodology-for-the-Assessment-of-Uncertainty-of-Bathymetric-data


  # Setting the data
  setwd("It's necessary to set the diretory here")
  
  getwd() 
  
  #  Packages
  pkg <- c("geoR","moments","scatterplot3d","tcltk2",
           "sp", "rgdal", "ggplot2" , "cluster" ,
           "bootstrap", "plyr", "robustbase", "MBESS", "rgeos", "gstat")
  
  sapply(pkg, require, character.only=TRUE)
  


  # Reading the data
  dados <- read.table("teste.txt", header=T, dec=",")
  names(dados)
  dados
  length(dados$dz)
  
  # Reading the data for geoestatistical analysis
  dados1 <- read.geodata("teste.txt", header=T, dec=",",coords=1:2, data.col=4)
  names(dados1)
  dados1
  length(dados1$data)
  dup.coords(dados1)
  
  # S-44 Analysis 
 
  
  # Tolerance
  esp <- sqrt((0.25^2)+((0.0075*dados$Z)^2)) #Ordem Especial
  A <- sqrt((0.50^2)+((0.013*dados$Z)^2)) #Order 1A or 1B
  B <- sqrt((1.0^2)+((0.023*dados$Z)^2)) #Order 2
  
  # Special Order
  aux <- round(((sum (abs(dados$dz)<=esp)/length(dados$dz))*100),2)
  #Order 1A or 1B
  aux1 <- round(((sum (abs(dados$dz)<=A)/length(dados$dz))*100),2)
  #Order 2
  aux2 <- round(((sum (abs(dados$dz)<=B)/length(dados$dz))*100),2)
  
  # Tolerance with the mean depth
  esp1<- sqrt((0.25^2)+((0.0075*mean(dados$Z))^2)) 
  A1 <- sqrt((0.50^2)+((0.013*mean(dados$Z))^2)) 
  B1 <- sqrt((1.0^2)+((0.023*mean(dados$Z))^2)) 
  
  # Exporting
  sink("Results_normam.txt", type="output", append=T)
  
  cat("#Results S-44 and NORMAM-25","\n",
      "------------------------------------------------------","\n",
      "Ordem Especial: "         ,aux ,"%"   ,"\n",
      "Ordem 1A/1B:"             ,aux1,"%"  ,"\n",
      "Ordem 2:"                 ,aux2,"%"  ,"\n",
      "------------------------------------------------------","\n\n",
      "Toler?ncia NORMAM-25 / S-44","\n",
      "Intervalo de Confian?a de 95%","\n",
      "Profundidade M?dia (m):", round(mean(dados$Z),3),"\n",
      "------------------------------------------------------","\n",
      "Ordem Especial (m):" , "[",round(-1*esp1,3),";",round(esp1,3),"]" ,"\n",
      "Ordem 1A/1B (m):"    , "[",round(-1*A1,3),";",round(A1,3),"]" ,"\n",
      "Ordem 2 (m):"        , "[",round(-1*B1,3),";",round(B1,3),"]" ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results_normam.txt")
  
  # Plot
  Ordem <- NULL
  s44 <- dados
  for(i in 1:length(dados$dz)) {
    
    if (s44$dz [i] >= -1*esp1 & s44$dz [i] <= esp1){
      
      Ordem[i] <- "Special"
      
    }
    
    else if (s44$dz[i] >= -1*A1 & s44$dz[i] < -1*esp1 | s44$dz[i] <= A1 &  s44$dz[i] > esp1){
      
      Ordem[i] <- "1A/1B"
    }
    
    else if (s44$dz[i] >= -1*B1 & s44$dz[i] < -1*A1 | s44$dz[i] <= B1 &  s44$dz[i] > A1){
      
      Ordem[i] <- "2"
    } 
    
    else { Ordem [i] <- "No Classification"}
    
  }
  
  s44 <- cbind(dados,Ordem)
  
  # Plot ggplot2
  windows(8,6,title="Classificação: NORMAM-25 / S44")
  par(mfrow=c(1,1), family="serif")
  ggplot(s44, aes(x = X, y = Y, colour = Ordem)) + geom_point(size=2)+
    labs(title= "Classification: NORMAM-25 / S-44", x= "E (m)", y= "N (m)") +
    theme_bw()+theme(plot.title = element_text(hjust = 0.5, size = 16))+
    theme(legend.title = element_text(size=14, face="bold"))+
    scale_color_manual(values = c( "2"="red","Sem Classificação" = "blue",
                                   "Especial" = "forestgreen", "1A/1B" = "black"))
  
  # Plot
  windows(8,6,title="Classification: NORMAM-25 / S44")
  par(mfrow=c(1,1), cex=1.2, family="serif")
  plot(s44$X, s44$Y, col= s44$Ordem,
       xlab=" E (m)", ylab="N (m)", main="Classificação: NORMAM-25 / S-44",pch=16)
  legenda <- aggregate(s44, by = list(unique.values = s44$Ordem), FUN=length)
  legend("bottomleft", inset=.05, legend= legenda$unique.values, 
         col= legenda$unique.values, pch=16,bty="o", title="Ordem")
  
  
  # Method                               
  ## Explonatory Analysis
  
  (res=summary(dados1))
  

  attach(res)
  
  ## Measures
  (med = round(mean(dados$dz),3))
  (min = round(min(dados$dz),3))
  (max = round(max(dados$dz),3))
  (des = round(sd(dados$dz),4))
  (var = round(var(dados$dz),4))
  (CV = round(100*sd(dados$dz)/mean(dados$dz),2))
  (curt = round(kurtosis(dados$dz),2))
  (assim = round(skewness(dados$dz),2))
  (descritiva = data.frame(med,min,max,var,des,CV,curt,assim))
  (n=length(dados$dz))
  (dist.min= round((distances.summary[1]),3))
  (dist.max= round((distances.summary[2]),3))
  
  ## Exporting
  sink("Results.txt", type="output", append=T)
  cat(" Methods Proposition ","\n",
      "------------------------------------------------------","\n",
      " Explonatory Analysis:","\n",
      "------------------------------------------------------","\n",
      n, "observa??es"    ,"\n",
      "Mean:"      ,med,'metros'  ,"\n",
      "Min:"  ,min,"metros"  ,"\n",
      "Max:"     ,max,"metros"  ,"\n",
      "Var:"  ,var,"metros?"  ,"\n",
      "Standard Deviation:"     ,des,"metros"  ,"\n",
      "CV:"         ,CV,"%"  ,"\n",
      "Curtosis: "         ,curt    ,"\n",
      "Assimetry:"      ,assim  ,"\n",
      "dist.min.:"  ,dist.min,"metros"  ,"\n",
      "dist.max.:" ,dist.max,"metros" ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results.txt")
  
 
  ## Explonatory Analysis - Graphs
 
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(2,2), family="serif")
  
  hist(dados$dz, xlab="Discrepancies (m)", ylab= "Frequency", main=" Histogram")
  rug(jitter(dados$dz))
  plot(density(dados$dz), xlab="Discrepancies (m)", ylab= "Frequency", main=" Density")
  boxplot(dados$dz, xlab="dZ", ylab= "Discrepancies (m)", main="Boxplot (Tukey)")
  qqnorm(dados$dz, xlab="Quantis Teóricos", ylab= "Quantis Amostrados", main=" Normal Q-Q Plot")
  qqline(dados$dz,lty=2, col='red')
  par(mfrow=c(1,1), family="serif")
  

  ## Boxplot Ajusted
  windows(8,6,title="Explonatory Analysis - Graphs")
  par(mfrow=c(1,2), family="serif")
  adjbox(dados$dz, xlab="dZ", ylab= "Discrepancies (m)", main="Boxplot Ajusted")
  boxplot(dados$dz, xlab="dZ", ylab= "Discrepancies (m)", main="Boxplot (Tukey)")
  par(mfrow=c(1,1) ,family="serif")
  

  ## Spatial Exploratory Analysis 
 
  
  windows(8,6,canvas="snow2",title="Depth")
  ggplot(dados, aes(x = X, y = Y, colour = dz)) + geom_point()+
    xlab("E (m)") + ylab("N (m)") + ggtitle("Study Area") +
    theme_bw()+theme(plot.title = element_text(hjust = 0.5))
  
  windows(8,6,title="MAIB")
  scatterplot3d(dados$X,dados$Y,dados$dz,xlab=" E (m)", ylab="N (m)", zlab="Depth (m)", main="Study Area")
  
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(2,2), family="serif")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="equal")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="data.proportional")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="quartiles")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="deciles")
  par(mfrow=c(1,1), family="serif")
  
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(1,1), family="serif")
  points(dados1,xlab="E (m)",ylab="N (m)",pt.divide="quartiles", main="Quartis Graphs")
  
  ## Trend analysis
   
  windows(8,6,title="Graphs for trend")
  par(mfrow=c(1,1), family="serif")

  ## Outliers detection for normal data
  ## Boxplot de Tukey
  out.box <- boxplot.stats(dados$dz)
  
  ## Isolates outliers detected by Tukey's boxplot
  result <- dados[-which(dados$dz<out.box$stats[1] | dados$dz>out.box$stats[5]),]
  
  ## Z-Score Modificated
  ZSM <- abs((0.6745*(dados$dz-median(dados$dz)))/(mad(dados$dz,constant = 1)))  
  
  ## Isolates outliers detected by Z-Score Modificated
  result1 <- dados[-which(ZSM>3),]
  
  ## Outliers detection for assimetric data
  

  ## Outliers isolated detect by Boxplot ajusted
  result.box.ajust <- dados[-which(dados$dz<out.box1$stats[1] | dados$dz>out.box1$stats[5]),]
  
  ## Exporting information
  sink("Results.txt", type="output", append=T)
  
  cat(" Outlier detection","\n",
      "------------------------------------------------------","\n",
      "Boxplot (Tukey): "         ,out.box$out,"\n\n",
      "Boxplot Ajusted: "         ,out.box1$out,"\n\n",
      "Z-Score Modificated: "         ,dados$dz[which(ZSM>3)]   ,"\n\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results.txt")
  
  ## Data without Outilier
  
  write.table(results, "dados_without_out_boxplot.txt", dec=",")
  
  write.table(result1, "dados_without__out_ZSM.txt", dec=",")
  
  write.table(result.box.ajust, "dados_without_out_boxplot_ajustado.txt", dec=",")

  ## Data without outliers 
 
  
  ## Reading
  dados <- read.table("dados_without_out_boxplot.txt", header=T, dec=",")
  names(dados)
  dados
  length(dados$dz)
  
  ## Reading the data for geoestatistical analysis
  dados1 <- read.geodata("dados_without_out_boxplot.txt", header=T, dec=",",coords=1:2, data.col=4)
  names(dados1)
  dados1
  length(dados1$data)
  dup.coords(dados1)
  
  ## Exploratory Analysis
 
  (res=summary(dados1))
  
  attach(res)
  
  ## Mean measures
  (med = round(mean(dados$dz),3))
  (min = round(min(dados$dz),3))
  (max = round(max(dados$dz),3))
  (des = round(sd(dados$dz),4))
  (var = round(var(dados$dz),4))
  (CV = round(100*sd(dados$dz)/mean(dados$dz),2))
  (curt = round(kurtosis(dados$dz),2))
  (assim = round(skewness(dados$dz),2))
  (descritiva = data.frame(med,min,max,var,des,CV,curt,assim))
  (n=length(dados$dz))
  (dist.min= round((distances.summary[1]),3))
  (dist.max= round((distances.summary[2]),3))
  
  ## Exporting information
  sink("Results.txt", type="output", append=T)
  cat(" Data without Outliers ","\n",
      "------------------------------------------------------","\n",
      " Exploratory Analysis:","\n",
      "------------------------------------------------------","\n",
      n, "observa??es"    ,"\n",
      "Mean:"      ,med,'metros'  ,"\n",
      "Min:"  ,min,"metros"  ,"\n",
      "Max:"     ,max,"metros"  ,"\n",
      "Var:"  ,var,"metros?"  ,"\n",
      "Standard Deviation:"     ,des,"metros"  ,"\n",
      "CV:"         ,CV,"%"  ,"\n",
      "Curtosis: "         ,curt    ,"\n",
      "Assimetry:"      ,assim  ,"\n",
      "dist.min:"  ,dist.min,"metros"  ,"\n",
      "dist.max:" ,dist.max,"metros" ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results.txt")
  
  ## Exploratory Analysis - Graphs
  
  windows(8,4,title="Exploratory Analysis - Graphs")
  par(mfrow=c(1,3), family="serif")
  hist(dados$dz, xlab="Depth (m)", ylab= "Frequency", main=" Histogram")
  rug(jitter(dados$dz))
  plot(density(dados$dz), xlab="Batimetria Espectral (m)", ylab= "Frequency", main=" Density")
  qqnorm(dados$dz, xlab="Quantis Teóricos", ylab= "Quantis Amostrados", main=" Normal Q-Q Plot")
  qqline(dados$dz,lty=2, col='red')
  par(mfrow=c(1,1), family="serif")
  
  
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(1,2), family="serif")
  adjbox(dados$dz, xlab="dZ", ylab= "Depth (m)", main="Boxplot Ajustado")
  boxplot(dados$dz, xlab="dZ", ylab= "Depth (m)", main="Boxplot (Tukey)")
  par(mfrow=c(1,1), family="serif")
  
  
  ## Spatial Exploratory Analysis
 
  
  windows(8,6,canvas="snow2",title="Depth")
  ggplot(dados, aes(x = X, y = Y, colour = dz)) + geom_point()+
    xlab("E (m)") + ylab("N (m)") + ggtitle("Study Area") +
    theme_bw()+theme(plot.title = element_text(hjust = 0.5))
  
  windows(8,6,title="Discrepancies")
  scatterplot3d(dados$X,dados$Y,dados$dz,xlab=" E (m)", ylab="N (m)", zlab=" Depth (m)", main="Study Area")
  
  
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(2,2), family="serif")
    #Visualiza??o
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="equal")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="data.proportional")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="quartiles")
  points(dados1,xlab="E (m)",ylab="N (m)", pt.divide="deciles")
  par(mfrow=c(1,1), family="serif")
  
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(1,1), family="serif")
  points(dados1,xlab="E (m)",ylab="N (m)", 
         pt.divide="quartiles", main="Quartis Graphs")
  
  ## Trend Analysis
  
  
  windows(8,6,title="Exploratory Analysis - Graphs")
  par(mfrow=c(1,1), family="serif")
  plot(dados1,low=T) 
  
  
 
  ## Independence Analysis (semivariogram)
   
  ## Empiric Semivariogram 
  
  ### 4 semivariograms:
  #1 - with range of 100% of the max distance
  #2 - with range of 75% of the max distance
  #3 - with range of 50% of the max distance
  #4 - with range of 25% of the max distance
  
  windows(8,6,title="Semivariograma Omnidirecional")
  escala.y=2*var
  
  par(mfrow=c(2,2), family="serif")
  vario.emp.1 <- variog(dados1,max.dist=(dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distances (m)",ylab="Semivariances (m)", main=("100% of the max distance"))
  abline(var(dados1$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Amostral Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  vario.emp.1 <- variog(dados1,max.dist=(0.75*dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distâncias (m)",ylab="Semivariances (m)", main=("75% of the max distance"))
  abline(var(dados1$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Amostral Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  vario.emp.1 <- variog(dados1,max.dist=(0.50*dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distâncias (m)",ylab="Semivariâncias (m)", main=("50% of the max distance"))
  abline(var(dados1$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Amostral Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  vario.emp.1 <- variog(dados1,max.dist=(0.25*dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distâncias (m)",ylab="Semivariâncias (m)", main=("25% of the max distance"))
  abline(var(dados1$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Amostral Variance", col="gray60",lty=2, lwd=2,bty='n')
  par(mfrow=c(1,1), family="serif")
  
  dist.max
  0.75*dist.max
  0.50*dist.max
  0.25*dist.max
  
  ### Omnidirecional semivariogram of the discrepancies/ Monte Carlo Envelope
 
  
  M <- (0.25*dist.max) 
  
  windows(8,6,title="Omnidirecional semivariogram")
  par(mfrow=c(1,1), family="serif")
  vario.emp.1 <- variog(dados1,max.dist= M, direction="omnidirectional")
  plot(vario.emp.1, ylim=c(0,escala.y),xlab="Distâncias (m)",ylab="Semivariancies (m)", main=("Semivariogram")) 
  abline(var(dados1$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Amostral Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  #Envelope de variograma (Monte Carlo Simulation)
  vario.env <- variog.mc.env(dados1, obj.v=vario.emp.1)
  plot(vario.emp.1, env=vario.env,ylim=c(0,escala.y),xlab="Distâncias (m)",ylab="Semivariancies (m)", main=("Semivariogram")) 
  abline(var(dados1$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Variância Amostral", col="gray60",lty=2, lwd=2,bty='n')
  
  #Exportando informa??es:
  sink("Results.txt", type="output", append=T)
  cat(" Results of the Semivariogram:","\n",
      "------------------------------------------------------","\n",
      vario.emp.1$n.data, "observações"    ,"\n",
      "Distances:"      , vario.emp.1$u  ,"\n",
      "Semivariances:"  ,vario.emp.1$v  ,"\n",
      "Number of pairs:"     ,vario.emp.1$n  ,"\n",
      "Square Deviation:"  ,vario.emp.1$sd,"\n",
      "Max Distance:"     ,vario.emp.1$max.dist,"\n",
      "Direction:"         ,vario.emp.1$direction  ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results.txt")
  
  
  ### Functions
  
  #### Uncertainty Function
  theta <- function(x){(sqrt((sd(x)^2)+(mean(x)^2)))}
  
  ####  RMSE
  theta1 <- function(x){(sqrt((sum(x^2))/length(x)))}
  
  #### TCL
  theta2 <- function(x,y){(sqrt(((sd(x)^2)*y)+(mean(x)^2)))}
  
  #### Robust Uncertainty
  theta3 <- function(x){(sqrt((mad(x)^2)+(median(x)^2)))}
  
  
  ## Independent Sample
  
  
  ### Normality
  
  par(mfrow=c(1,1), family="serif")
  qqnorm(dados$dz, xlab="Quantis Teóricos", ylab= "Quantis Amostrados", main=" Normal Q-Q Plot")
  qqline(dados$dz,lty=2, col='red')

  
  shapiro.test(dados$dz)
  shap <- shapiro.test(dados$dz)
  ks.test(dados$dz,"pnorm", mean(dados$dz), sd(dados$dz))
  ks <- ks.test(dados$dz,"pnorm", mean(dados$dz), sd(dados$dz))

  sink("Results.txt", type="output", append=T)
  
  cat(" Normality of the sample ","\n",
      "Tamanho da Amostra: ",n,"\n",
      "------------------------------------------------------","\n",
      "\n Kolmogorov-Smirnov "        ,"\n",
      "p-value: "         ,ks$p.value   ,"\n",
      "Normal: "         ,(ks$p.value>0.05),"\n",
      "\np-valeu > 0.05, The sample is normal","\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results.txt")
  
  ### Normal Sample without Outliers
  
  ### Verification
  tol <- 0.1 
  abs((1.96*sd(dados$dz)-quantile(dados$dz,0.95))) < tol
  abs((mean(dados$dz)-median(dados$dz))) < tol
  abs((sd(dados$dz)- quantile(dados$dz,0.683))) < tol
  
  ### Statistics 
  ivt = theta(dados$dz)
  rms = theta1(dados$dz)

  ### Number of samples to estimate the CL with bootstrap
  
  sample=5000
  
  ### Boott - Bootstrap-t Confidence Limits
  results.boot <- boott(dados$dz,theta, nboott=amostra,VS=FALSE,perc=c(0.025,0.975))
  results.boot1 <- boott(dados$dz,theta1, nboott=amostra,VS=FALSE,perc=c(0.025,0.975))
 
  ### Nonparametric BCa Confidence Limits
  results.bca <- bcanon(dados$dz, amostra, theta,alpha=c(0.025, 0.975))
  results.bca1 <- bcanon(dados$dz, amostra, theta1,alpha=c(0.025, 0.975))
 
  ### CI for RMSE
  results.rms <- ci.rmsea(rms,length(dados$dz),length(dados$dz),conf.level = 0.95, alpha.lower = NULL, alpha.upper = NULL)
  
  ### Exporting the information
  sink("Results.txt", type="output", append=T)
  
  cat(" Vertical Uncertainty \n Independence Sample, Normal e and without Outliers","\n",
      "\n CI  95%","\n",
      "------------------------------------------------------","\n",
      "Incerteza (m): "         ,round(ivt,3)   ,"\n",
      "IC bootstrap-t (m): "         ,"[",round(results.boot$confpoints[1,1],3),
      ";",round(results.boot$confpoints[1,2],3),"]","\n",
      "IC BCa (95%): "         ,"[",round(results.bca$confpoints[1,2],3),
      ";",round(results.bca$confpoints[2,2],3),"]","\n",
      
      "\n RMSE (m): "         ,round(rms,3)   ,"\n",
      "IC (qui-quadrado)(m): "         ,"[",round(results.rms$Lower.Conf.Limit,3),";",round(results.rms$Upper.Conf.Limit,3),"]","\n",
      "IC bootstrap-t (m): "         ,"[",round(results.boot1$confpoints[1,1],3),
      ";",round(results.boot1$confpoints[1,2],3),"]","\n",
      "IC BCa (m): "         ,"[",round(results.bca1$confpoints[1,2],3),
      ";",round(results.bca1$confpoints[2,2],3),"]","\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Results.txt")
  
  ### Non-Normal Sample
  ###  TCL

  ### Partitioning - PAM function {cluster}

  ### Average number of points per cluster 
  
  Tamanho_amostral = 4
  
  ### Number of clusters
  
  k= round((length(dados$dz))/Tamanho_amostral, 0) 
 
  grupos <- pam(dados[,c(1,2)],k=k,metric = "euclidean", stand = TRUE)

  ### Group the samples by index and calculate the average of each
  
  TCL <- aggregate(dados$dz~grupos$clustering, FUN = mean)
  
  ### Plot the clusters
  
  windows(10,5)
  par(mfrow=c(1,2), family="serif")
  plot(dados$X,dados$Y,xlab=" E (m)", ylab="N (m)", col=grupos$clustering, 
       main="Grouping")
  points(grupos$medoids, pch=16, col=25)
  legend("bottom", inset=.05, legend= "centroid", 
         col= 25, pch=16,bty="o")
  plot(dados$dz~grupos$clustering, xlim=c(0,k+1),
       ylim=c(min(dados$dz-0.1),max(dados$dz+0.1)), xlab="Grupos", 
       ylab="Discrep?ncias (m)", xaxt="n", main="Distribution of Groupings")
  axis(1,at=seq(1,k, by=1)) # add the x axis
  points(TCL, col=2,pch=16)
  legend("bottom",inset=.05, legend= "Cluster Average", 
         col= 2, pch=16,bty="o")
  par(mfrow=c(1,1), family="serif")
  
  ### Analysis of the normality of the new sample
  
  shap1 <- shapiro.test(TCL$`dados$dz`)
  ks1 <- ks.test(TCL$`dados$dz`,"pnorm", mean(TCL$`dados$dz`), sd(TCL$`dados$dz`))
  
  windows(8,6,title="Exploratory Analysis")
  par(mfrow=c(2,2), family="serif")
  hist(TCL$`dados$dz`, xlab="Average of Discrepancies (m)", ylab= "Frequency", main="Histogram")
  rug(jitter(TCL$`dados$dz`))
  plot(density(TCL$`dados$dz`), xlab="Average of Discrepancies (m)", ylab= "Frequency", main="Density")
  boxplot(TCL$`dados$dz`, xlab="dZ", ylab= "Average of Discrepancies (m)", main="Boxplot")
  qqnorm(TCL$`dados$dz`, xlab="Theoretical Quantiles", ylab= "Sampled Quantities", main="Normal Q-Q Plot")
  qqline(TCL$`dados$dz`,lty=2, col='red')
  par(mfrow=c(1,1), family="serif")

  ### Exporting information:
  
  sink("Resultados.txt", type="output", append=T)
  
  cat("Central Limit Theorem (CLT)","\n",
      "------------------------------------------------------","\n",
      "Original Sample Size: ",length(dados$dz)   ,"\n",
      "Number of Groupings: "         ,k   ,"\n",
      "Defined Sample Size: "         ,Tamanho_amostral   ,"\n",
      "Average Sample Size of Clusters: " ,round(mean(grupos$clusinfo[,1]),3)  ,"\n\n",
      "CLT average (m): "         ,round(mean(TCL$`dados$dz`),3)   ,"\n",
      "CLT variance: "         ,round(var(TCL$`dados$dz`),3)   ,"\n\n",
      "Shapiro-Wilk Test (m?) "        ,"\n",
      "p-value: "         ,shap1$p.value   ,"\n",
      "Normal: "         ,(shap1$p.value>0.05),"\n",
      "\n Kolmogorov-Smirnov test"        ,"\n",
      "p-value: "         ,ks1$p.value   ,"\n",
      "Normal: "         ,(ks1$p.value>0.05),"\n",
      "\np-valeu > 0.05, sample is normal at 5% significance level","\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Resultados.txt")
  
  # Sample obtained by CLT: normal and without outliers
  
  ### Verification
  
  tol <- 0.1 # 10 centimeters
  abs ((1.96*sd(TCL$`dados$dz`)-quantile(TCL$`dados$dz`,0.95))) < tol
  abs((mean(TCL$`dados$dz`)-median(TCL$`dados$dz`))) < tol
  abs((sd(TCL$`dados$dz`)- quantile(TCL$`dados$dz`,0.683))) < tol

  ### Calculation of statistics
  
  ivt1 = theta2(TCL$`dados$dz`,mean(grupos$clusinfo[,1]))
 
  ### Number of samples to estimate CI by bootstrap
  
  amostra=5000
  
  ### Boott - Bootstrap-t Confidence Limits
  
  results.boot2 <- boott(TCL$`dados$dz`,theta2, mean(grupos$clusinfo[,1]), nboott=amostra,VS=FALSE,perc=c(0.025,0.975))
  
  ### Nonparametric BCa Confidence Limits
  
  results.bca2 <- bcanon(TCL$`dados$dz`, amostra, theta2 ,mean(grupos$clusinfo[,1]), alpha=c(0.025, 0.975))
  
  ### Exporting information:
  
  sink("Resultados.txt", type="output", append=T)
  
  cat("Vertical Uncertainty \n Independent, Normal and No Outliers Sample\n CLT","\n",
      "\n Confidence Interval of 95%","\n",
      "------------------------------------------------------","\n",
      "Uncertainty (m): "         ,round(ivt1,3)   ,"\n",
      "IC bootstrap-t (m): "         ,"[",round(results.boot2$confpoints[1,1],3),
      ";",round(results.boot2$confpoints[1,2],3),"]","\n",
      "IC BCa (95%): "         ,"[",round(results.bca2$confpoints[1,2],3),
      ";",round(results.bca2$confpoints[2,2],3),"]","\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Resultados.txt")
  
  # Robust Approach
  
  ### Calculation of statistics
  
  ivt2 = theta3(dados$dz)
  
  ### Number of samples to estimate CI by bootstrap  
  
  amostra1=5000
  
  ### Boott - Bootstrap-t Confidence Limits
  
  results.boot3 <- boott(dados$dz,theta3, nboott=amostra1,VS=FALSE,perc=c(0.025,0.975))
  
  ### Nonparametric BCa Confidence Limits
  results.bca3 <- bcanon(dados$dz, amostra1, theta3,alpha=c(0.025, 0.975))
  
  ### Exporting information:
  
  sink("Resultados.txt", type="output", append=T)
  
  cat("Vertical Uncertainty \n Independent and Non-Normal Sample\n Robust Approach","\n",
      "\n Confidence Interval of 95%","\n",
      "------------------------------------------------------","\n",
      "Robust uncertainty (m): "         ,round(ivt2,3)   ,"\n",
      "IC bootstrap-t (m): "         ,"[",round(results.boot3$confpoints[1,1],3),
      ";",round(results.boot3$confpoints[1,2],3),"]","\n",
      "IC BCa (m): "         ,"[",round(results.bca3$confpoints[1,2],3),
      ";",round(results.bca3$confpoints[2,2],3),"]","\n",
      "\n Mediana (m): "         ,round(median(dados$dz),3)   ,"\n",
      "NMAD (m): "         ,round(mad(dados$dz),3)   ,"\n",
      "Q (0,683) (m): "         ,round(quantile(dados$dz,0.683),3),"\n",
      "Q (0,95) (m): "         ,round(quantile(dados$dz,0.95),3)   ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Resultados.txt")
  
  # Dependent sample
  
  ## Block Bootstrap: Gerar IC com 95%
  
  ### Getting the data
  
  ### Load data without outliers
  
  dados <- read.table("dados_semout_boxplot.txt", header=T, dec=",")
  coordinates(dados) <- c("X", "Y")
  
  ### Calculation of statistics
  
  ivt3 = theta(dados$dz)
  rms1 = theta1(dados$dz)
  ivt_robs = theta3(dados$dz)
  
  ### Generate blocks with a predefined diagonal size
  
  ### suggestion: Range obtained from Geostatistical analysis
  
  tamanho <- 200
  
  ### data.frame
  
  #data(dados$dz)
  #coordinates(dados$dz) <- ~x+y
  #gridded(dados$dz) <- TRUE
  #bbox(dados$dz)
  
  #data(dados)
  #coordinates(dados) <- ~x+y
  #proj4string(dados) <- CRS("+init=epsg:28992")
  
  ### Delimit the number and location of each block
  
  Bloco <- makegrid(bbox(dados), cellsize = (tamanho*sqrt(2)), pretty = FALSE)
  coordinates(Bloco) <- c("x1","x2")
  gridded(Bloco) <- TRUE
  Bloco <- as.SpatialPolygons.GridTopology(Bloco@grid)
  plot(Bloco)     # Plotting the Blocks
  points(dados) # Plotting the original data
  
  ### Extract the Block number where each point is overlapping
  
  ptsInBloco <- as.numeric(gIntersects(dados, Bloco, byid=TRUE, 
                                       returnDense=FALSE, checkValidity=TRUE))  # All that intersect for each buffer
  
  ### Number of Bootstrap replications
  
  n_vezes <- 500  
  
  tab_boot <- tab_boot_dz <- NULL 
  
  for (i in 1:n_vezes)   
  {
    # FIRST - Block draw
    Grid <- sample(unique(ptsInBloco),dim(dados)[1], replace = TRUE)
    # SECOND - drawing a point within each Block selected previously
    pontos <- (as.numeric(lapply(Grid,function(x) sample(which(ptsInBloco==x),1))))  # Repeated points are counted only once
    tab_boot <- rbind(tab_boot,(pontos))
    tab_boot_dz <- rbind(tab_boot_dz,dados@data$dz[pontos])
  }
  
  ### Convert data to data.frame
  
  tab_boot <- as.data.frame(tab_boot)
  tab_boot_dz <- as.data.frame(tab_boot_dz)
  
  ### Generate a new dataset after the bootstrap block
  
  dados_novos_ivt <- apply(tab_boot_dz,1,theta) 
  dados_novos_rms <- apply(tab_boot_dz,1,theta1) 
  dados_novos_robs <- apply(tab_boot_dz,1,theta3) 
  
  ### Block bootstrap confidence intervals
  
  IC_ivt <- quantile(dados_novos_ivt,c(0.025, 0.975)) 
  IC_rms <- quantile(dados_novos_rms,c(0.025, 0.975)) 
  IC_robs <- quantile(dados_novos_robs,c(0.025, 0.975)) 
  
  ### Bias
  
  vies_ivt <- ivt3 - median(dados_novos_ivt)
  vies_rms <- rms1 - median(dados_novos_rms)
  vies_robs <- ivt_robs - median(dados_novos_robs)
  
  windows(8,8,title="Gráficos para análise exploratória")
  par(mfrow=c(3,2), family="serif")
  hist(dados_novos_ivt, xlab="Uncertainty (m)", ylab= "Frequency", main="Histogram (bootstrap)")
  qqnorm(dados_novos_ivt, xlab="Theoretical Quantiles", ylab= "Sampled Quantities", main=" Normal Q-Q Plot (bootstrap)")
  qqline(dados_novos_ivt,lty=2, col='red')
  hist(dados_novos_rms, xlab="RMSE (m)", ylab= "Frequency", main=" Histogram (bootstrap)")
  qqnorm(dados_novos_rms, xlab="Theoretical Quantiles", ylab= "Sampled Quantities", main=" Normal Q-Q Plot (bootstrap)")
  qqline(dados_novos_rms,lty=2, col='red')
  hist(dados_novos_robs, xlab="Incerteza Robusta (m)", ylab= "Frequency", main=" Histogram (bootstrap)")
  qqnorm(dados_novos_robs, xlab="Theoretical Quantiles", ylab= "Sampled Quantities", main=" Normal Q-Q Plot (bootstrap)")
  qqline(dados_novos_robs,lty=2, col='red')
  par(mfrow=c(1,1), family="serif")

  ### Exporting information:
  
  sink("Resultados.txt", type="output", append=T)
  
  cat("Vertical Uncertainty \n Dependent Sample - Bootstrap Block","\n",
      "Number of replicas: " ,n_vezes,"\n",
      "Block side size (m): " ,round (tamanho*sqrt(2),3),"\n",
      "\n Confidence Interval of 95%","\n",
      "------------------------------------------------------","\n",
      "Uncertainty (m): "         ,round(ivt3,3)   ,"\n",
      "IC (m): "         ,"[",round(IC_ivt[1],3),";",round(IC_ivt[2],3),"]","\n",
      "Bootstrap bias (m): "         ,round(vies_ivt,3)   ,"\n",
      
      "\n RMSE (m): "         ,round(rms1,3)   ,"\n",
      "IC (m): "         ,"[",round(IC_rms[1],3),";",round(IC_rms[2],3),"]","\n",
      "Bootstrap bias (m): "         ,round(vies_rms,3)   ,"\n",

      "\n Robust uncertainty(m): "         ,round(ivt_robs,3)   ,"\n",
      "IC (m): "         ,"[",round(IC_robs[1],3),";",round(IC_robs[2],3),"]","\n",
      "Bootstrap bias (m): "         ,round(vies_robs,3)   ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Resultados.txt")
