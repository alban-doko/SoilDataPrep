SoilParamsCont<-function(catch, DEM, c=500){
  
  #Adjust catchment projection to WGS84 longlat
  wgs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  catch <- spTransform(catch, wgs)
  
  #Crop DEM to the size of the catchment's bbox
  DEM<-crop(DEM, bbox(catch))
  
  #Divide study area into tiles
  v<-ceiling(nrow(DEM)/c) #number of vertical tiles
  h<-ceiling(ncol(DEM)/c) #number of horizontal tiles
  vcells<-ceiling(nrow(DEM)/v) 
  hcells<-ceiling(ncol(DEM)/h)
  print(paste("Number of tiles to calculate:", v, "x", h, "=", h*v))
  
  #Define factors to adapt different raster resolutions
  f_d<-floor(0.008333333/res(DEM)[1])
  f_a<-floor(0.002083333/res(DEM)[1])
  
  soil_sum_collected<-read.table("soil_sum_collected.txt") #results of former run, to aggregate all results
  
  #Start from last treated tile of former run
  start<-read.table("last_tile.txt", header=T)
  
  for (a in start$a:v){
    for (b in start$b:h){
      
      write.table(x=data.frame("a"=a, "b"=b), file="last_tile.txt", row.names = F, sep=" \t")
      print(paste("Treating tile", a,b, Sys.time(), "Memory in use:", memory.size(max=F)))
      
      #Crop DEM to extent of current tile
      dem<-crop(DEM, extent(DEM,((a-1)*vcells +1), a*vcells,((b-1)*hcells +1), b*hcells))    
      dem<-mask(x=dem, mask=catch) #set cells outside the catchment to NA
      
      #Jump tiles outside the catchment/study area
      if(sum(is.na(getValues(dem)))==length(getValues(dem))) next
      
      d5<-raster("Pelletier_DTB/depth_5.tif")
      d5<-crop(d5,dem)
      d6<-raster("Pelletier_DTB/depth_6.tif")
      d6<-crop(d6,dem)
      slope<-terrain(dem, opt="slope", unit="degrees", neighbors=8)
      
      #Adjust raster resolution: d5,6 (1 km resolution) to DEM
      d5<-disaggregate(d5, fact=f_d)
      d6<-disaggregate(d6, fact=f_d)
      d5<-resample(d5, dem, method="bilinear")
      d6<-resample(d6, dem, method="bilinear")
      rm(dem)
      
      #Create d5 cells
      dfun<-function(slope){ifelse(slope>=20, 1,0)}
      depth_5<-calc(slope, fun=dfun)
      depth_5<-depth_5*d5
      
      #Create d6 cells
      dfun<-function(slope){ifelse(slope<20, 1,0)}
      depth_6<-calc(slope, fun=dfun)
      depth_6<-depth_6*d6
      rm(slope)
      rm(d5)
      rm(d6)
      
      #Depth=d5+d6
      depth<- depth_5+depth_6
      rm(depth_5)
      rm(depth_6)
      
      #Adjust raster resolution: depth (=DEM resolution) to SoilGrids (250 m)
      depth<-aggregate(depth, fact=f_a)
      soils<-raster("SoilGrids/TAXNWRB.tif")
      soils<-crop(soils, depth)
      depth<-resample(depth, soils, method="bilinear")
      
      #Aluvial
      afun<-function(depth){ifelse(depth>=3, 1,0)}
      aluvial<-calc(depth, fun=afun)
      
      #Create new soil ids: if alluvial +1000
      sfun<-function(aluvial){ifelse(aluvial==1,1000,0)} 
      new<-calc(aluvial, fun=sfun)
      soils<-soils+new
      writeRaster(soils, file=paste0("MapSoils/soils_", a,"_", b,".tif"), overwrite=T)
      
      aluvial[is.na(aluvial)]=0 #mask NAs
      soil_sum = aggregate(x=data.frame(aluvial=getValues(aluvial), total_depth=getValues(depth)), by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
      names(soil_sum)[-1]=c("aluvial","depth")
      rm(aluvial)
      rm(depth)
      
      #Make sure that depth values are never negative
      for (di in 1:length(soil_sum$depth)){
        if(soil_sum$depth[di] <0)
          warning(paste("For tile no. ", a, b, di,": Depth values < 0 appear!"))}
      
      #Apply PTFs to each horizon####
      for (soillayer in c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"))
      {
        print(paste("- soillayer", soillayer, Sys.time(), "Memory in use:", memory.size(max=F)))
        
        clay<-raster(paste0("SoilGrids/CLYPPT_M_", soillayer, ".tif"))
        clay<-crop(clay, soils)
        silt<-raster(paste0("SoilGrids/SLTPPT_M_", soillayer, ".tif"))
        silt<-crop(silt, soils)
        sand<-raster(paste0("SoilGrids/SNDPPT_M_", soillayer, ".tif"))
        sand<-crop(sand, soils)
        coarse<-raster(paste0("SoilGrids/CRFVOL_M_", soillayer, ".tif"))
        coarse<-crop(coarse, soils)
        bulkD<-raster(paste0("SoilGrids/BLDFIE_M_", soillayer, ".tif"))
        bulkD<-crop(bulkD,soils)
        om<-raster(paste0("SoilGrids/ORCDRC_M_", soillayer, ".tif"))
        om<-crop(om, soils)
        ph<-raster(paste0("SoilGrids/PHIHOX_M_", soillayer, ".tif"))
        ph<-crop(ph, soils)
        cec<-raster(paste0("SoilGrids/CECSOL_M_", soillayer, ".tif"))
        cec<-crop(cec,soils)
        
        soil_attributes=data.frame(clay  =getValues(clay),
                                   silt  =getValues(silt),
                                   bulkD  =getValues(bulkD)/1000,
                                   om     =1.74 * getValues(om)/10, #convert OC to OM
                                   coarse =getValues(coarse),
                                   topSoil=as.numeric(soillayer=="sl1"))
        
        euptf_attributes=data.frame(TOPSOIL= ifelse(soillayer=="sl1", "top","sub"),
                                    USSAND= getValues(sand),
                                    USSILT = getValues(silt),
                                    USCLAY  = getValues(clay),
                                    OC = getValues(om)/10,
                                    BD = getValues(bulkD)/1000,
                                    PH_H2O = getValues(ph)/10,
                                    CEC = getValues(cec)) # 1 cmolc/kg = 1 meq/100g
        
        rm(clay)
        rm(silt)
        rm(sand)
        rm(bulkD)
        rm(om)
        rm(coarse)
        rm(ph)
        rm(cec)
        
        
        #Calculate theta_r/pwp/_s and ks with {euptf}####
        ptf_props<-NULL
        
        #Water content at permanent wilting point (1500 kPa/15000 cm)
        ptf_props=data.frame(theta_pwp = predict.ptf(newdata=euptf_attributes, ptf="PTF12"))
        
        #Saturated water content
        ptf_props$theta_s<- predict.ptf(newdata=euptf_attributes, ptf="PTF06")
        
        #Saturated hydraulic conductivity (horizons) [cm/d]    	
        ptf_props$ks<- (10^(predict.ptf(newdata=euptf_attributes, ptf="PTF17")))*10
        
        #Calculate theta_s/2.5/1.8,s_f, h_b and lambda with ptf.rawls####  
        #Residual water content
        ptf_props$theta_r=pft.rawls(soilprop=soil_attributes, h=0, parameters="theta_r")[,"theta_r"]
        
        #Water content at field capacity (316 hPa / pF=2.6)
        ptf_props$theta_2.5=pft.rawls(soilprop=soil_attributes, h=316, parameters="theta")[,"theta"]
        
        #Water content at field capacity (63 hPa / pF=1.8)
        ptf_props$theta_1.8=pft.rawls(soilprop=soil_attributes, h=63, parameters="theta")[,"theta"]
        
        #Suction at the wetting front (horizons) [mm]
        ptf_props$S_f=pft.rawls(soilprop=soil_attributes, parameters="S_f")[,"S_f"]
        
        #Bubbling pressure (horizons) [cm]
        ptf_props$h_b = pft.rawls(soilprop=soil_attributes, parameters="h_b")[,"h_b"]
        
        #Pore-size-index (horizons) [-]
        ptf_props$lambda = pft.rawls(soilprop=soil_attributes, parameters="lambda")[,"lambda"]
        
        
        #Aggregate properties from basic horizon input data####
        soil_sum2  = aggregate(x=soil_attributes, by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
        soil_sum2$cellcount = table(getValues(soils))
        names(soil_sum2)[-1]=paste0(soillayer, names(soil_sum2)[-1]) #adjust column names
        soil_sum = merge(soil_sum, soil_sum2) 
        
        #Aggregate properties from PTFs####
        soil_sum2 = aggregate(x=ptf_props, by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
        soil_sum2$nfk=soil_sum2$theta_2.5-soil_sum2$theta_r          #compute nfk
        names(soil_sum2)[-1]=paste0(soillayer, names(soil_sum2)[-1]) #adjust column names
        soil_sum = merge(soil_sum, soil_sum2)   
        write.table(x=soil_sum, file="soil_sum_recent.txt", sep="\t")
        rm(soil_sum2)
      }
      
      soil_sum_collected = rbind(soil_sum_collected, soil_sum) #collect results of single tiles
      write.table(x=soil_sum_collected, file="soil_sum_collected.txt", sep=" \t")
    }}
  
  #--------------------------------------------------------------------------------------
  soil_sum = soil_sum_collected #results of all tiles together
  soil_means<- data.table(soil_sum)
  soil_means<- computeWeightedMeans(data_table=soil_means, weight=soil_means$sl7cellcount, by=soil_means$soil_id )
  soil_sum<-data.frame(soil_means)
  rm(soil_means)
  write.table(x=soil_sum, file="soil_sum_weighted.txt", sep=" \t")
  
  #Thickness [mm]
  soil_sum$sl1thickness = 25 
  soil_sum$sl2thickness = 75
  soil_sum$sl3thickness = 120
  soil_sum$sl4thickness = 230
  soil_sum$sl5thickness = 350
  soil_sum$sl6thickness = 700
  soil_sum$sl7thickness = 500
  
  #Convert unit of coarse from % to [-]
  soil_sum$sl1coarse = soil_sum$sl1coarse / 100 
  soil_sum$sl2coarse = soil_sum$sl2coarse / 100 
  soil_sum$sl3coarse = soil_sum$sl3coarse / 100
  soil_sum$sl4coarse = soil_sum$sl4coarse / 100
  soil_sum$sl5coarse = soil_sum$sl5coarse / 100
  soil_sum$sl6coarse = soil_sum$sl6coarse / 100
  soil_sum$sl7coarse = soil_sum$sl7coarse / 100
  
  
  #Prepare output files to be imported into make_wasa_db####
  #For table soils
  write.table(file="soils.txt", x=data.frame(pid=soil_sum$soil_id, desc="NA", bedrock=1, aluvial=soil_sum$aluvial, b_om=soil_sum$sl1om),
              sep="\t", quote=FALSE, row.names=FALSE)
  
  #For table horizons
  hor_fields=c("pid","descr","soil_id","position","theta_r", "theta_pwp", "theta_2.5", "theta_1.8", "nfk", "theta_s", "thickness", "ks", "S_f", "lambda", "h_b","coarse")
  write.table(file="horizons.txt", x=t(hor_fields),
              sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
  
  hcounter=0 #horizon counter
  for (soil_id in unique(soil_sum$soil_id))
  {
    srow=which(soil_sum$soil_id==soil_id)
    for (soillayer in c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"))
    {
      hcounter=hcounter+1 
      if (soil_sum[srow, paste0(soillayer, "thickness")]<=0) next #skip non-exiting subsoils
      
      hfields=intersect (paste0(soillayer, hor_fields), names(soil_sum) ) #fields to extract for current horizon
      oline=soil_sum[srow, hfields] #extract the current horizon
      oline=cbind(pid=hcounter, descr="", soil_id=soil_id, 
                  position = as.numeric(sub(soillayer,pattern = "sl", repl="")),
                  oline)
      
      write.table(file="horizons.txt", x=oline,
                  sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE, append=TRUE)
    }}
  
  #For table r_soil_contains_particles (only topsoil is considered)  
  soil_sum$sl1sand=100-(soil_sum$sl1silt+soil_sum$sl1clay)
  soil_idss=rep(soil_sum$soil_id, each=3)
  pclass  =rep(1:3, nrow(soil_sum))
  tt=matrix(c(soil_sum$sl1clay, soil_sum$sl1silt,soil_sum$sl1sand)/100, nrow=length(soil_sum$sl1clay))
  frac    =as.vector(t(tt))
  write.table(file="r_soil_contains_particles.txt", 
              x=data.frame(soil_id=soil_idss,pclass=pclass,frac=frac),
              sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
  
  #For table particle_classes
  write.table(file="particle_classes.txt", 
              x=data.frame(class_id=1:3,desc=c("clay","silt","sand"), upper_limit=c(0.002,0.05,2)),
              sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
  
  
  #Create output map####
  #Read in all soil id maps, with new soil ids for alluvium and then merge them to one map
  
  l_soils<-list()
  x<-list.files("MapSoils")
  for (i in 1:length(x)){
    l_soils[[i]]<-raster(paste0("MapSoils/",x[i]))}
  m_soils<-do.call(raster::merge, l_soils)
  writeRaster(m_soils, file="Mapsoils/soils_catchment.tif", overwrite=T)
  
}
    