SoilParams<-function(catch, DEM, c=1000, resume=FALSE){
  
  #Adjust catchment projection to WGS84 longlat
  print("reprojecting maps...")
  wgs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  catch <- spTransform(catch, wgs)
  
  if (projection(DEM)==projection(catch)|projection(DEM)=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
    #only do reprojection if necessary 
    DEM_latlon =DEM else
    DEM_latlon <- projectRaster(DEM, crs = wgs)   #takes some time
  print("reprojecting done.")
  

  #Crop DEM to the size of the catchment's bbox
  DEM<-crop(DEM_latlon, bbox(catch))
  
  
  #Divide study area into tiles
  v<-ceiling(nrow(DEM)/c) #number of vertical tiles
  h<-ceiling(ncol(DEM)/c) #number of horizontal tiles
  vcells<-ceiling(nrow(DEM)/v) 
  hcells<-ceiling(ncol(DEM)/h)
  print(paste("Number of tiles to calculate:", v, "x", h, "=", h*v))
  
  #Define factors to adapt different raster resolutions
  f_d<-floor(0.008333333/res(DEM)[1])
  f_a<-floor(0.002083333/res(DEM)[1])

    
  
if (resume) #Start from last treated tile of former run
{
  if (any (!file.exists("soil_sum_collected.txt", "last_tile.txt")))
  {
    print(paste("soil_sum_collected.txt or last_tile.txt not found, cannot resume, start from beginning."))
    resume=FALSE
  } else
  {
    print("Resuming from previous run...")
    soil_sum_collected<-read.table("soil_sum_collected.txt") #results of former run, to aggregate all results
    #Start from last treated tile of former run
    start<-read.table("last_tile.txt", header=T)
  }
}

if (!resume) #Start new run, do not resume
{
  soil_sum_collected = NULL #aggregates results of single tiles
  dir.create("MapSoils") #Create a folder to store map of new soil ids
  unlink("MapSoils/*.*") #delete maps of prior runs
  start=data.frame(a=1, b=1) #Start from the first tile####
 } 
  
  
 for (a in start$a:v){
   for (b in 1:h){
     
     if (a == start$a & b < start$b) next  #fast-forward to specified beginning
      
      write.table(x=data.frame("a"=a, "b"=b), file="last_tile.txt", row.names = F, sep=" \t")
      print(paste("Treating tile", a,b, Sys.time(), "Memory in use:", memory.size(max=F)))
      
      #Crop DEM to extent of current tile
      dem<-crop(DEM, extent(DEM,((a-1)*vcells +1), a*vcells,((b-1)*hcells +1), b*hcells))    
      dem<-mask(x=dem, mask=catch) #set cells outside the catchment to NA
      
      #Jump tiles outside the catchment/study area
      if(sum(is.na(getValues(dem)))==length(getValues(dem))) 
      {
        print("empty tile, skipped.")
        next
      }  
      
      d5<-raster("Pelletier_DTB/depth_5.tif") #"hillslope-soildepth"
      d5<-crop(d5,dem, snap="out")
      d5[d5<=0]<-NA #apparently, nodata and 0 are not clearly distinguished, so we assume NA for cells with 0 
      d6<-raster("Pelletier_DTB/depth_6.tif") #"valley-bottom soildepth"
      d6<-crop(d6,dem, snap="out")
      d6[d6<=0]<-NA
      slope<-terrain(dem, opt="slope", unit="degrees", neighbors=8)
      
      #Adjust raster resolution: Pelletier d5,6 (1 km resolution) to DEM
      d5 = resample(x=d5, y=dem, method="ngb")
      d6 = resample(x=d6, y=dem, method="ngb")
      #plot(d5)
      #plot(d5_2)
      
      # d5<-disaggregate(d5, fact=f_d)
      # 
      # 
      # d6<-disaggregate(d6, fact=f_d)
      # d5<-resample(d5, dem, method="bilinear")
      # d6<-resample(d6, dem, method="bilinear")
      
      rm(dem)
      
      #Create d5 mask - mark steep cells as "hillslope"
      dfun<-function(slope){ifelse(slope>=20, 1,0)}
      hillslopes<-calc(slope, fun=dfun)
      depth_5<-hillslopes*d5
      depth_5[hillslopes==0]<-0 #enable superposition by addition
      depth_5[hillslopes==1 & is.na(d5)] <-0 #set true soil depth of hillslopes to 0
      
      
      
      #Create d6 mask - mark flat cells as "valley bottom"
      #depth_6<- 1-depth_5 #complement of hillslopes
      #plot(depth_6)
      depth_6 <- (!hillslopes)*d6
      depth_6[hillslopes==1]<-0 #set contribution of this layer on hillslopes to 0
      

      rm(slope)
      rm(d5)
      rm(d6)
      
      depth<- depth_5+depth_6 #superpose depth grids
      
      # summary(depth_5)
      # summary(depth_5[depth_5[]!=0])
      # summary(depth_6[depth_6[]!=0])
      # 
      # min(which(depth_5[]<=0.33 & depth_5[]!=0))
      # depth_5[657354]
      # depth_6[657354]
      # 
      # summary(depth_6)
      # summary(depth)
      # summary(depth[depth[]!=0])
      # 
      # plot(depth)
      rm(depth_5)
      rm(depth_6)
      
      
#      depth<-aggregate(depth, fact=f_a)

      #load soil grids soil taxonomy
      soils<-raster("SoilGrids/TAXNWRB.tif")
      soils[]=as.integer(soils[]) #convert to integer
      dataType(soils)="INT2S"
      
      soils<-crop(soils, depth, snap="out")
      #depth<-resample(depth, soils, method="bilinear")
      
      soils = resample(x=soils, y=depth, method="ngb") #Adjust raster resolution: SoilGrids (250 m) to depth (=DEM resolution)
      
      #alluvial
      afun<-function(depth){ifelse(depth>=3, 1,0)}
      alluvial<-calc(depth, fun=afun)
      #plot(alluvial)
      #plot(depth)
      
      #Create new soil ids: if alluvial +1000
      sfun<-function(alluvial){ifelse(alluvial==1,1000,0)} 
      new<-calc(alluvial, fun=sfun)
      soils<-soils+new
      #plot(soils)
      #plot(soils2)
      #click(soils2)
      
      soils[]=as.integer(soils[]) #convert to integer
      dataType(soils)="INT2S"
      writeRaster(soils, file=paste0("MapSoils/soils_", a,"_", b,".tif"), overwrite=T, datatype="INT2S")
      
      alluvial[is.na(alluvial)]=0 #mask NAs
      soil_sum_tile = aggregate(x=data.frame(alluvial_flag=getValues(alluvial), depth=getValues(depth)), by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
      
      counts = as.data.frame(table(getValues(soils))) #tabulate soil-ID frequency
      names(counts)=c("soil_id","cellcount")
      soil_sum_tile=merge(soil_sum_tile, counts) #add cellcounts
      soil_sum_tile$tile_a= a #record current tile number
      soil_sum_tile$tile_b= b
      
      rm(alluvial)
      rm(depth)
      
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
       # rm(cec) #keep this a a template
        
        
        #Calculate theta_r/pwp/_s and ks with {euptf}####
        #Water content at permanent wilting point (1500 kPa/15000 cm)
        ptf_props=data.frame(theta_pwp = predict.ptf(newdata=euptf_attributes, ptf="PTF12"))
        
        #Saturated water content
        ptf_props$theta_s<- predict.ptf(newdata=euptf_attributes, ptf="PTF06")
        
        #Saturated hydraulic conductivity (horizons) [cm/d]    	
        ptf_props$ks<- (10^(predict.ptf(newdata=euptf_attributes, ptf="PTF17")))*10
        
        #Calculate theta_s/2.5/1.8,suction, h_b and lambda with ptf.rawls####  
        #Residual water content
        ptf_props$theta_r=pft.rawls(soilprop=soil_attributes, h=0, parameters="theta_r")[,"theta_r"]
        
        #Water content at field capacity (316 hPa / pF=2.6)
        ptf_props$fk=pft.rawls(soilprop=soil_attributes, h=316, parameters="theta")[,"theta"]
        
        #Water content at field capacity (63 hPa / pF=1.8)
        ptf_props$fk63=pft.rawls(soilprop=soil_attributes, h=63, parameters="theta")[,"theta"]
        
        #Suction at the wetting front (horizons) [mm]
        ptf_props$suction=pft.rawls(soilprop=soil_attributes, parameters="suction")[,"suction"]
        
        #Bubbling pressure (horizons) [cm]
        ptf_props$bubb_pres = pft.rawls(soilprop=soil_attributes, parameters="h_b")[,"h_b"]
        
        #Pore-size-index lambda (horizons) [-]
        ptf_props$pore_size_i = pft.rawls(soilprop=soil_attributes, parameters="lambda")[,"lambda"]
        
        #str(soil_attributes)
        
        soil_attributes=cbind(soil_attributes, ptf_props) #combine all acquired attributes in a single table
        
        soil_sum_layer=data.frame(soil_id=soil_sum_tile$soil_id)
        #aggregate coarse grids by finer resolution soil-ID grid
        for (s_attribute in names(soil_attributes))
        {
          #s_attribute="clay"
          t_raster=cec  #use this 250-m-grid as template
          t_raster[]=soil_attributes[, s_attribute]   #put computed values into grid
          t_raster2=resample(x=t_raster, y=soils, method="ngb")  #resample to DEM-resolution
          attr_aggregated  = aggregate(x=getValues(t_raster2), by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
          names(attr_aggregated)[2]=s_attribute #rename column
          soil_sum_layer = merge(soil_sum_layer, attr_aggregated, all=T) #add this attribute to collection
        }

        soil_sum_layer$nfk=soil_sum_layer$fk-soil_sum_layer$theta_r          #compute nfk
        names(soil_sum_layer)[-1]=paste0(soillayer, names(soil_sum_layer)[-1]) #add layer number to column names      
        
        
        # #Aggregate properties from basic horizon input data####
        # soil_sum2  = aggregate(x=soil_attributes, by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
        # soil_sum2$cellcount = table(getValues(soils))
        # soil_sum2$tile_a= a
        # soil_sum2$tile_b= b
        # names(soil_sum2)[-1]=paste0(soillayer, names(soil_sum2)[-1]) #adjust column names

        soil_sum_tile = merge(soil_sum_tile, soil_sum_layer) #add current layer to collection done over all layers of this tile
        #soil_sum_tile <- unique(soil_sum_tile)
        
        #Aggregate properties from PTFs####
        #soil_sum2 = aggregate(x=ptf_props, by=list(soil_id=getValues(soils)), FUN=mean, na.rm=TRUE) #aggregate according to soil_id
        #soil_sum2$nfk=soil_sum2$fk-soil_sum2$theta_r          #compute nfk
        #names(soil_sum2)[-1]=paste0(soillayer, names(soil_sum2)[-1]) #adjust column names
        #soil_sum_tile = merge(soil_sum_tile, soil_sum2)   
        write.table(x=soil_sum_tile, file="soil_sum_recent.txt", sep="\t", quote=FALSE)
        rm(soil_sum_layer)
      }
      
      soil_sum_collected = rbind(soil_sum_collected, soil_sum_tile) #collect results of single tiles
      write.table(x=soil_sum_collected, file="soil_sum_collected.txt", sep=" \t", quote=FALSE)
    }}
  
  #--------------------------------------------------------------------------------------
  #### aggregate results of all tiles, weighted by number of cells found ####
  soil_means<- data.table(soil_sum_collected)
  soil_means<- computeWeightedMeans(data_table=soil_means, weight=soil_means$cellcount, by=soil_means$soil_id )
  soil_means2<-data.frame(soil_means)
  rm(soil_means)
  write.table(x=soil_means2, file="soil_sum_weighted.txt", sep=" \t")
  
  #Thickness [mm]
  soil_means2$sl1thickness = 25 
  soil_means2$sl2thickness = 75
  soil_means2$sl3thickness = 120
  soil_means2$sl4thickness = 230
  soil_means2$sl5thickness = 350
  soil_means2$sl6thickness = 700
  soil_means2$sl7thickness = 500
  
  #Convert unit of coarse from % to [-]
  soil_means2$sl1coarse = soil_means2$sl1coarse / 100 
  soil_means2$sl2coarse = soil_means2$sl2coarse / 100 
  soil_means2$sl3coarse = soil_means2$sl3coarse / 100
  soil_means2$sl4coarse = soil_means2$sl4coarse / 100
  soil_means2$sl5coarse = soil_means2$sl5coarse / 100
  soil_means2$sl6coarse = soil_means2$sl6coarse / 100
  soil_means2$sl7coarse = soil_means2$sl7coarse / 100
  
  
  #Prepare output files to be imported into make_wasa_db####
  #For table soils
  write.table(file="soil.dat", x=data.frame(pid=soil_means2$soil_id, description="NA", bedrock_flag=1, alluvial=soil_means2$alluvial, b_om=soil_means2$sl1om),
              sep="\t", quote=FALSE, row.names=FALSE)
  
  #For table horizons
  hor_fields=c("pid","description","soil_id","position","theta_r", "theta_pwp", "fk", "fk63", "nfk", "theta_s", "thickness", "ks", "suction", "pore_size_i", "bubb_pres","coarse_frag")
  write.table(file="horizons.dat", x=t(hor_fields),
              sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
  
  hcounter=0 #horizon counter
  for (soil_id in unique(soil_means2$soil_id))
  {
    srow=which(soil_means2$soil_id==soil_id)
    for (soillayer in c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"))
    {
      hcounter=hcounter+1 
      if (soil_means2[srow, paste0(soillayer, "thickness")]<=0) next #skip non-exiting subsoils
      
      hfields=intersect (paste0(soillayer, hor_fields), names(soil_means2) ) #fields to extract for current horizon
      oline=soil_means2[srow, hfields] #extract the current horizon
      oline=cbind(pid=hcounter, description="", soil_id=soil_id, 
                  position = as.numeric(sub(soillayer,pattern = "sl", repl="")),
                  oline)
      
      write.table(file="horizons.dat", x=oline,
                  sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE, append=TRUE)
    }}
  
  #For table r_soil_contains_particles (only topsoil is considered)  
  soil_means2$sl1sand=100-(soil_means2$sl1silt+soil_means2$sl1clay)
  soil_idss=rep(soil_means2$soil_id, each=3)
  class_id  =rep(1:3, nrow(soil_means2))
  tt=matrix(c(soil_means2$sl1clay, soil_means2$sl1silt,soil_means2$sl1sand)/100, nrow=length(soil_means2$sl1clay))
  frac    =as.vector(t(tt))
  write.table(file="r_soil_contains_particles.dat", 
              x=data.frame(soil_id=soil_idss,class_id=class_id,fraction=frac),
              sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
  
  #For table particle_classes
  write.table(file="particle_classes.dat", 
              x=data.frame(class_id=1:3,description=c("clay","silt","sand"), upper_limit=c(0.002,0.05,2)),
              sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
  
  
  #Create output map####
  #Read in all soil id maps, with new soil ids for alluvium and then merge them to one map
  

  l_soils<-list()
  x<-list.files("MapSoils", pattern="^soils_[0-9].*\\.tif$") #avoid reading in an old merged map or other files
  for (i in 1:length(x))
  l_soils[[i]]<-raster(paste0("MapSoils/",x[i]))
  m_soils<-do.call(raster::merge, l_soils)

  #storage.mode(m_soils[])  
  m_soils[]=as.integer(m_soils[]) #convert to integer
  dataType(m_soils)="INT2S"
  
  writeRaster(m_soils, file="Mapsoils/soils_catchment.tif", overwrite=T, datatype="INT2S")

}
