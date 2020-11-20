//########################################################################################
//# Written by: Adapted from work by Jeff and Sherrie
//# Last edited by: Anne, Sept 2020
//# Gets the revisit rates for individual public satellites from GEE. Used for Figure 3
//########################################################################################

//file that downloads (to Google Drive) how often public satellites
// revisit each of randomly selected locations across africa and europe

//must be run 4 times, changing the dhs file to africa and europe, for 
// both 2010 and 2019

// last edited by Anne Driscoll in Aug 2020

var modis = ee.ImageCollection("MODIS/006/MOD09GA"),
    l7 = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR"),
    l5 = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR"),
    l8 = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR"),
    s2 = ee.ImageCollection("COPERNICUS/S2");

var dhs = ee.FeatureCollection("users/adriscoll/africa_loc");
var start_date = "2010-01-01"
var end_date = "2010-12-31"

var maskClouds = function(image) {
  // Select the QA band.
  var QA = image.select('state_1km')
  // Make a mask to get bit 10, the internal_cloud_algorithm_flag bit.
  var bitMask = 1 << 10;
  // Return an image masking out cloudy areas.
  return image.updateMask(QA.bitwiseAnd(bitMask).eq(0))
}
var getPixelsCloudless = function(image) {
  return(image.set("PixelsCloudless", image.select("sur_refl_b01")
              .reduceRegion({reducer: ee.Reducer.count()})
              .get("sur_refl_b01")))
} 
var getPixels = function(image) {
  return(image.set("Pixels", image.select("sur_refl_b01").unmask()
              .reduceRegion({reducer: ee.Reducer.count()})
              .get("sur_refl_b01")))
} 
var clipImageCollection = function(ic, geom){
  return(ic.map(function(image){return ee.Image(image).clip(geom)}))
}

// count number of images for every sampled DHS locations
var add_num_img = function(feature) {
  
  var poly = feature.geometry().buffer(1000).bounds(); //get dhs point
 
  var fc_l5 = l5.filterBounds(poly).filter(ee.Filter.neq('CLOUD_COVER', -1))
    .filter(ee.Filter.lte('CLOUD_COVER', 30))
    .filterDate(start_date, end_date);
  
  var fc_l7 = l7.filterBounds(poly).filter(ee.Filter.neq('CLOUD_COVER', -1))
    .filter(ee.Filter.lte('CLOUD_COVER', 30))
    .filterDate(start_date, end_date);
  
  var fc_l8 = l8.filterBounds(poly).filter(ee.Filter.neq('CLOUD_COVER', -1))
    .filter(ee.Filter.lte('CLOUD_COVER', 30))
    .filterDate(start_date, end_date);
  
  var fc_s2 = s2.filterBounds(poly)
    .filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', 30))
    .filterDate(start_date, end_date);

  //do all the modis processing
  var fc_modis = modis.filterBounds(poly)
    .filterDate(start_date, end_date) //filter time and loc
    
  fc_modis = clipImageCollection(fc_modis, poly); //clip to location
  
  fc_modis = fc_modis.map(maskClouds) //mask clouds
    .map(getPixelsCloudless) // calc num w/o clouds
    .map(getPixels); //calc num w + w/o clouds
    
  var perc = fc_modis.first().get("Pixels")
  
  perc = ee.Number(perc).multiply(0.7).ceil()
  fc_modis =  fc_modis.filterMetadata('PixelsCloudless', "greater_than", perc);
  
  //combine everything for output
  feature = feature.set('num_l7', fc_l7.size())
    .set('num_l8', fc_l8.size())
    .set('num_l5', fc_l5.size())
    .set('num_s2', fc_s2.size())
    .set('num_modis', fc_modis.size())

  return feature;
}

var dhs_new = ee.FeatureCollection(dhs.map(add_num_img));
Export.table.toDrive({
  collection: dhs_new,
  description: 'Africa_overpass_' + start_date,
  fileFormat: 'CSV'
});

