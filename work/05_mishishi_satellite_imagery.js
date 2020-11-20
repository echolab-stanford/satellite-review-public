//########################################################################################
//# Written by: Written by Anne Driscoll, Sept 2019
//# Last edited by: Anne, Sept 2020
//# Gets imagery from various public satellites for Figure 3 E-G
//########################################################################################

//file that downloads (to Google Drive) the images from different satellites at 
// different time points for Figure 4

var bungoma = ee.Geometry.Point([34.561520, 0.563276])
var eldoret = ee.Geometry.Rectangle([35.235, 0.48, 35.275, 0.52])
eldoret = ee.Geometry.Rectangle([34.545, 0.545, 34.585, 0.585]) //actually bungoma
eldoret = ee.Geometry.Rectangle([28.565, -13.02, 28.605, -12.98]) //actually mishishi
Map.centerObject(eldoret, 14);

//FUNCTIONS from https://gis.stackexchange.com/questions/277059/cloud-mask-for-landsat8-on-google-earth-engine
var cld = require('users/fitoprincipe/geetools:cloud_masks')


//IMAGES FOR THE FIGURE WITH SPARKLINES

//nl 
var nl = ee.ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG") 
                .filterBounds(eldoret)
                .sort('CLOUD_COVER', true);
var nl = nl.first();

//modis
var modis = ee.ImageCollection("MODIS/006/MYD09Q1") 
                .sort('CLOUD_COVER', true);
var modis = modis.first();

//l5
var l5 = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR")
                .filterBounds(eldoret)
                .sort('CLOUD_COVER', true);
var l5 = l5.first();

//s2
var s2 = ee.ImageCollection("COPERNICUS/S2_SR") 
                .filterBounds(eldoret)
                .sort('CLOUDY_PIXEL_PERCENTAGE', true);
var s2 = s2.first();

var vizParams = {bands: ['avg_rad'], min: 0.2, max: 16, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(nl.clip(eldoret), vizParams, 'nl');
Export.image.toDrive({
  image: nl.visualize(vizParams),
  description: 'NL',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['B3', 'B2', 'B1'], min: -300, max: 3500, gamma: 1.3};
//Map.addLayer(l5, vizParams, 'Landsat 8 false color');
Export.image.toDrive({
  image: l5.visualize(vizParams),
  description: 'L5',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['VV', 'VH', 'VV'], min: -30, max: 16};
//Map.addLayer(s1.clip(eldoret), vizParams, "s1");
Export.image.toDrive({
  image: s1.visualize(vizParams),
  description: 'S1',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['B4', 'B3', 'B2'], min: 100, max: 3200};
//Map.addLayer(s2.clip(eldoret), vizParams, "s2");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'S2',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['sur_refl_b01', 'sur_refl_b02'], min: 440, max: 2750};
//Map.addLayer(modis.clip(eldoret), vizParams, "modis");
Export.image.toDrive({
  image: modis.visualize(vizParams),
  description: 'MODIS',
  region: eldoret,
  scale: 250, 
  folder: 'review-paper'
});

var vizParams = {bands: ['B1'], min: 150, max: 1300, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(s2.clip(eldoret), vizParams, "aerosols");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'aerosols',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['B8'], min: 220, max: 6000, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(s2.clip(eldoret), vizParams, "nir");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'nir',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['B9'], min: 1200, max: 3800, palette: ['#01025C', '#F0FF00']};
M//ap.addLayer(s2.clip(eldoret), vizParams, "water vapor");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'vapor',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['B11'], min: 230, max: 6600, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(s2.clip(eldoret), vizParams, "swir");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'swir',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['AOT'], min: 69, max: 74, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(s2.clip(eldoret), vizParams, "aerosol optical thickness");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'AOT',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['WVP'], min: 640, max: 1750, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(s2.clip(eldoret), vizParams, "water vapor pressure");
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'WVP',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


//IMAGES FOR THE INDEX PLOT

//indices
var ndmi =  s2.select('B8').subtract(s2.select('B11')).
            divide(s2.select('B8').add(s2.select('B11'))).rename('NDMI');

var savi =  s2.select('B8').subtract(s2.select('B4')).
            divide((s2.select('B8').add(s2.select('B11')).add(0.5)).multiply(1.5)).rename('SAVI');

var ndbi = s2.select('B11').subtract(s2.select('B8')).
            divide(s2.select('B11').add(s2.select('B8'))).rename('NDBI');
          
var ndvi = s2.normalizedDifference(['B8', 'B4']).rename('NDVI');
            
var bsi = s2.select('B11').add(s2.select('B4'));
var bsi2 = s2.select('B8').add(s2.select('B2'));
var bsi3 = bsi.subtract(bsi2);
bsi2 = bsi.add(bsi2);
bsi = bsi3.divide(bsi2).rename('BSI');

var ndwi =  s2.expression(
            '( (nir - swir) / (nir + swir) )', {
              'nir': s2.select('B8'),
              'swir': s2.select('B12')
          });
ndwi = ndwi.rename('NDWI')

var si =  s2.expression(
            '( (1-b2) * (1-b3) * (1-b4) )', {
              'b2': s2.select('B2'),
              'b3': s2.select('B3'),
              'b4': s2.select('B4')
          });
si = si.cbrt().rename('SI')

var avi =  s2.expression(
            '( (b8 * (1 - b4)) * (b8 - b4) )', {
              'b8': s2.select('B8'),
              'b4': s2.select('B4')
          });
avi = avi.cbrt().rename('AVI')

var nddi = modis.normalizedDifference(['sur_refl_b01', 'sur_refl_b02'])

var vizParams = {bands: ['NDWI'], min: -.25, max: .6, palette: ['#01025C', '#F0FF00']};
Export.image.toDrive({
  image: ndwi.visualize(vizParams),
  description: 'NDWI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

var vizParams = {bands: ['SAVI'], min: -0.22, max: 0.5, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(savi.clip(eldoret), vizParams, "savi");
Export.image.toDrive({
  image: savi.visualize(vizParams),
  description: 'SAVI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


var vizParams = {bands: ['NDMI'], min: -0.5, max: 0.5, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(ndmi.clip(eldoret), vizParams, "ndmi");
Export.image.toDrive({
  image: ndmi.visualize(vizParams),
  description: 'NDMI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


var vizParams = {bands: ['NDBI'], min: -0.3, max: 0.25, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(ndbi.clip(eldoret), vizParams, "ndbi");
Export.image.toDrive({
  image: ndbi.visualize(vizParams),
  description: 'NDBI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


var vizParams = {bands: ['BSI'], min: -0.44, max: 0.33, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(bsi.clip(eldoret), vizParams, "bsi");
Export.image.toDrive({
  image: bsi.visualize(vizParams),
  description: 'BSI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


var vizParams = {bands: ['NDVI'], min: .05, max: 0.81, palette: ['#01025C', '#F0FF00']};
//Map.addLayer(ndvi.clip(eldoret), vizParams, "ndvi");
Export.image.toDrive({
  image: ndvi.visualize(vizParams),
  description: 'NDVI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


var vizParams = {bands: ['SI'], min: -1006, max: -105, palette: ['#01025C', '#F0FF00']};
Export.image.toDrive({
  image: si.visualize(vizParams),
  description: 'SI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


var vizParams = {bands: ['nd'], min: -0.55, max: -0.18, palette: ['#01025C', '#F0FF00']};
Export.image.toDrive({
  image: nddi.visualize(vizParams),
  description: 'NDDI',
  region: eldoret,
  scale: 250, //this is from modis
  folder: 'review-paper'
});


var vizParams = {bands: ['B1'], min: 135, max: 1430, palette: ['#01025C', '#F0FF00']};
Export.image.toDrive({
  image: s2.visualize(vizParams),
  description: 'aerosol',
  region: eldoret,
  scale: 60, //this band is coarser 
  folder: 'review-paper'
});

var vizParams = {bands: ['AVI'], min: -2623, max: 1001, palette: ['#01025C', '#F0FF00']};
Export.image.toDrive({
  image: avi.visualize(vizParams),
  description: 'AVI',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


//IMAGES FOR THE OVER TIME NDVI PLOT

//mask clouds
var s2 = ee.ImageCollection("COPERNICUS/S2_SR") 
                .filterBounds(eldoret);
var cloudBitMask = ee.Number(2).pow(10).int();
var cirrusBitMask = ee.Number(2).pow(11).int();
var qa = s2.select('QA60');
function maskS2clouds(image) {
  var qa = image.select('QA60');
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
             qa.bitwiseAnd(cirrusBitMask).eq(0));
  return image.updateMask(mask);
}

//read in Sentinel 2 and mask it
var s2 = s2.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 70))
                .map(maskS2clouds);
var vizParams = {bands: ['NDVI'], min: .05, max: 0.81, palette: ['#01025C', '#F0FF00']};

//indices in November
var ndvi = s2.filterDate('2019-11-01', '2019-11-30').median()
             .normalizedDifference(['B8', 'B4']).rename('NDVI');

Map.addLayer(ndvi.clip(eldoret), vizParams, "NDVI-Nov");
Export.image.toDrive({
  image: ndvi.visualize(vizParams),
  description: 'NDVI-Nov',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

//indices in December
var ndvi = s2.filterDate('2018-11-01', '2018-11-30').median()
             .normalizedDifference(['B8', 'B4']).rename('NDVI');

Map.addLayer(ndvi.clip(eldoret), vizParams, "NDVI-Nov");
Export.image.toDrive({
  image: ndvi.visualize(vizParams),
  description: 'NDVI-Nov',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

//indices in March
var ndvi = s2.filterDate('2019-02-20', '2019-03-30').median()
             .normalizedDifference(['B8', 'B4']).rename('NDVI');
Map.addLayer(ndvi.clip(eldoret), vizParams, "NDVI-Mar");
Export.image.toDrive({
  image: ndvi.visualize(vizParams),
  description: 'NDVI-Mar',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});


//indices in May
var ndvi = s2.filterDate('2019-05-01', '2019-05-30').median()
             .normalizedDifference(['B8', 'B4']).rename('NDVI');

Map.addLayer(ndvi.clip(eldoret), vizParams, "NDVI-May");
Export.image.toDrive({
  image: ndvi.visualize(vizParams),
  description: 'NDVI-May',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});

//indices in June
var ndvi = s2.filterDate('2019-09-01', '2019-09-30').median()
             .normalizedDifference(['B8', 'B4']).rename('NDVI');

Map.addLayer(ndvi.clip(eldoret), vizParams, "NDVI-Sept");
Export.image.toDrive({
  image: ndvi.visualize(vizParams),
  description: 'NDVI-Sept',
  region: eldoret,
  scale: 10, 
  folder: 'review-paper'
});



//testing area
print(ndvi.clip(eldoret).reduceRegion(ee.Reducer.max(), eldoret));
//print(nl.reduceRegion(ee.Reducer.mean(), eldoret));
print(ndvi.clip(eldoret).reduceRegion(ee.Reducer.min(), eldoret));
//print(s2)