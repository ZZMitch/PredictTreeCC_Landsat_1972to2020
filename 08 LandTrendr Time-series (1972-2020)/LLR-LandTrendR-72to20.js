var studyarea = ee.FeatureCollection("users/mitchbon/ROP_CRW"),
    tca = ee.Image("users/mitchbon/TCA_72to20_R1"),
    tcb = ee.Image("users/mitchbon/TCB_72to20_R"),
    tcg = ee.Image("users/mitchbon/TCG_72to20_R"),
    tcw = ee.Image("users/mitchbon/TCW_72to20_R"),
    studyarea1 = ee.Image("users/mitchbon/CVW_ROP_usearea"),
    cc = ee.Image("users/mitchbon/cc_72to20"),
    ROP = ee.FeatureCollection("users/mitchbon/ROP"),
    cc_res = ee.Image("users/mitchbon/cc_72to20_res1"),
    tca_fit = ee.Image("users/mitchbon/TCA_72to20_fitted"),
    da = ee.FeatureCollection("users/mitchbon/DA2016_Peel_1600"),
    tca_fit_res = ee.Image("users/mitchbon/TCA_72to20_fitted_res");

// Created with Justin Braaten //

// set LT params - test vs. 100 pixel random sample (try different combinations for best score)
var runParams = {
  maxSegments:            8, 
  spikeThreshold:         0.75, 
  vertexCountOvershoot:   0, 
  preventOneYearRecovery: false, 
  recoveryThreshold:      0.25, 
  pvalThreshold:          0.05, 
  bestModelProportion:    0.75, 
  minObservationsNeeded:  6
};

//////////////////////////////
//////////// TCA /////////////
/////////////////////////////
// load LandsatLinkr median timeseries 
var tca = ee.Image(tca);
//print('Projection of input ts, crs, and trans:', tca.projection()); // Confirm EPSG:32617
//print('Scale in meters:', tca.projection().nominalScale()); // Confirm 30 m

// Change band names to represent years
var tca = tca.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])

// check out RGB through time
print(tca, 'tca_median');

// Mask value 0 (missing data)
//var tca = tca.mask(0); // masks everything...
//var tca_mask = tca.eq(0); // Creates new raster with 1 when TCA is 0
//Map.addLayer(tca_mask)

Map.addLayer(tca, {bands:['1972','1996','2020'], min:0, max: 4000}, "tca_median");

// Ensure that index is to be segmented is oriented so that vegetation loss is a positive delta
var tca = tca.multiply(-1) // Very important (otherwise Landtrendr does not ID change well)

// make a list of bands
var tca_bands = []; // holder for band position and year
var tca_years = []; // holder for year label - used when flattening FTV
var i = -1;
for(var y=1972;y<=2020;y++){
  if(y != 1973){
    i++;
    tca_bands.push([i,y]);
    tca_years.push(y.toString());
  }
}
//print(tca_bands, 'bands');
//print(tca_years, 'years');

// cast bands as ee.List
tca_bands = ee.List(tca_bands);
//print('bands linked with years:', tca_bands);

// extract each band as an image in a list
var tca_imgList = tca_bands.map(function(tca_i){
  tca_i = ee.List(tca_i); // make i a ee.List
  var tca_band = tca.select([tca_i.get(0)]); // extract the band
  var tca_outImage = tca_band.addBands(tca_band.multiply(-1)) // add an inverted copy of the band for FTV
                     .rename(['tca_lt', 'tca']) // name the bands
                     .set('system:time_start', ee.Date.fromYMD(tca_i.get(1), 7, 1).millis()); // set the year - default date to july 1st
  return tca_outImage;
});

// convert the image list to a collection
var tca_col = ee.ImageCollection(tca_imgList);
//print('Image list to col:', tca_col);

//You might also try resampling by 'bicubic' before running LandTrendr to help reduce spatial "speckle"
//var resampled = tca.resample('bicubic');
//Map.addLayer(resampled, {bands:['b1','b24','b48'], min:0, max: 4000}, 'tca_median_bicubic');
runParams.timeSeries = tca_col;

// run LT
var tca_LT = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);
//print('tca_LT', tca_LT);
// Converts from UTM Zone 17N to WGS 84 at this point...

// subset the TCA FTV data
var tca_FTV = tca_LT.select('tca_fit');
//print('TCA FTV:', tca_FTV);

// flatten FTV array into band stack
var tca_FTVstack = tca_FTV.arrayFlatten([tca_years]);  // This is the fitted result we want
print('Stacked TCA FTV:', tca_FTVstack);

// Check projection and scale
//print('Projection of fitted tca, crs, and trans:', tca_FTVstack.projection()); // EPSG: 4326
//print('Scale in meters:', tca_FTVstack.projection().nominalScale()); // 111319.5

// map it - see what is looks like as RGB change 
Map.addLayer(tca_FTVstack, {bands:['1972', '1996', '2020'], min:0, max:4000}, "tca_fitted");

//////////////////////////////
//////////// TCB /////////////
/////////////////////////////
// load LandsatLinkr median timeseries 
var tcb = ee.Image(tcb);
var tcb = tcb.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
print(tcb, 'tcb_median');
Map.addLayer(tcb, {bands:['1972','1996','2020'], min: 0, max: 5000}, "tcb_median");

// Ensure that index is to be segmented is oriented so that vegetation loss is a positive delta
var tcb = tcb.multiply(1) // Very important (otherwise Landtrendr does not ID change well)
// NOTE: TCB only one that is 1, all others should be -1

// make a list of bands
var tcb_bands = []; // holder for band position and year
var tcb_years = []; // holder for year label - used when flattening FTV
var i = -1;
for(var y=1972;y<=2020;y++){
  if(y != 1973){
    i++;
    tcb_bands.push([i,y]);
    tcb_years.push(y.toString());
  }
}

// cast bands as ee.List
tcb_bands = ee.List(tcb_bands);

// extract each band as an image in a list
var tcb_imgList = tcb_bands.map(function(tcb_i){
  tcb_i = ee.List(tcb_i); // make i a ee.List
  var tcb_band = tcb.select([tcb_i.get(0)]); // extract the band
  var tcb_outImage = tcb_band.addBands(tcb_band.multiply(1)) // add an inverted copy of the band for FTV
                     .rename(['tcb_lt', 'tcb']) // name the bands
                     .set('system:time_start', ee.Date.fromYMD(tcb_i.get(1), 7, 1).millis()); // set the year - default date to july 1st
  return tcb_outImage;
});

// convert the image list to a collection
var tcb_col = ee.ImageCollection(tcb_imgList);

// set LT params - test vs. 100 pixel random sample (try different combinations for best score)
runParams.timeSeries = tcb_col;

// run LT
var tcb_LT = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);

// subset the TCB FTV data
var tcb_FTV = tcb_LT.select('tcb_fit');

// flatten FTV array into band stack
var tcb_FTVstack = tcb_FTV.arrayFlatten([tcb_years]);  // This is the fitted result we want
print('Stacked TCB FTV:', tcb_FTVstack);

// map it - see what is looks like as RGB change 
Map.addLayer(tcb_FTVstack, {bands:['1972', '1996', '2020'], min: 0, max: 5000}, "tcb_fitted");

//////////////////////////////
//////////// TCG /////////////
/////////////////////////////
// load LandsatLinkr median timeseries 
var tcg = ee.Image(tcg);
var tcg = tcg.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
print(tcg, 'tcg_median');
Map.addLayer(tcg, {bands:['1972','1996','2020'], min:0, max: 4000}, "tcg_median");

// Ensure that index is to be segmented is oriented so that vegetation loss is a positive delta
var tcg = tcg.multiply(-1) // Very important (otherwise Landtrendr does not ID change well)

// make a list of bands
var tcg_bands = []; // holder for band position and year
var tcg_years = []; // holder for year label - used when flattening FTV
var i = -1;
for(var y=1972;y<=2020;y++){
  if(y != 1973){
    i++;
    tcg_bands.push([i,y]);
    tcg_years.push(y.toString());
  }
}

// cast bands as ee.List
tcg_bands = ee.List(tcg_bands);

// extract each band as an image in a list
var tcg_imgList = tcg_bands.map(function(tcg_i){
  tcg_i = ee.List(tcg_i); // make i a ee.List
  var tcg_band = tcg.select([tcg_i.get(0)]); // extract the band
  var tcg_outImage = tcg_band.addBands(tcg_band.multiply(-1)) // add an inverted copy of the band for FTV
                     .rename(['tcg_lt', 'tcg']) // name the bands
                     .set('system:time_start', ee.Date.fromYMD(tcg_i.get(1), 7, 1).millis()); // set the year - default date to july 1st
  return tcg_outImage;
});

// convert the image list to a collection
var tcg_col = ee.ImageCollection(tcg_imgList);

// set LT params - test vs. 100 pixel random sample (try different combinations for best score)
runParams.timeSeries = tcg_col;

// run LT
var tcg_LT = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);

// subset the TCG FTV data
var tcg_FTV = tcg_LT.select('tcg_fit');

// flatten FTV array into band stack
var tcg_FTVstack = tcg_FTV.arrayFlatten([tcg_years]);  // This is the fitted result we want
print('Stacked TCG FTV:', tcg_FTVstack);

// map it - see what is looks like as RGB change 
Map.addLayer(tcg_FTVstack, {bands:['1972', '1996', '2020'], min:0, max:4000}, "tcg_fitted");

//////////////////////////////
//////////// TCW /////////////
/////////////////////////////
// load LandsatLinkr median timeseries 
var tcw = ee.Image(tcw);
var tcw = tcw.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
print(tcw, 'tcw_median');
Map.addLayer(tcw, {bands:['1972','1996','2020'], min:-2000, max: 0}, "tcw_median");

// Ensure that index is to be segmented is oriented so that vegetation loss is a positive delta
var tcw = tcw.multiply(-1) // Very important (otherwise Landtrendr does not ID change well)

// make a list of bands
var tcw_bands = []; // holder for band position and year
var tcw_years = []; // holder for year label - used when flattening FTV
var i = -1;
for(var y=1972;y<=2020;y++){
  if(y != 1973){
    i++;
    tcw_bands.push([i,y]);
    tcw_years.push(y.toString());
  }
}

// cast bands as ee.List
tcw_bands = ee.List(tcw_bands);

// extract each band as an image in a list
var tcw_imgList = tcw_bands.map(function(tcw_i){
  tcw_i = ee.List(tcw_i); // make i a ee.List
  var tcw_band = tcw.select([tcw_i.get(0)]); // extract the band
  var tcw_outImage = tcw_band.addBands(tcw_band.multiply(-1)) // add an inverted copy of the band for FTV
                     .rename(['tcw_lt', 'tcw']) // name the bands
                     .set('system:time_start', ee.Date.fromYMD(tcw_i.get(1), 7, 1).millis()); // set the year - default date to july 1st
  return tcw_outImage;
});

// convert the image list to a collection
var tcw_col = ee.ImageCollection(tcw_imgList);

// set LT params - test vs. 100 pixel random sample (try different combinations for best score)
runParams.timeSeries = tcw_col;

// run LT
var tcw_LT = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);

// subset the TCG FTV data
var tcw_FTV = tcw_LT.select('tcw_fit');

// flatten FTV array into band stack
var tcw_FTVstack = tcw_FTV.arrayFlatten([tcw_years]);  // This is the fitted result we want
print('Stacked TCW FTV:', tcw_FTVstack);

// map it - see what is looks like as RGB change 
Map.addLayer(tcw_FTVstack, {bands:['1972', '1996', '2020'], min:-2000, max:0}, "tcw_fitted");

//ROP_CRW Boundary
var empty = ee.Image().byte(); //Create empty image
var outline = empty.paint({
  featureCollection: studyarea,
  color: 1,
  width: 3
});
Map.addLayer(outline, {palette: '#000000'}, 'ROP_CRW');
//print('ROP_CRW', studyarea)

// Generate 100 random points to test parameters
//var pts = ee.FeatureCollection.randomPoints(studyarea, 100, 0, 0);
//print(pts, "100 random points");
//Map.addLayer(pts, {}, "100 random points");
//Export.table.toDrive(pts, "100randompts")

//var TCA_med_tbl = tca.reduceRegions(pts, ee.Reducer.first(), 30);
//Export.table.toDrive(TCA_med_tbl, "TCA_med_tbl"); // Muliply by -1
//var TCA_fit_tbl = pts.map(function(feature) {
//  return feature.set(tca_FTVstack.reduceRegion({
//    reducer: 'first',
//    geometry: feature.geometry(),
//    crs: "EPSG:32617",
//    crsTransform: [30,0,550000,0,-30,4885000] // This is the key, taken from input WGS1984 UTM17N images
//  }))
//});
//Export.table.toDrive(TCA_fit_tbl, "TCA_fit_tbl"); 

//var TCB_med_tbl = tcb.reduceRegions(pts, ee.Reducer.first(), 30);
//Export.table.toDrive(TCB_med_tbl, "TCB_med_tbl");
//var TCB_fit_tbl = pts.map(function(feature) {
//  return feature.set(tcb_FTVstack.reduceRegion({
//    reducer: 'first',
//    geometry: feature.geometry(),
//    crs: "EPSG:32617",
//    crsTransform: [30,0,550000,0,-30,4885000] 
//  }))
//});
//Export.table.toDrive(TCB_fit_tbl, "TCB_fit_tbl"); 

//var TCG_med_tbl = tcg.reduceRegions(pts, ee.Reducer.first(), 30);
//Export.table.toDrive(TCG_med_tbl, "TCG_med_tbl"); // Muliply by -1
//var TCG_fit_tbl = pts.map(function(feature) {
//  return feature.set(tcg_FTVstack.reduceRegion({
//    reducer: 'first',
//    geometry: feature.geometry(),
//    crs: "EPSG:32617",
//    crsTransform: [30,0,550000,0,-30,4885000] 
//  }))
//});
//Export.table.toDrive(TCG_fit_tbl, "TCG_fit_tbl"); 
 
//var TCW_med_tbl = tcw.reduceRegions(pts, ee.Reducer.first(), 30);
//Export.table.toDrive(TCW_med_tbl, "TCW_med_tbl"); // Muliply by -1
//var TCW_fit_tbl = pts.map(function(feature) {
//  return feature.set(tcw_FTVstack.reduceRegion({
//    reducer: 'first',
//    geometry: feature.geometry(),
//    crs: "EPSG:32617",
//    crsTransform: [30,0,550000,0,-30,4885000] 
//  }))
//});
//Export.table.toDrive(TCW_fit_tbl, "TCW_fit_tbl"); 

// Old Export version
//var TCW_fit_tbl = tcw_FTVstack.reduceRegions({
//  collection: pts, 
//  reducer: ee.Reducer.first(),
//  scale: 30, 
//  crs: "EPSG:32617"});
//Export.table.toDrive(TCW_fit_tbl, "TCW_fit_tbl"); // Sometimes takes a while?

////////// Export for conversion to %CC //////////
//Export.image.toDrive({
//image: tca_FTVstack,
//description: 'TCA_72to20_fitted',
//region: studyarea1, 
//crs: 'EPSG: 32617',
//crsTransform: [30,0,550000,0,-30,4885000] 
//});
// Snap to real in ArcGIS (pixels a bit offset)

//Export.image.toDrive({
//image: tcb_FTVstack,
//description: 'TCB_72to20_fitted',
//region: studyarea1, 
//crs: 'EPSG: 32617',
//crsTransform: [30,0,550000,0,-30,4885000] 
//});

//Export.image.toDrive({
//image: tcg_FTVstack,
//description: 'TCG_72to20_fitted',
//region: studyarea1, 
//crs: 'EPSG: 32617',
//crsTransform: [30,0,550000,0,-30,4885000] 
//});

//Export.image.toDrive({
//image: tcw_FTVstack,
//description: 'TCW_72to20_fitted',
//region: studyarea1, 
//crs: 'EPSG: 32617',
//crsTransform: [30,0,550000,0,-30,4885000] 
//});

////////// Examine CC time-series //////////
var cc = cc.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
Map.addLayer(cc, {}, "canopy cover");
print('canopy cover', cc); // Mask out water? 

/// Test: Extract mean CC time-series for whole study area ///
//var mean_all = cc.reduceRegion({
//  reducer: ee.Reducer.mean(),
//  geometry: studyarea,
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000],
//  maxPixels: 200000000 
//})
//print("mean_all", mean_all);
//var feature = ee.Feature(null, mean_all);
//var featureCollection = ee.FeatureCollection([feature]);
//Export.table.toDrive(featureCollection, "mean_all"); 

/// Test: Extract mean CC time-series for each Peel municipalitiy ///
//var empty = ee.Image().byte(); //Create empty image
//var ROP = empty.paint({
//  featureCollection: ROP,
//  color: 1,
//  width: 3
//});
//Map.addLayer(ROP, {palette: '#000000'}, 'ROP');

//var mean_bymun = cc.reduceRegions({
//  collection: ROP,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by mun", mean_bymun);

//Export.table.toDrive({
//  collection: mean_bymun,
//  description: "mean_bymun",
//  selectors: ['NAME', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 

/// Test: Extract mean CC time-series from all Peel DAs ///
//var empty = ee.Image().byte(); //Create empty image
//var da = empty.paint({
//  featureCollection: da,
//  color: 1,
//  width: 3
//});
//Map.addLayer(da, {palette: '#000000'}, 'DA_Peel');

//var mean_byda = cc.reduceRegions({
//  collection: da,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by da", mean_byda);

//Export.table.toDrive({
//  collection: mean_byda,
//  description: "mean_byda",
//  selectors: ['DAUID', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 

////////// Examine CC time-series (residential) //////////
var cc_res = cc_res.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
Map.addLayer(cc_res, {}, "canopy cover (res model)");
print('canopy cover (res model)', cc_res); 

/// Test: Extract mean CC time-series for whole study area ///
//var mean_all = cc_res.reduceRegion({
//  reducer: ee.Reducer.mean(),
//  geometry: studyarea,
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000],
//  maxPixels: 200000000 
//})
//print("mean_all", mean_all);
//var feature = ee.Feature(null, mean_all);
//var featureCollection = ee.FeatureCollection([feature]);
//Export.table.toDrive(featureCollection, "mean_all"); 

/// Test: Extract mean CC time-series for each Peel municipalitiy ///
//var empty = ee.Image().byte(); //Create empty image
//var ROP = empty.paint({
//  featureCollection: ROP,
//  color: 1,
//  width: 3
//});
//Map.addLayer(ROP, {palette: '#000000'}, 'ROP');

//var mean_bymun = cc_res.reduceRegions({
//  collection: ROP,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by mun", mean_bymun);

//Export.table.toDrive({
//  collection: mean_bymun,
//  description: "mean_bymun",
//  selectors: ['NAME', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 

/// Test: Extract mean CC time-series from all Peel DAs ///
//var empty = ee.Image().byte(); //Create empty image
//var da = empty.paint({
//  featureCollection: da,
//  color: 1,
//  width: 3
//});
//Map.addLayer(da, {palette: '#000000'}, 'DA_Peel');
//print('da', da)

//var mean_byda = cc_res.reduceRegions({
//  collection: da,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by da", mean_byda);

//Export.table.toDrive({
//  collection: mean_byda,
//  description: "mean_byda",
//  selectors: ['DAUID', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 

////////// Examine CC time-series (inc. water VHR) //////////
//var cc_incwater = cc_incwater.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
//              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
//              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
//              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
//              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020'])
//Map.addLayer(cc_incwater, {}, "canopy cover (inc water VHR)");
//print('canopy cover (inc water VHR)', cc_incwater); 

/// Test: Extract mean CC time-series for whole study area ///
//var mean_all = cc_incwater.reduceRegion({
//  reducer: ee.Reducer.mean(),
//  geometry: studyarea,
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000],
//  maxPixels: 200000000 
//})
//print("mean_all", mean_all);
//var feature = ee.Feature(null, mean_all);
//var featureCollection = ee.FeatureCollection([feature]);
//Export.table.toDrive(featureCollection, "mean_all"); 

/// Test: Extract mean CC time-series for each Peel municipalitiy ///
//var empty = ee.Image().byte(); //Create empty image
//var ROP = empty.paint({
//  featureCollection: ROP,
//  color: 1,
//  width: 3
//});
//Map.addLayer(ROP, {palette: '#000000'}, 'ROP');

//var mean_bymun = cc_incwater.reduceRegions({
//  collection: ROP,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by mun", mean_bymun);

//Export.table.toDrive({
//  collection: mean_bymun,
//  description: "mean_bymun",
//  selectors: ['NAME', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 

/// Test: Extract mean CC time-series from all Peel DAs ///
//var empty = ee.Image().byte(); //Create empty image
//var da = empty.paint({
//  featureCollection: da,
//  color: 1,
//  width: 3
//});
//Map.addLayer(da, {palette: '#000000'}, 'DA_Peel');

//var mean_byda = cc_incwater.reduceRegions({
//  collection: da,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by da", mean_byda);

//Export.table.toDrive({
//  collection: mean_byda,
//  description: "mean_byda",
//  selectors: ['DAUID', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 

///// Extract fitted TCA time-series for each DA /////
///// Clip to res extent - done in ArcGIS /////
var tca_fit_res = tca_fit_res.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
              
Map.addLayer(tca_fit_res, {}, "TCA fitted (res only)");

//var mean_byda = tca_fit_res.reduceRegions({
//  collection: da,
//  reducer: ee.Reducer.mean(),
//  crs: 'EPSG: 32617',
//  crsTransform: [30,0,550000,0,-30,4885000]
//});
//print("mean by da", mean_byda);

//Export.table.toDrive({
//  collection: mean_byda,
//  description: "mean_byda",
//  selectors: ['DAUID', '1972', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', 
//              '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', 
//              '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', 
//              '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
//              '2020']
//}); 
