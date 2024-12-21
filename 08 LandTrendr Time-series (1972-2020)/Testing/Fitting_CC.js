var cc = ee.Image("users/mitchbon/cc_72to20");

// Fitting CC time-series

// CC time-series info and mapping
var cc = cc.rename(['1972', '1974', '1975', '1976', '1977', '1978', '1979',
              '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989',
              '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
              '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
              '2020'])
print(cc, 'cc time-series');
Map.addLayer(cc, {bands:['1972', '1996', '2020'], min: 0, max: 1}, "canopy cover");

// set LT params - using same as run for TC indices (test different?)
var runParams = { 
  maxSegments:            8, 
  spikeThreshold:         0.75, 
  vertexCountOvershoot:   0, 
  preventOneYearRecovery: false, 
  recoveryThreshold:      0.25, //0.25
  pvalThreshold:          0.05, 
  bestModelProportion:    0.75, 
  minObservationsNeeded:  6
};

// Ensure that index is to be segmented is oriented so that vegetation loss is a positive delta
var cc = cc.multiply(-1) // Very important (otherwise Landtrendr does not ID change well)

// make a list of bands
var cc_bands = []; // holder for band position and year
var cc_years = []; // holder for year label - used when flattening FTV
var i = -1;
for(var y=1972;y<=2020;y++){
  if(y != 1973){
    i++;
    cc_bands.push([i,y]);
    cc_years.push(y.toString());
  }
}

// cast bands as ee.List
cc_bands = ee.List(cc_bands);

// extract each band as an image in a list
var cc_imgList = cc_bands.map(function(cc_i){
  cc_i = ee.List(cc_i); // make i a ee.List
  var cc_band = cc.select([cc_i.get(0)]); // extract the band
  var cc_outImage = cc_band.addBands(cc_band.multiply(-1)) // add an inverted copy of the band for FTV
                     .rename(['cc_lt', 'cc']) // name the bands
                     .set('system:time_start', ee.Date.fromYMD(cc_i.get(1), 7, 1).millis()); // set the year - default date to july 1st
  return cc_outImage;
});

// convert the image list to a collection
var cc_col = ee.ImageCollection(cc_imgList);

runParams.timeSeries = cc_col;

// run LT
var cc_LT = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);

// subset the CC FTV data
var cc_FTV = cc_LT.select('cc_fit');

// flatten FTV array into band stack
var cc_FTVstack = cc_FTV.arrayFlatten([cc_years]);  // This is the fitted result we want
print('Stacked CC FTV:', cc_FTVstack);

// map it - see what is looks like as RGB change 
Map.addLayer(cc_FTVstack, {bands:['1972', '1996', '2020'], min:0, max:1}, "cc_fitted");