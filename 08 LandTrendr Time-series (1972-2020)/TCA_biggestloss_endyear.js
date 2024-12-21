var tca = ee.Image("users/mitchbon/TCA_72to20_R1"),
    ROP_CRW = ee.FeatureCollection("users/mitchbon/ROP_CRW");

//Find the biggest loss end year for a LandTrendr TCA time-series

// set LT params
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


// Create Biggest Loss End Year Data
//LT-GEE Guide (Working with Outputs)
var lt = tca_LT.select('LandTrendr');
//print(lt);

var vertexMask = lt.arraySlice(0,3,4); // slice out Vertex row
var vertices = lt.arrayMask(vertexMask);
//print(vertices);
//Map.addLayer(vertices);

var left = vertices.arraySlice(1, 0, -1);
//Map.addLayer(left, {}, 'left');
var right = vertices.arraySlice(1, 1, null);
//Map.addLayer(right, {}, 'right');
var startYear = left.arraySlice(0, 0, 1);
//Map.addLayer(startYear, {}, 'startYear');
var startVal = left.arraySlice(0, 2, 3);
//Map.addLayer(startVal, {}, 'startVal');
var endYear = right.arraySlice(0, 0, 1);
//Map.addLayer(startVal, {}, 'startVal');
var endVal = right.arraySlice(0, 2, 3);
//Map.addLayer(endVal, {}, 'endVal');

var dur = endYear.subtract(startYear); // Right now opposite (may try multiply by -1)
//Map.addLayer(dur, {}, 'dur')
var mag = endVal.subtract(startVal);
//Map.addLayer(mag, {}, 'mag')
var rate = mag.divide(dur);
//Map.addLayer(rate, {}, 'rate');

var segInfo = ee.Image.cat([startYear.add(1), endYear, startVal, endVal, mag, dur, rate])
                      .toArray(0)
                      .mask(vertexMask.mask())
                      

var sortByThis = segInfo.arraySlice(0, 4, 5).toArray(0).multiply(-1);
var segInfoSorted = segInfo.arraySort(sortByThis);
var bigDelta = segInfoSorted.arraySlice(1, 0, 1);
//Map.addLayer(bigDelta, {}, 'bigDelta'); // Segment with most TCA loss

var bigDeltaImg = ee.Image.cat(bigDelta.arraySlice(0,0,1).arrayProject([1]).arrayFlatten([['yod']]),
                               bigDelta.arraySlice(0,1,2).arrayProject([1]).arrayFlatten([['endYr']]),
                               bigDelta.arraySlice(0,2,3).arrayProject([1]).arrayFlatten([['startVal']]).multiply(-1),
                               bigDelta.arraySlice(0,3,4).arrayProject([1]).arrayFlatten([['endVal']]).multiply(-1),
                               bigDelta.arraySlice(0,4,5).arrayProject([1]).arrayFlatten([['mag']]).multiply(-1),
                               bigDelta.arraySlice(0,5,6).arrayProject([1]).arrayFlatten([['dur']]),
                               bigDelta.arraySlice(0,6,7).arrayProject([1]).arrayFlatten([['rate']]).multiply(-1));
//Map.addLayer(bigDeltaImg, {}, 'bigDeltaImg');

var distMask = bigDeltaImg.select(['mag']).lt(-1000) // Loss of more than 1000
                                          .and(bigDeltaImg.select(['dur']).lt(4)); // Over less than 4 years
//Map.addLayer(distMask, {}, 'diskMask');

var bigFastDist = bigDeltaImg.mask(distMask).int16();
Map.addLayer(bigFastDist, {}, 'bigFastDist');
// Don't do MMU for now

// Export
Export.image.toDrive({
  image: bigFastDist,
  description: 'TCA_BigLoss_EndYr_1000_4',
  //scale: 30,
  crs: 'EPSG:32617',
  crsTransform: [30,0,550000,0,-30,4885000],
  region: ROP_CRW
});