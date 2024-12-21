var TCA_019030 = ee.Image("users/mitchbon/TCA_72to20_019030"),
    ROP_CRW = ee.FeatureCollection("users/mitchbon/ROP_CRW"),
    TCA_019029 = ee.Image("users/mitchbon/TCA_72to20_019029"),
    TCA_020029 = ee.Image("users/mitchbon/TCA_72to20_020029"),
    TCA_020030 = ee.Image("users/mitchbon/TCA_72to20_020030");

var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js');

// Add Assets
// Define visualization parameters
var vizParams = {
  bands: ['y1972', 'y1996', 'y2020'], // If 1972 missing asset won't visualize
  min: 0,
  max: 4000
};

print(TCA_019030, 'TCA time-series 019030');
//Map.addLayer(TCA_019030, vizParams, 'TCA time-series 019030');

print(TCA_019029, 'TCA time-series 019029');
//Map.addLayer(TCA_019029, vizParams, 'TCA time-series 019029');

print(TCA_020030, 'TCA time-series 020030');
//Map.addLayer(TCA_020030, vizParams, 'TCA time-series 020030');

print(TCA_020029, 'TCA time-series 020029');
//Map.addLayer(TCA_020029, vizParams, 'TCA time-series 020029');

// Add multiband images to ImageCollection
var TCA_col = ee.ImageCollection([TCA_019030, TCA_019029, TCA_020029, TCA_020030]);
//print(TCA_col);

// Reduce to composite with mean (could also consider median, max)
var TCA_comp = TCA_col.mean();
print(TCA_comp, 'TCA composite (1972-2020)');
Map.addLayer(TCA_comp, vizParams, 'TCA composite (1972-2020)');
// Running this in LandTrendr: Tile error: Image '0' does not have a 'system:time_start' property.

// Ensure that index that is to be segmented is oriented so that vegetation loss is a positive delta
var TCA_comp1 = TCA_comp.multiply(-1) // VERY IMPORTANT (OTHERWISE LANDTRENDR DOES NOT ID CHANGE WELL)
//Map.addLayer(TCA_comp1, {}, 'TCA composite -1 (1972-2020)');

//Export.image.toDrive({
//  image: TCA_comp,
//  description: 'TCA_72to20_median',
//  scale: 30,
//  crs: 'EPSG:32617',
//  region: ROP_CRW,
//}) // May need to shift

// Make a list of bands
var bands = []; // holder for band position and year
var years = []; // holder for year label - used when flattening FTV
var i = -1;
for(var y=1972;y<=2020;y++){
    i++;
    bands.push([i,y]);
    years.push(y.toString());
}
//print(bands); // Starts at 0
//print(years); // Starts at 0

// Cast bands as ee.List
bands = ee.List(bands);
print('bands linked with years:', bands);

// Extract each band as an image in a list
var imgList = bands.map(function(i){
  i = ee.List(i); // make i a ee.List
  var band = TCA_comp1.select([i.get(0)]); // Extract the band
  var outImage = band.addBands(band.multiply(-1)) // Add an inverted copy of the band for FTV
                     .rename(['tca_lt', 'tca']) // Name the bands
                     .set('system:time_start', ee.Date.fromYMD(i.get(1), 7, 1).millis()); // set the year - default date to july 1st
  return outImage;
});
//print(imgList);

// convert the image list to a collection
var col = ee.ImageCollection(imgList);
print('Image list to col:', col);

// LandTrendr
// set LT params - could play with these, but this set is generically good
var runParams = { 
  maxSegments:            8, //6
  spikeThreshold:         0.9,
  vertexCountOvershoot:   3,
  preventOneYearRecovery: true, //false
  recoveryThreshold:      0.25,
  pvalThreshold:          0.05, //0.1
  bestModelProportion:    0.75, //1.25
  minObservationsNeeded:  6
};
runParams.timeSeries = col;

// Run LT
var LTresult = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);
print('LTresult:', LTresult);
//Map.addLayer(LTresult); 

// Subset the TCA FTV data
var tcaFTV = LTresult.select('tca_fit');
print('TCA FTV:', tcaFTV);

// Subset other two, to look like default?
//var LTtest = LTresult.select('LandTrendr', 'rmse')
//print('LTtest:', LTtest);

// Flatten FTV array into band stack
var tcaFTVstack = tcaFTV.arrayFlatten([years]);  // This is the fitted result we want
print('Stacked TCA FTV:', tcaFTVstack);
Map.addLayer(tcaFTVstack, {bands:['1972', '1996', '2020'], min:0, max:4000}, 
'TCA fitted (1972-2020)');

//Export.image.toDrive({
//  image: tcaFTVstack,
//  description: 'TCA_72to20_fitted',
//  scale: 30,
//  crs: 'EPSG:32617',
//  region: ROP_CRW,
//})

//ROP_CRW Boundary
var empty = ee.Image().byte(); //Create empty image
var outline = empty.paint({
  featureCollection: ROP_CRW,
  color: 1,
  width: 3
});
Map.addLayer(outline, {palette: 'FF0000'}, 'ROP_CRW');
//Map.addLayer(ROP_CRW, {}, 'ROP_CRW');

// define change parameters
//var changeParams = { // May update if needed in publication later...
//  delta:  'loss',
//  sort:   'greatest',
//  year:   {checked:false, start:2000, end:2010},
//  mag:    {checked:true,  value:500,  operator: '>', dsnr:false},
//  dur:    {checked:true,  value:4,    operator: '<'},
//  preval: {checked:false,  value:300,  operator: '>'},
//  mmu:    {checked:true,  value:11},
//};

//////// Mapping ////////
//getChangeMap
//var biggestloss = ltgee.getChangeMap(tcaFTVstack, changeParams);
//print('biggestloss', biggestloss)

//var changeImg = ltgee.getChangeMap(lt2, changeParams);

//var palette = ['#9400D3', '#4B0082', '#0000FF', '#00FF00', '#FFFF00', '#FF7F00', '#FF0000'];

//var yodVizParams = {
//  min: startYear,
//  max: endYear,
//  palette: palette
//};

//var magVizParams = {
//  min: 200,
//  max: 1000,
//  palette: palette
//};

//Map.addLayer(changeImg.select(['mag']), magVizParams, 'Magnitude of Change'); //Export
//Map.addLayer(changeImg.select(['yod']), yodVizParams, 'Year of Detection'); //Export

//LT-GEE Guide (Working with Outputs)
var lt = LTresult.select('LandTrendr');
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
  description: 'DevelopmentFinder1',
  //scale: 30,
  crs: 'EPSG:32617',
  crsTransform: [30,0,550000,0,-30,4885000],
  region: ROP_CRW
});


// Random points > build and record fitted values > compare with DA construction > pick best parameters/method