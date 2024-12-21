// follow along in https://emapr.github.io/LT-GEE/api.html#buildsrcollection

// load the LandTrendr.js module
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js');

// define parameters
var startYear = 1985;
var endYear = 2017;
var startDay = '06-20';
var endDay = '09-20';
var aoi = ee.Geometry.Point(-79.966, 43.743);
var maskThese = ['cloud', 'shadow', 'snow']
var bandList = ['TCA']
var index = 'TCA'
var ftvList = ['TCA']

var runParams = { 
  maxSegments:            6,
  spikeThreshold:         0.9,
  vertexCountOvershoot:   3,
  preventOneYearRecovery: true,
  recoveryThreshold:      0.25,
  pvalThreshold:          0.05,
  bestModelProportion:    0.75,
  minObservationsNeeded:  6
};

// define change parameters
var changeParams = {
  delta:  'loss',
  sort:   'greatest',
  year:   {checked:false, start:2000, end:2010},
  mag:    {checked:true,  value:200,  operator: '>', dsnr:false},
  dur:    {checked:true,  value:4,    operator: '<'},
  preval: {checked:true,  value:300,  operator: '>'},
  mmu:    {checked:true,  value:11},
};

// center and zoom the display in case outputs are to be mapped 
Map.centerObject(aoi,10);
Map.addLayer(aoi);

// apply LandTrendr.js functions

// build SRcollection
var annualSRcollection = ltgee.buildSRcollection(startYear, endYear, startDay, endDay, aoi, maskThese);
print(annualSRcollection); //1

// buildClearPixelCountCollection
var nClearCollection = ltgee.buildClearPixelCountCollection(startYear, endYear, startDay, endDay, aoi, maskThese);
print(nClearCollection); //2

// transformSRcollection
var indexCollection = ltgee.transformSRcollection(annualSRcollection, bandList);
var year2000 = ee.Image(indexCollection.filterDate('2000-01-01','2000-12-31').first());
print(year2000); //3

// buildLTcollection
var annualLTcollection = ltgee.buildLTcollection(annualSRcollection, index, ftvList);
print(annualLTcollection) //4

// collectionToBandStack
var collectionBandStack = ltgee.collectionToBandStack(indexCollection, startYear, endYear);
print(collectionBandStack); //5
//Map.addLayer(collectionBandStack, {"bands":["2000"],"min":-1000,"max":5000,"palette":["ff2f0d","fff825","0ab308"]});

// runLT
var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, ftvList, runParams, maskThese);
print(lt) //NOTE: id - ftv_tca_fit!! //6

// getSegmentData
var segInfo = ltgee.getSegmentData(lt, index, 'all');
print(segInfo); //7
//Map.addLayer(segInfo); // use 'Inspector' to explore values

//getFittedData
var tcaFTV = ltgee.getFittedData(lt, startYear, endYear, ftvList[0]);
print(tcaFTV) //8

//getChangeMap
changeParams.index = index;

var changeImg = ltgee.getChangeMap(lt, changeParams);

var palette = ['#9400D3', '#4B0082', '#0000FF', '#00FF00', '#FFFF00', '#FF7F00', '#FF0000'];
var yodVizParms = {
  min: startYear,
  max: endYear,
  palette: palette
};

var magVizParms = {
  min: 200,
  max: 1600,
  palette: palette
};

Map.centerObject(aoi, 11);
Map.addLayer(changeImg.select(['mag']), magVizParms, 'Magnitude of Change');
Map.addLayer(changeImg.select(['yod']), yodVizParms, 'Year of Detection');

// getSegmentCount
var segCount = ltgee.getSegmentCount(segInfo);
//Map.addLayer(segCount, {"min":0, "max":6});
