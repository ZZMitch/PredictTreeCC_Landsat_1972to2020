//######################################################################################################## 
//#                                                                                                    #\\
//#                           LANDTRENDR GREATEST DISTURBANCE MAPPING                                  #\\
//#                                                                                                    #\\
//########################################################################################################


// date: 2018-10-07
// author: Justin Braaten | jstnbraaten@gmail.com
//         Zhiqiang Yang  | zhiqiang.yang@oregonstate.edu
//         Robert Kennedy | rkennedy@coas.oregonstate.edu
// parameter definitions: https://emapr.github.io/LT-GEE/api.html#getchangemap
// website: https://github.com/eMapR/LT-GEE
// notes: 
//   - you must add the LT-GEE API to your GEE account to run this script. 
//     Visit this URL to add it:
//     https://code.earthengine.google.com/?accept_repo=users/emaprlab/public
//   - use this app to help parameterize: 
//     https://emaprlab.users.earthengine.app/view/lt-gee-change-mapper


//##########################################################################################
// START INPUTS
//##########################################################################################

// define collection parameters
var startYear = 1985;
var endYear = 2017;
var startDay = '06-20';
var endDay = '09-20';
var aoi = ee.Geometry.Point(-122.8848, 43.7929);
var index = 'NBR';
var maskThese = ['cloud', 'shadow', 'snow', 'water'];

// define landtrendr parameters
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
  year:   {checked:true, start:1986, end:2017},
  mag:    {checked:true, value:200,  operator:'>'},
  dur:    {checked:true, value:4,    operator:'<'},
  preval: {checked:true, value:300,  operator:'>'},
  mmu:    {checked:true, value:11},
};

//##########################################################################################
// END INPUTS
//##########################################################################################

// load the LandTrendr.js module
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js'); 

// add index to changeParams object
changeParams.index = index;

// run landtrendr
var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, [], runParams, maskThese);


// Map.addLayer(lt)

lt = lt.select('LandTrendr');
var vertexMask = lt.arraySlice(0, 3, 4); // slice out the 'Is Vertex' row - yes(1)/no(0)
var vertices = lt.arrayMask(vertexMask); // use the 'Is Vertex' row as a mask for all rows

var left = vertices.arraySlice(1, 0, -1);    // slice out the vertices as the start of segments
var right = vertices.arraySlice(1, 1, null); // slice out the vertices as the end of segments
var startYearV = left.arraySlice(0, 0, 1);    // get year dimension of LT data from the segment start vertices
var startVal = left.arraySlice(0, 2, 3);     // get spectral index dimension of LT data from the segment start vertices
var endYearV = right.arraySlice(0, 0, 1);     // get year dimension of LT data from the segment end vertices 
var endVal = right.arraySlice(0, 2, 3);      // get spectral index dimension of LT data from the segment end vertices
var dur = endYearV.subtract(startYearV);       // subtract the segment start year from the segment end year to calculate the duration of segments 
var mag = endVal.subtract(startVal);         // substract the segment start index value from the segment end index value to calculate the delta of segments
var rate = mag.divide(dur);                  // calculate the rate of spectral change

var segInfo = ee.Image.cat([startYearV.add(1), endYearV, startVal, endVal, mag, dur, rate])
                      .toArray(0)
                      .mask(vertexMask.mask());
                      
var sortByThis = segInfo.arraySlice(0,4,5).toArray(0).multiply(-1); // need to flip the delta here, since arraySort is working by ascending order
var segInfoSorted = segInfo.arraySort(sortByThis); // sort the array by magnitude
var bigDelta = segInfoSorted.arraySlice(1, 0, 1); // get the first segment in the sorted array (greatest magnitude vegetation loss segment)


// FIGURE OUT WHICH OBSERVATIONS ARE AFTER GREATEST DISTURBANCE
// get the end year of the greatest dist seg.
var bigDeltaEnd = bigDelta.arraySlice(0, 1, 2);
print(bigDeltaEnd, 'bigDeltaEnd');
Map.addLayer(bigDeltaEnd, {}, 'bigDeltaEnd')

// Use it to make a mask on the obs year
var obsYears = lt.arraySlice(0, 0, 1);
print(obsYears, 'obsYears');
Map.addLayer(obsYears, {}, 'obsYears')
var bigDeltaEndRep = bigDeltaEnd.arrayRepeat(1, endYear - startYear + 1);
print(bigDeltaEndRep, 'bigDeltaEndRep');
Map.addLayer(bigDeltaEndRep, {}, 'bigDeltaEndRep'); // End year of disturbance
var postDistMask = obsYears.gte(bigDeltaEndRep);
print(postDistMask);
Map.addLayer(postDistMask, {}, "postDistMask");

// Get rid of all observations that are less then the start of the segment after the greatest disturbance segment 
var postDistObs = lt.arrayMask(postDistMask);
print(postDistObs, 'postDistObs');
Map.addLayer(postDistObs, {}, "postDistObs");

var postDistObsImg = ee.Image(postDistObs.arraySlice(0,2,3).arrayProject([1]).arrayFlatten([['test']])) //arrayFlatten?
print(postDistObsImg, 'postDistObsImage')
Map.addLayer(postDistObsImg, {}, 'postDistObsImage')

Export.image.toDrive({
  image: postDistObsImg,
  description: 'postDistObsImg',
  scale: 30
})


// Go through the whole process to pull out the segments, but these now all post greatest disturbance.
var vertexMask = postDistObs.arraySlice(0, 3, 4);
print(vertexMask, 'vertexMask');
Map.addLayer(vertexMask, {}, 'vertexMask')

var vertices = lt.arrayMask(vertexMask);

var left = vertices.arraySlice(1, 0, -1);    // slice out the vertices as the start of segments
var right = vertices.arraySlice(1, 1, null); // slice out the vertices as the end of segments
var startYearV = left.arraySlice(0, 0, 1);    // get year dimension of LT data from the segment start vertices
var startVal = left.arraySlice(0, 2, 3);     // get spectral index dimension of LT data from the segment start vertices
var endYearV = right.arraySlice(0, 0, 1);     // get year dimension of LT data from the segment end vertices 
var endVal = right.arraySlice(0, 2, 3);      // get spectral index dimension of LT data from the segment end vertices
var dur = endYearV.subtract(startYearV);       // subtract the segment start year from the segment end year to calculate the duration of segments 
var mag = endVal.subtract(startVal);         // substract the segment start index value from the segment end index value to calculate the delta of segments
var rate = mag.divide(dur);                  // calculate the rate of spectral change

// This segment info only includes segments that are post greatest disturbance (theoretically)
var segInfo = ee.Image.cat([startYearV.add(1), endYearV, startVal, endVal, mag, dur, rate])
                      .toArray(0)
                      .mask(vertexMask.mask());
//print(segInfo, 'segInfo');
//Map.addLayer(segInfo);

//var sortByThis = segInfo.arraySlice(0, 4, 5).toArray(0);
//var segInfoSorted = segInfo.arraySort(sortByThis);
//var bigDelta = segInfoSorted.arraySlice(1, 0, 1);
//print(bigDelta, 'bigDelta');
//Map.addLayer(bigDelta, {}, 'bigDelta');