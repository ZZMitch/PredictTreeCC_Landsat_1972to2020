var tcainv = ee.Image("users/mitchbon/tca_inv");

//check out https://emaprlab.users.earthengine.app/view/lt-gee-change-mapper to help parameterize

ee.Algorithms.TemporalSegmentation.LandTrendr
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js');

//var startYear = 1972;
//var endYear = 2018;

//var coords = [[-80.374, 43.352],
//              [-79.398, 43.352],
//              [-79.398, 44.104],
//              [-80.374, 44.104],
//              [-80.374, 43.352]];
              
//var aoi = ee.Geometry.Polygon(coords)

// Can play around with these... check literature for potential values
var run_params = { 
  maxSegments:            6,
  spikeThreshold:         0.9,
  vertexCountOvershoot:   3,
  preventOneYearRecovery: false,
  recoveryThreshold:      0.25,
  pvalThreshold:          0.1,
  bestModelProportion:    1.25,
  minObservationsNeeded:  6
};

run_params.timeSeries = tcainv

var LTresult = ee.Algorithms.TemporalSegmentation.LandTrendr(run_params) // run LT-GEE
                                                   // .select('LandTrendr'); // just get LandTrendr band
print(LTresult)

//testing...
var segmentationInfo = LTresult.select(['LandTrendr']); // subset the LandTrendr segmentation info

var LTarray = LTresult.select(['LandTrendr']); // subset the LandTrendr segmentation info
var year = LTarray.arraySlice(0, 0, 1); // slice out the year row
var fitted = LTarray.arraySlice(0, 2, 3); // slice out the fitted values row

//print(LTarray)
//print(year)

//var segmentationInfo = LTresult.select(['rmse']); // subset the rmse band
//print(segmentationInfo)

//var B4ftv = LTresult.select(['b4_fit']); // subset the B4_fit band
//print(B4ftv)

//Getting segment information
//var vertexMask = LTresult.arraySlice(0, 3, 4); // slice out the 'Is Vertex' row - yes(1)/no(0)
//var vertices = LTresult.arrayMask(vertexMask); // use the 'Is Vertex' row as a mask for all rows

//var left = vertices.arraySlice(1, 0, -1);    // slice out the vertices as the start of segments
//var right = vertices.arraySlice(1, 1, null); // slice out the vertices as the end of segments
//var startYear = left.arraySlice(0, 0, 1);    // get year dimension of LT data from the segment start vertices
//var startVal = left.arraySlice(0, 2, 3);     // get spectral index dimension of LT data from the segment start vertices
//var endYear = right.arraySlice(0, 0, 1);     // get year dimension of LT data from the segment end vertices 
//var endVal = right.arraySlice(0, 2, 3);      // get spectral index dimension of LT data from the segment end vertices

//var dur = endYear.subtract(startYear);       // subtract the segment start year from the segment end year to calculate the duration of segments 
//var mag = endVal.subtract(startVal);         // substract the segment start index value from the segment end index value to calculate the delta of segments
//var rate = mag.divide(dur);                  // calculate the rate of spectral change

//var segInfo = ee.Image.cat([startYear.add(1), endYear, startVal, endVal, mag, dur, rate])
//                      .toArray(0)
//                      .mask(vertexMask.mask());
//print(segInfo)

//export image??
//Export.image.toDrive({
//  image: LTresult
//})