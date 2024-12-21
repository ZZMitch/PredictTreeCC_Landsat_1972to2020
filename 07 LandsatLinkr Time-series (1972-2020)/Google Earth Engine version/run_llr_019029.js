var ROP_CRW = ee.FeatureCollection("users/mitchbon/ROP_CRW");

// #############################################################################
// #############################################################################
// #############################################################################

/**
 * 1. View WRS-1 granules - figure out what WRS-1 granule to process
 * -- Make a processing dir: https://gist.github.com/jdbcode/36f5a04329d5d85c43c0408176c51e6d
 * 2. Create MSS WRS-1 reference image - for MSS WRS1 to MSS WRS2 harmonization
 * 3. View WRS-1 collection - identify bad MSS images
 * 4. Prepare MSS WRS-1 images
 * 5. Get TM-to-MSS correction coefficients
 * 6. Export MSS-to-TM corrected images
 * 7. Inspect the full time series collection - explore time series via animation and inspector tool to check for noise
 * 8. Run LandTrendr and display the fitted collection on the map
 * 9. Display the year and magnitude of the greatest disturbance during the time series
 */

var LLR_STEP = 7; 

// #############################################################################

var PROJ_PATH = 'users/mitchbon/LandTrendr';   // Must be the same path used to create the asset folder - cannot contain / at end - check for this in the code.
var WRS_1_GRANULE = '019029';
var CRS = 'EPSG:32617';

var DOY_RANGE = [160, 240];
var MAX_CLOUD = 100;
var MAX_GEOM_RMSE = 1;

var EXCLUDE_IDS = [
  'LM10190291974223PAC04',
  'LM10190291975182GMD03',
  'LM20190291975191PAC05',
  'LM10190291975218AAA02',
  'LM20190291976186AAA02',
  'LM10190291976195PAC01',
  'LM20190291976240AAA02',
  'LM20190291977198GMD04',
  'LM30190291978166PAC04',
  'LM30190291978184PAC03',
  'LM20190291978211PAC03',
  'LM30190291978220PAC03',
  'LM20190291979224XXX01',
  'LM20190291980183XXX01',
  'LM20190291981213PAC01',
  'LM30190291982163PAC01',
  'LM50170301986227AAA03'  // <-- JDB added (RE: https://github.com/gee-community/ee-LandsatLinkr/issues/8)
];

// #############################################################################
// #############################################################################
// #############################################################################

var params = {
  maxRmseVerify: MAX_GEOM_RMSE,
  maxCloudCover: MAX_CLOUD,
  doyRange: DOY_RANGE,
  wrs1: WRS_1_GRANULE,
  crs: CRS,
  excludeIds: EXCLUDE_IDS,
  baseDir: PROJ_PATH + '/' + WRS_1_GRANULE
};

var llr = require('users/jstnbraaten/modules:landsatlinkr/landsatlinkr.js');
switch (LLR_STEP) {
  case 1:
    llr.wrs1GranuleSelector();
    break;
  case 2:
    llr.exportMssRefImg(params);
    break;
  case 3:
    llr.viewWrs1Col(params);
    break;
  case 4:
    llr.processMssWrs1Imgs(params);
    break;
  case 5:
    llr.exportMss2TmCoefCol(params);
    break;
  case 6:
    llr.exportFinalCorrectedMssCol(params);
    break;
  case 7:
    var col = llr.getColForLandTrendrFromAsset(params);
    llr.displayCollection(col);
    llr.animateCollection(col);
    break;
  case 8:
    var lt = llr.runLandTrendrMss2Tm(params);
    //llr.displayFittedCollection() not built yet
    break;
  case 9:
    var lt = llr.runLandTrendrMss2Tm(params);
    llr.displayGreatestDisturbance(lt, params);
    break;
}

print(col);
Map.addLayer(col, {}, 'Landsat Collection');

// Just take TCA
var col_tca = col.select(['tca'])
print(col_tca, 'Just TCA')
Map.addLayer(col_tca, {}, 'Landsat Collection (TCA)');

// Convert from ImageCollection to multi-band Image
var col_tca1 = col_tca.toBands();
var col_tca2 = col_tca1.rename(ee.List.sequence(1972, 2020)); // Error: wants strings, these are floats
var col_tca2 = col_tca1.rename(['y1972', 'y1973', 'y1974', 'y1975', 'y1976', 'y1977', 'y1978', 'y1979',
              'y1980', 'y1981', 'y1982', 'y1983', 'y1984', 'y1985', 'y1986', 'y1987', 'y1988', 'y1989',
              'y1990', 'y1991', 'y1992', 'y1993', 'y1994', 'y1995', 'y1996', 'y1997', 'y1998', 'y1999',
              'y2000', 'y2001', 'y2002', 'y2003', 'y2004', 'y2005', 'y2006', 'y2007', 'y2008', 'y2009',
              'y2010', 'y2011', 'y2012', 'y2013', 'y2014', 'y2015', 'y2016', 'y2017', 'y2018', 'y2019',
              'y2020']) // Do we need to create new var or can we just update existing var?
print(col_tca2, 'TCA (1972-2020)')
Map.addLayer(col_tca2, {}, 'TCA (1972-2020)');
Map.addLayer(ROP_CRW, {}, 'ROP_CRW');

// Export to Drive - test (in case needed)
//Export.image.toDrive({
//  image: col_tca2,
//  description: 'TCA_72to20',
//  scale: 30,
//  region: ROP_CRW,
//})

// Export as Asset - to open and merge with WRS1 later
Export.image.toAsset({
  image: col_tca2,
  description: 'TCA_72to20',
  assetId: 'TCA_72to20_019029',
  scale: 30,
  region: ROP_CRW
});