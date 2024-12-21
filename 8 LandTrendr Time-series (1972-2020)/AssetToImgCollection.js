// load/import image
var img = ee.Image('users/mitchbon/tca_inv') // Inverted TCA Composite from LandsatLinkR
           // .select([0,1,2,3,4,6]); // selected bands

// check out the image metadata
print(img);

// create a band and year list - bands start at 0 position 
var bands = ee.List([[0,1972], [1,1974], [2,1975], [3,1976], [4,1977], [5,1978], [6,1979],
                     [7,1980], [8,1981], [9,1982], [10,1983],[11,1984],[12,1985],[13,1986],[14,1987],[15,1988],[16,1989],
                     [17,1990],[18,1991],[19,1992],[20,1993],[21,1994],[22,1995],[23,1996],[24,1997],[25,1998],[26,1999],
                     [27,2000],[28,2001],[29,2002],[30,2003],[31,2004],[32,2005],[33,2006],[34,2007],[35,2008],[36,2009],
                     [37,2010],[38,2011],[39,2012],[40,2013],[41,2014],[42,2015],[43,2016],[44,2017],[45,2018]]);
// alter band/year list to correspond to your stack
// print(bands)

// extract each band as an image in a list
var imgList = bands.map(function(i){
  i = ee.List(i);
  return img.select([i.get(0)])
           .set('system:time_start', ee.Date.fromYMD(i.get(1), 8, 1));
});
//print(imgList)

// convert the image list to a collection
var col = ee.ImageCollection(imgList);
print(col)

// test it out
// var b1 = col.filterDate('1972-01-01', '1972-12-31');
var test = col.filterDate('2018-01-01', '2018-12-31');
// print(b1);
print(test);
// Map.addLayer(b1, {min:-3000, max:0}, 'b1');
Map.addLayer(test, {min:-4000, max:0}, 'test');