//Purpose of script: Select a particular road and make a vector buffer
// of 1km around it


var blocks = ee.FeatureCollection("TIGER/2010/Blocks");
var roads = ee.FeatureCollection("TIGER/2016/Roads");
var sfNeighborhoods = ee.FeatureCollection('users/sciencebyAJ/GEE_F2021/SFneighborhoods');

var geometry = sfNeighborhoods.geometry()
Map.centerObject(geometry)

// Filter blocks and roads to San Francisco boundary
var sfBlocks = blocks.filter(ee.Filter.bounds(geometry))
var sfRoads = roads.filter(ee.Filter.bounds(geometry))

// Select by Location
// Select all census blocks within 1km of an interstate
var interstateRoads = sfRoads.filter(ee.Filter.eq('rttyp', 'I'))

// Visualize the layers
Map.addLayer(sfBlocks.draw({color: 'gray', strokeWidth: 1}).clip(geometry), {},
  'All Blocks')
Map.addLayer(interstateRoads.draw({color: 'blue', strokeWidth: 3}).clip(geometry), {},
  'Interstate Roads')
  
// Define a spatial filter, with distance 1 km.
var joinFilter = ee.Filter.withinDistance({
  distance: 1000,
  leftField: '.geo',
  rightField: '.geo',
  maxError: 10
});
