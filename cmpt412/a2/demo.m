% Never trust Matlab.
clear;

% Obtain the calibration LUT.
LUT = lut;

% Cone
NS = normals('cone-lamp1.tif','cone-lamp2.tif','cone-lamp3.tif',LUT);
quiv(NS);

% Hex
NS = normals('hex1-lamp1.tif','hex1-lamp2.tif','hex1-lamp3.tif',LUT);
quiv(NS);

% Sphere again
NS = normals('sphere-lamp1.tif','sphere-lamp2.tif','sphere-lamp3.tif',LUT);
quiv(NS);

NS = normals('cylinder-lamp1.tif','cylinder-lamp2.tif','cylinder-lamp3.tif',LUT);
quiv(NS);

NS = normals('ellipsoid-lamp1.tif','ellipsoid-lamp2.tif','ellipsoid-lamp3.tif',LUT);
quiv(NS);

NS = normals('hex2-lamp1.tif','hex2-lamp2.tif','hex2-lamp3.tif',LUT);
quiv(NS);