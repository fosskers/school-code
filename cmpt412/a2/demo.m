% Never trust Matlab.
clear;

% Obtain the calibration LUT.
LUT = lut;

% Get cone normals.
NS = normals('cone-lamp1.tif','cone-lamp2.tif','cone-lamp3.tif',LUT);
