% LUT.m
% Build a lookup table of surface normals, indexed by the ratios of
% intensities from three light sources. The spheres used to
% calibrate the LUT must be available in the directory this
% function is run in.
%
% author: Colin Woodbury <cwoodbur@sfu.ca>
% modified: 2016 February 29 @ 17:06

function LUT = lut
    % Read in the three sphere images
    i1 = imread('sphere-lamp1.tif');
    i2 = imread('sphere-lamp2.tif');
    i3 = imread('sphere-lamp3.tif');

    % [For testing] Display the three images
    %imshow(i1);
    %    figure;
    %    imshow(i2);
    %    figure;
    imshow(i3);

    % Points sampled from the sphere to determine the radius:
    % (181,304) (464,304) (322,158) (322,436)
    % Note that (0,0) is in the top-left corner of the image.

    % Sphere parameters as found manually via `ginput`.
    c = [323,300,0];  % The center.
    r = 142;  % The radius.
    
    []  % Empty result, for now.