% LUT.m
% Build a lookup table of surface normals, indexed by the ratios of
% intensities from three light sources. The spheres used to
% calibrate the LUT must be available in the directory this
% function is run in.
%
% author: Colin Woodbury <cwoodbur@sfu.ca>
% modified: 2016 February 29 @ 17:06

function LUT = lut
    % Read in the three sphere images. 468 rows (max y), 637 columns (max x)
    i1 = imread('sphere-lamp1.tif');
    i2 = imread('sphere-lamp2.tif');
    i3 = imread('sphere-lamp3.tif');

    % [For testing] Display the three images
    %imshow(i1);
    %    figure;
    %    imshow(i2);
    %    figure;
    %imshow(i3);
    
    % Points sampled from the sphere to determine the radius:
    % (181,304) (464,304) (322,158) (322,436)
    % Note that (1,1) is in the top-left corner of the image.

    % Sphere parameters as found manually via `ginput`.
    c = [323,300,0];  % The center in image-space.
    r = 142;  % The radius, in pixels.

    % The size of the image files.
    MAX_ROWS = size(i1,1);
    MAX_COLS = size(i1,2);

    % The `z` values of every (x,y) point in the image.
    % For each pixel, we find its vector in sphere-space.
    % If its magnitude is longer than its radius, we set `z` to 0.
    % Otherwise, we find the actual `z` value in sphere-space.
    zs = zeros(MAX_ROWS,MAX_COLS);
    count = 0;
    for i=1:MAX_ROWS
        for j=1:MAX_COLS
            v = [i,j] - c(:,1:2);  % Vector in sphere-space.
            if sqrt(v(1) ^ 2 + v(2) ^ 2) > r
                zs(i,j) = 0;  % Point isn't within the sphere.
            else
                zs(i,j) = sqrt(r ^ 2 - v(1) ^ 2 - v(2) ^ 2);
                count = count + 1;
            end
        end
    end

    fprintf('%d (%.2f%%) pixels in Sphere\n', count, 100 * count / (MAX_ROWS*MAX_COLS));

    % View the calibration sphere.
    % mesh(zs);
    
    []  % Empty result, for now.
