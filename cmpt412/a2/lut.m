% lut.m
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

    % --- Build an LUT from valid sphere normals --- %
    %
    % For each pixel, we find its vector in R^2 sphere-space.
    % If its magnitude is longer than the radius, then that pixel
    % doesn't belong to the sphere. Otherwise, we calculate the
    % normal, convert to stereographic coordinates, and store it
    % based on ratios of pixel intensities.
    LUT = {};
    ns = [];  % Normals. Likely not needed.
    count = 0;
    for i=1:MAX_ROWS
        for j=1:MAX_COLS
            v = [i,j] - c(:,1:2);  % Vector in sphere-space.
            if sqrt(sum(v .^ 2)) <= r
                v = [v, sqrt(r^2 - v(1)^2 - v(2)^2)];
                n = v / sqrt(sum(v .^ 2));  % The normal.
                ns(i,j,1) = n(1);
                ns(i,j,2) = n(2);
                ns(i,j,3) = n(3);

                X = n(1) / (1 - n(3));
                Y = n(2) / (1 - n(3));

                E1 = [i1(i,j,1), i1(i,j,2), i1(i,j,3)];
                E2 = [i2(i,j,1), i2(i,j,2), i2(i,j,3)];
                E3 = [i3(i,j,1), i3(i,j,2), i3(i,j,3)];

                % Find the ratios at the vector level, then average
                % (grayscale) to get a single scalar
                % intensity. `ceil` and `+ 1` prevent 0-index
                % errors. Stupid Matlab and its 1-indexing.
                E12 = ceil(sum(E1 ./ E2) / 3) + 1;
                E23 = ceil(sum(E2 ./ E3) / 3) + 1;

                % Overwrite on collisions. Doing anything else
                % causes NaNs further down.
                LUT{E12,E23} = [X,Y];

                count = count + 1;
            end
        end
    end

    fprintf('%d (%.2f%%) pixels in Sphere\n', count, 100 * count / (MAX_ROWS*MAX_COLS));

    % View the calibration sphere.
    % mesh(ns(:,:,3));

    % --- Interpolate stereographic coordinates in empty cells --- %
    % Contruct the values necessary for `griddata`.
    RS = [];
    CS = [];
    XS = [];
    YS = [];
    for i=1:size(LUT,1)
        for j=1:size(LUT,2)
            if not(isempty(LUT{i,j}))
                RS = [RS, i];
                CS = [CS, j];
                XS = [XS, LUT{i,j}(1)];
                YS = [YS, LUT{i,j}(2)];
            end
        end
    end

    % Interpolate. Note that using any other interpolation scheme
    % than `v4` yields a high number of `NaN` values.
    [xq,yq] = meshgrid(1:256,1:256);
    interpX = griddata(RS,CS,XS,xq,yq,'v4');
    interpY = griddata(RS,CS,YS,xq,yq,'v4');

    % View the interpolated X and Y values.
    %{
    mesh(interpX);
    figure
    mesh(interpY);
    %}

    % Convert back to a cell array.
    for i=1:size(LUT,1)
        for j=1:size(LUT,2)
            LUT{i,j} = [interpX(i,j), interpY(i,j)];
        end
    end
