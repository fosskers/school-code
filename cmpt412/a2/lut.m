% lut.m
% Build a lookup table of surface normals, indexed by the ratios of
% intensities from three light sources. The spheres used to
% calibrate the LUT must be available in the directory this
% function is run in.
%
% author: Colin Woodbury <cwoodbur@sfu.ca>
% modified: 2016 March  7 @ 21:27

function LUT = lut
    % Read in the three sphere images. 468 rows (max y), 637 columns (max x)
    i1 = rgb2gray(imread('sphere-lamp1.tif'));
    i2 = rgb2gray(imread('sphere-lamp2.tif'));
    i3 = rgb2gray(imread('sphere-lamp3.tif'));

    fprintf('Calibrating LUT...\n');
    
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
    % normal, and convert it to stereographic coordinates.
    LUT = {};
    LUT{256,256} = [];
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

                E1 = i1(i,j) + 1;
                E2 = i2(i,j) + 1;

                % Overwrite on collisions. Doing anything else
                % causes NaNs further down.
                LUT{E1,E2} = [X,Y];

                count = count + 1;
            end
        end
    end

    fprintf('%d (%.2f%%) pixels in Sphere\n', count, 100 * count / (MAX_ROWS*MAX_COLS));

    fprintf('LUT size: %d x %d\n', size(LUT,1), size(LUT,2));

    % View the calibration sphere.
    % mesh(ns(:,:,3));

    fprintf('Gather LUT values to interpolate\n');
    
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

    fprintf('Interpolate\n');
    
    % Interpolate. Note that using any other interpolation scheme
    % than `v4` yields a high number of `NaN` values.
    [xq,yq] = meshgrid(1:256,1:256);
    interpX = griddata(RS,CS,XS,xq,yq,'linear');
    interpY = griddata(RS,CS,YS,xq,yq,'linear');

    % View the interpolated X and Y values.
    %{
    mesh(interpX);
    figure
    mesh(interpY);
    %}

    fprintf('Repair LUT\n');
    
    % Convert back to a cell array.
    for i=1:size(LUT,1)
        for j=1:size(LUT,2)
            LUT{i,j} = [interpX(i,j), interpY(i,j)];
        end
    end
