% meanshift.m
% Using backprojection over a small subsection of the frame to
% increase peformance.
% author: Colin Woodbury <cwoodbur@sfu.ca>

function [XS YS] = meanshift(V, M)

% Find starting location over whole image.
% Repeat:
%   Get subimage based on current location
%   Get ratio Hist
%   Replace subimage pixels with ratio values as in backproj
%   Find center of mass
%   Move to center of mass and record it

% Max dimensions of a frame
MAX_X = size(V,2);
MAX_Y = size(V,1);

% Size of the area to look around
R = 100;

% Experimentally best-sized mask.
MASK = ones(20,20);

% Our results
XS = [];
YS = [];

% Find the starting location over the whole image.
Fimg = rgb2ycbcr(V(:, :, :, 1));  % The first Frame
F = histogram(Fimg);
[X Y] = backproj(Fimg,F,M);

% O(n^3), baby.
for i=1:size(V,4)
    fprintf('FRAME: %d | (%d,%d)\n',i,X,Y);
    
    % Get subimage based on current location.
    %    Fimg = rgb2ycbcr(V(max(0,Y-R):min(MAX_Y,Y+R), max(0,X-R): ...
    %                  min(MAX_X,X+R), :, i));
    Fimg = rgb2ycbcr(V(max(1,Y-R):min(MAX_Y,Y+R),max(1,X-R):min(MAX_X,X+R),:, i));

    %figure
    %imshow(ycbcr2rgb(Fimg));
    F = histogram(Fimg);

    % Get ratio matrix and replace the image pixels with ratios
    Rat = ratio(F,M);
    B = [];
    
    for j=1:size(Fimg,1)
        for k=1:size(Fimg,2)
            [CB,CR] = bin(Fimg(j,k,2), Fimg(j,k,3));
            B(j,k) = Rat{CB,CR};
        end
    end

    % Convolve
    C = conv2(B,MASK);

    % Calculate Mean-Shift Vector to find new coordinates in global space
    [X_new Y_new] = com(C);  % The center of mass, our next
                             % starting point.
    X = X + X_new - round(size(Fimg,2)/2);
    Y = Y + Y_new - round(size(Fimg,1)/2);

    XS = [XS; X];
    YS = [YS; Y];
end
