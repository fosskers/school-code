% Detect Pencil Edges
% Colin Woodbury - 301238755

% Never trust Matlab.
clear;

%I = imread('Three-on-Carpet.JPG');  % Perfect!
%I = imread('OnePencilA.JPG');
%I = imread('CrossedPencilsA.JPG');
%I = imread('Red_Green_Pencils.JPG');  % Perfect!
%I = imread('SixCrossed.JPG');
figure
imshow(I)

% Grayscale, and smooth the image.
GR = rgb2gray(I);
FI = imgaussfilt(GR);

%figure
%imshow(FI)

% --- Multithresh Method --- %

% multithresh :: Image -> Int -> Vector Int
%n   Yields a vector of threshold level, and a `metric` of the
%   success of the algorithm.
[levels,metric] = multithresh(FI,2);  % Threshold of 2 seems to be best.

fprintf('Multithresh levels: [%d %d]\n', levels(1), levels(2))

% imquantize :: Image -> Vector Int -> Image
%   Yields a new image the same size as the input image, and has
%   values between 1 and (n+1), where n is the length of the input
%   vector of theshold values.
seg_I = imquantize(FI, levels);

% Report success metric of the thresholding algorithm.
fprintf('Multithresh metric: %.3f\n', metric)

% Sanitize `seg_I` to only show white where pencils were.
BW = [];

for i=1:size(seg_I,1)
    for j=1:size(seg_I,2)
        if seg_I(i,j) == 3
            BW(i,j) = 1.0;
        else
            BW(i,j) = 0;
        end
    end
end

figure
imshow(BW)

% HBreak --- Doesn't work!
%{
H = bwmorph(BW,'hbreak');
figure
imshow(H)
%}

% Skeletonize - Reduce the pencil shapes to thin lines
skel = bwmorph(BW,'skel',Inf);
figure
imshow(skel)

% Hough Transform
[H,thetas,ros] = hough(BW);
peaks = houghpeaks(H,2);
lines = houghlines(BW,thetas,ros,peaks);

% Overlay calculated lines over grayscale image
figure
imshow(FI)
hold on
max_len = 0;
for k = 1:length(lines)
    xy = [lines(k).point1; lines(k).point2];
    plot(xy(:,1),xy(:,2),'LineWidth',2,'Color','green');

    % Plot beginnings and ends of lines
    plot(xy(1,1),xy(1,2),'x','LineWidth',2,'Color','yellow');
    plot(xy(2,1),xy(2,2),'x','LineWidth',2,'Color','red');

    % Determine the endpoints of the longest line segment
    len = norm(lines(k).point1 - lines(k).point2);
    if ( len > max_len)
        max_len = len;
        xy_long = xy;
    end
end
