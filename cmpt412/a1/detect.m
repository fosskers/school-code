function detect(filename)

I = imread(filename);
figure
imshow(I)

% Grayscale, and smooth the image.
GR = rgb2gray(I);
FI = imgaussfilt(GR);

% --- Multithresh --- %
% `multithresh` is the crux of the pencil detection technique used
% here. Instead of doing any actual edge detection, we opt to
% separate the image into different intensity
% regions. Experimentally, a value of 2 passed to the function
% performs best. 2 yields threshold levels that when given to
% `imquantize` as is done below, divides the image into three
% partitions by relative intensity. Further below, I interpret
% those partitions, and colour black only the areas which weren't
% already white. This yields a binary image where only the shapes
% of the pencils are visible, effectively avoiding the need for
% finicky edge detection altogether.

% multithresh :: Image -> Int -> Vector Int
%   Yields a vector of threshold level, and a `metric` of the
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

% --- Skeletonize --- %
% bwmorph :: BWImage -> String -> Int -> BWImage
%   Perform some operation, indicated by the given String. The Int
%   argument is the number of times to repeat the operation. `skel`
%   reduces white areas to thin stands.
BW = bwmorph(BW,'skel',Inf);

% Dilate once to thicken the lines and get better results on Hough.
BW = bwmorph(BW,'dilate');

% --- Hough Transform --- %
% hough :: BWImage -> (Matrix Float, Vector Float, Vector Float)
%   Perform a Hough Transformation.
[H,thetas,ros] = hough(BW);

% houghpeaks :: Matrix Float -> Int -> Matrix Int
%   Given the `H` matrix from `hough`, and a maximum bound for the
%   number of peaks, produce a matrix of coordinates of said peaks
%   in `H`. Four peaks seems to be an ideal number.
peaks = houghpeaks(H,4);

% houghlines :: BWImage -> V Float -> V Float -> M Int -> [Line]
%   Find the line segments formed from the theta values, ro values,
%   and peaks found by the other hough functions. This produces a
%   list of the longest straight lines it can find in the image.
lines = houghlines(BW,thetas,ros,peaks);

% --- Output --- %
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
