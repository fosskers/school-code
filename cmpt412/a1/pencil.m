% HB Inc. High-budget Pencil Detection Software
% Colin Woodbury - 301238755 <cwoodbur@sfu.ca>

% Never trust Matlab.
clear;

images = {
    'OnePencilA.JPG';
    'CrossedPencilsA.JPG';
    'Red_Green_Pencils.JPG';
    'Touching.JPG';
    'SixCrossed.JPG';
    'Three-on-Carpet.JPG'
};

% Find the pencils in each image.
for i=1:size(images)
    detect(images{i});
end
