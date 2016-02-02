% Detect Pencil Edges
% Colin Woodbury - 301238755

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

for i=1:size(images)
    detect(images{i});
end