% backproj.m
% Perform back projection on an image, given a model to locate
% within it.
% author: Colin Woodbury <cwoodbur@sfu.ca>

function [X,Y] = backproj(img,I,M)
R = ratio(I,M);  % The Ratio Histogram
B = [];

% Assign ratios to each pixel of the image.
fprintf('Assign ratios...\n');
for i=1:size(img,1)
    for j=1:size(img,2)
        [CB,CR] = bin(img(i,j,2), img(i,j,3));
        B(i,j) = R{CB,CR};
    end
end

% Apply the convolution.
fprintf('Convolving...\n');
MASK = ones(20,20);  % Experimentally best-sized mask.
C = conv2(B,MASK);

% Find the indices of the maximum point.
[X,Y] = find(C == max(C(:)));
