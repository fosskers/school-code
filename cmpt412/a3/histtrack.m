% histtrack.m
% Track an object through a given video.
% author: Colin Woodbury <cwoodbur@sfu.ca>

function [XS,YS] = histtrack(vidFile, xStart, xEnd, yStart, yEnd)

V = importdata(vidFile);
I = rgb2ycbcr(V(xStart:xEnd, yStart:yEnd, 1:3, 1));  % The model matrix.
M = histogram(I);  % Model histogram

figure
imshow(I);

XS = [];
YS = [];

for i=1:size(V,4)
    fprintf('FRAME %d\n',i);

    F = rgb2ycbcr(V(:, :, :, i));  % The current Frame
    [X,Y] = backproj(F,M);

    XS = [XS; X];
    YS = [YS; Y];
end

figure
imshow(F);
hold on;
p = plot([YS], [XS], 'o');
p.MarkerEdgeColor = 'red';
p.MarkerSize = 10;
hold off;
