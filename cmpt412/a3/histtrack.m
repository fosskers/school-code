% histtrack.m
% Track an object through a given video.
% author: Colin Woodbury <cwoodbur@sfu.ca>

function [XS,YS] = histtrack(V, M)
XS = [];
YS = [];

for i=1:size(V,4)
    fprintf('FRAME %d\n',i);

    Fimg = rgb2ycbcr(V(:, :, :, i));  % The current Frame
    F = histogram(Fimg);
    [X Y] = backproj(Fimg,F,M);

    XS = [XS; X];
    YS = [YS; Y];
end
