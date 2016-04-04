% com.m
% Finds the indices of the center of mass of a matrix.
% author: Colin Woodbury <cwoodbur@sfu.ca>

function [cX cY] = com(M)
[rows cols] = size(M);

y = 1:rows;
x = 1:cols;

[X Y] = meshgrid(x,y);

% Get avg of whole matrix.
avg = mean(mean(M));

cY = round(mean(Y(M >= avg)));
cX = round(mean(X(M >= avg)));
