% Track a blue cup.

clear;

V = importdata('bluecup.mat');
I = rgb2ycbcr(V(178:318, 31:148, 1:3, 1));  % The model matrix.
M = histogram(I);  % Model histogram

[XS,YS] = histtrack(V, M);