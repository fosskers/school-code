% Demo mean shift

clear;

V = importdata('bluecup.mat');
I = V(178:318, 31:148, 1:3, 1);  % The model matrix.
M = histogram(rgb2ycbcr(I));  % Model histogram

figure
imshow(I);
[XS YS] = meanshift(V, M);

figure
imshow(V(:,:,:,50));
hold on;
p = plot([XS], [YS], 'o');
p.MarkerEdgeColor = 'red';
p.MarkerSize = 10;
hold off;
