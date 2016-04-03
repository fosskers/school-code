% demo.m

% Never trust Matlab.
clear;

models = { 'database/swain_database/crunchberries.sqr.128.bmp'
           'database/swain_database/clamchowder.sqr.128.bmp'
           'database/swain_database/flyer.sqr.128.bmp'
           'database/swain_database/frankenberry.sqr.128.bmp'
           'database/swain_database/charmin.sqr.128.bmp'
           'database/swain_database/chickensoupnoodles.sqr.128.bmp'
           'database/swain_database/balloons.sqr.128.bmp'
           'database/swain_database/carebears.sqr.128.bmp'
           'database/swain_database/bakit.sqr.128.bmp' };

img = rgb2ycbcr(imread('database/collage.bmp'));
I = histogram(img);

figure
imshow(img);

for i=1:size(models,1)
    fprintf('Demoing %s\n',models{i});

    M = histogram(rgb2ycbcr(imread(models{i})));
    [X,Y] = backproj(img,I,M);

    % Add match results to the plot.
    hold on;
    p = plot([Y], [X],'o');
    p.MarkerEdgeColor = 'red';
    p.MarkerSize = 20;
    hold off;
end
