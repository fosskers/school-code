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

I = rgb2ycbcr(imread('database/collage.bmp'));

for i=1:size(models,1)
    M = histogram(rgb2ycbcr(imread(models{i})));
    [X,Y] = backproj(I,M);

    figure
    imshow(I);
    hold on;
    p = plot([Y], [X],'o');
    p.MarkerEdgeColor = 'red';
    p.MarkerSize = 20;
    hold off;
    title(models{i});
end

