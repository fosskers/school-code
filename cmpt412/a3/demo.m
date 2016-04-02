% demo.m

M = histogram(imread('database/swain_database/crunchberries.sqr.128.bmp'));
I = histogram(imread('database/collage.bmp'));

R = ratio(I,M)
