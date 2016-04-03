% histogram.m
% Form a colour histogram from a YCbCr image.
% author: Colin Woodbury <cwoodbur@sfu.ca>
%
% The input `img` is assumed to be a NxMx3 matrix of YCbCr
% values. Experiments have shown YCbCr Histograms to have a higher
% match rate than RGB, while requiring less computation to construct.
%
% Images read with `imread` can be converted to YCbCr with `rgb2ycbcr`.
%
% Our Histograms have two axes, with 16 bins per axis. While more
% bins are not necessarily better, 16 has shown experimentally to
% be a decent default.

function H = histogram(img)
H{16,16} = [];  % Our histogram.
blacks = 0;     % A count of the black pixels discarded.
totalPix = size(img,1) * size(img,2);

fprintf('Image Pixels: %d\n', totalPix);

for i=1:size(img,1)
    for j=1:size(img,2)
        % TODO: Don't add really dark pixels!
        % Might also have to deal with int overflow.
        if img(i,j,1) > 0
            % Transform CbCr values into bin indices.
            [CB,CR] = bin(img(i,j,2), img(i,j,3));
        
            % Increment the bin's pixel count, watching out for empty
            % bins.
            if isempty(H{CB,CR})
                H{CB,CR} = 1;
            else
                H{CB,CR} = H{CB,CR} + 1;
            end
        else
            blacks = blacks + 1;
        end
    end
end

fprintf('Black pixels discarded: %d (%.2f%%)\n', blacks, 100 * blacks ...
        / totalPix);