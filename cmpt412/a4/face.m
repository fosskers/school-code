% face.m
% All code for face recognition, plus a demo.
% author: Colin Woodbury

% Demo code
function F = face
    I = single(imread('s1/1.pgm'));
    F = vectorize(I);
end

% Turn an image file into a transformed feature vector via
% Fast Fourier.
function F = vectorize(img)
    F = [];
    I = fft2(img);

    for i=1:4
        for j=1:4
            F = [F; I(i,j)];
        end
    end

    % To simplify, just take the raw magnitude of each
    % component. This likely won't make much of a difference, as
    % the source paper demonstrates that the Complex component has
    % less of an effect than the Real.
    F = abs(F);
end