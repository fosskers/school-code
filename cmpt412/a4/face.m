% face.m
% All code for face recognition, plus a demo.
% author: Colin Woodbury

% Demo code
function F = face
    F = database
end

% Populate the database.
function DB = database
    DB = [];

    for s=1:40
        for i=1:10
            DB = [DB; subject(sprintf('s%d',s),sprintf('s%d/%d.pgm',s,i))];
        end
    end
end

% Given a subject name and filename to a picture of them, construct
% a `struct` that identifies them with their feature vector.
function S = subject(name,file)
    S = struct('name',name,'vec',vectorize(imread(file)));
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

% Find the Euclidean Distance between two vectors. For face
% recognition, the lower the value, the better.
function D = dist(U,V)
    D = sqrt(sum((U - V) .^ 2));
end
