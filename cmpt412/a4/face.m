% face.m
% All code for face recognition, plus a demo.
% author: Colin Woodbury

% Demo code
function face
    DB = database;

    % Randomize and extract a test set.
    DB = DB(randperm(length(DB)));
    tests = DB(1:200);
    DB = DB(201:end);
    matches = 0;

    % Try to identify the face in the test set. How accurate are
    % we?
    for i=1:length(tests)
        name = identify(tests(i).vec,DB);
        if strcmp(name,tests(i).name)
            matches = matches + 1;
        end
    end

    fprintf('Identified %d/%d faces correctly\n', matches, length(tests));
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

% Given a face in its feature vector form and a database to match
% against, yield a (subject) name associated with the candidate
% match.
% Assume that the exact face `F` is not already present in the DB.
function name = identify(F,DB)
    M_rate = Inf;
    name = 'Matlab needs strong typing';

    % Naively compare the given Face (vector) with every other one
    % in the database. The candidate match is the vector with the
    % lowest Euclidean distance.
    for i=1:length(DB)
        D = dist(F,DB(i).vec);

        if D < M_rate
            M_rate = D;
            name = DB(i).name;
        end
    end
end

% Find the Euclidean Distance between two vectors. For face
% recognition, the lower the value, the better.
function D = dist(U,V)
    D = sqrt(sum((U - V) .^ 2));
end
