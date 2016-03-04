% normals.m
% Find the surface orientation of an object, based on three images of
% it taken with different lighting and an orientation LUT.
% The LUT can be obtained via the `lut` function.
%
% author: Colin Woodbury <cwoodbur@sfu.ca>
% modified: 2016 March  4 @ 10:09

function NS = normals(if1,if2,if3,LUT)

fprintf('Discovering surface orientation...\n');

i1 = imread(if1);
i2 = imread(if2);
i3 = imread(if3);

% A cell array of recovered surface normals.
NS = {};

for i=1:size(i1,1)
    for j=1:size(i1,2)
        % Find the intensity indices in the _exact_ same way as is
        % done in `lut.m`.
        E1 = [i1(i,j,1), i1(i,j,2), i1(i,j,3)];
        E2 = [i2(i,j,1), i2(i,j,2), i2(i,j,3)];
        E3 = [i3(i,j,1), i3(i,j,2), i3(i,j,3)];

        % Do not proceed if any of the pixels is completely black.
        if not(allzero(E1) || allzero(E2) || allzero(E3))
            E12 = ceil(sum(E1 ./ E2) / 3) + 1;
            E23 = ceil(sum(E2 ./ E3) / 3) + 1;

            % Discover the FG coordinates.
            XY = LUT{E12,E23};
            X = XY(1);
            Y = XY(2);

            % Recover the surface normal.
            x = (2 * X) / (1 + X^2 + Y^2);
            y = (2 * Y) / (1 + X^2 + Y^2);
            z = (-1 + X^2 + Y^2) / (1 + X^2 + Y^2);

            NS{i,j} = [x,y,z];
        end
    end
end
