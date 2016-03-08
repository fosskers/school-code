% quiv.m
% Average an array of normals and display the results as a quiver
% plot.
%
% author: Colin Woodbury
% modified: 2016 March  7 @ 15:43

function quiv(NS)

fprintf('Averaging normals...\n');

XS = [];
YS = [];
US = [];
VS = [];

% Use a step-size of 100 to speed up calculation and produce a
% quiver that is meaningful to look at. Otherwise, it will contain
% more than 250k normals.
tempX = [];
tempY = [];
STEP = 10;
for i=1:size(NS,1)
    for j=1:size(NS,2)

        % Aggregate results at every 100x100 pixel block to find
        % average surface orientation for that block.
        if mod(i,STEP) == 0 && mod(j,STEP) == 0
            US = [US, sum(tempX) / size(tempX,2)];
            VS = [VS, sum(tempY) / size(tempY,2)];
            XS = [XS, i / STEP];
            YS = [YS, j / STEP];
            tempX = [];
            tempY = [];
        end

        if not(isempty(NS{i,j}))
            tempX = [tempX, NS{i,j}(1)];
            tempY = [tempY, NS{i,j}(2)];
        end
    end
end

fprintf('Showing quiver\n');
figure
quiver(XS,YS,US,VS);
