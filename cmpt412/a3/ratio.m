% ratio.m
% Form a ratio histogram. Works for variable sized input
% Histograms, but assumes that they themselves are the same size.
% author: Colin Woodbury <cwoodbur@sfu.ca>

function R = ratio(img,model)
R{size(model,1),size(model,2)} = [];  % Our Ratio Histogram.

for i=1:size(R,1)
    for j=1:size(R,2)
        if not(isempty(model{i,j}) || isempty(img{i,j}))
            R{i,j} = min(model{i,j} / img{i,j}, 1);
        else
            R{i,j} = 0;
        end
    end
end
