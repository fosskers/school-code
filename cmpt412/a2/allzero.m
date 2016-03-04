% allzero.m
% Is a matrix all zeros?
%
% author: Colin Woodbury <cwoodbur@sfu.ca>
% modified: 2016 March  4 @ 10:44

function res = allzero(m)

% Assume true.
res = 1;

for i=1:size(m,1)
    for j=1:size(m,2)
        if m(i,j) ~= 0
            res = 0;
            return
        end
    end
end