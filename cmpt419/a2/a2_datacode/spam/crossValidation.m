% crossValidation
% Colin Woodbury - 301238755
% Adapted from A1

function [training, validation, trTa, vaTa] = crossValidation(data, ...
                                                  targets, offset, base)

% Given some data (say n=100), split it into training and
% validation sets based on a given base-?? offset.
% If the offset is 0, data rows 1-10 will be the validation set.
% If the offset is 0, data rows 11-20 will be the validation set,
% and so on.

% For training data
validation = data(offset*base+1:(offset+1)*base,:);
before = data(1:offset*base,:);
after = data((offset+1)*base + 1:end,:);
training = vertcat(before,after);

% For target vectors
vaTa = targets(offset*base+1:(offset+1)*base,:);
before = targets(1:offset*base,:);
after = targets((offset+1)*base + 1:end,:);
trTa = vertcat(before,after);
