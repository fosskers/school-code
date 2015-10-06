function [training, validation] = crossValidation(data, offset)

% Given some data (say n=100), split it into training and
% validation sets based on a given base-10 offset.
% If the offset is 0, data rows 1-10 will be the validation set.
% If the offset is 0, data rows 11-20 will be the validation set,
% and so on.

validation = data(offset*10+1:offset*10+10,:);
before = data(1:offset*10,:);
after = data(offset*10+11:end,:);
training = vertcat(before,after);
