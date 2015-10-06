% polynomial_regression.m
%
% Solution to Question 4.2.1

% Load the data
[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);
X = Data(:,8:end);
%X = normalizeData(X);

% TO DO:: Fill in
