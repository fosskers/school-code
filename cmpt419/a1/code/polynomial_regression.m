% polynomial_regression.m
%
% Solution to Question 4.2.1
% Author: Colin Woodbury <cwoodbur@sfu.ca>

% Load the data
[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);
X = Data(:,8:end);
%X = normalizeData(X);

% Try it with deg 1 for now.
Phi = designMatrix(X,'polynomial',1);
w = pinv(Phi) * t;

% Squared Error
err = 0;
for i=1:size(X,1)
    % Apply phi to x's
    phid = [1];  % phi0 = const 1
    for j=1:size(X(i,:),2)
        phid(j+1,1) = X(i,j) ^ 1; % degree 1 for now
    end

    % Accumulate error
    err = err + (t(i) - w' * phid) ^ 2;
end

% Final error value
err = err * 0.5;

plot(1:6,[err 0 0 0 0 0]);
