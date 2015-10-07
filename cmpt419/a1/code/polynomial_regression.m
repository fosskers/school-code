% polynomial_regression.m
%
% Solution to Question 4.2.1
% Author: Colin Woodbury <cwoodbur@sfu.ca>

% Make sure workspace is empty.
clear;

% Load the data
[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);
tTr = t(1:100);
tTe = t(101:end);
X = Data(:,8:end);
X = normalizeData(X);
XTr = X(1:100,:);
XTe = X(101:end,:);

errM = [];
DEG_MAX = 6;

for deg=1:DEG_MAX
    PhiTr = designMatrix(XTr,'polynomial',deg);
    w = pinv(PhiTr) * tTr;

    % Squared Error for Training Data
    err = sum((tTr - PhiTr * w) .^ 2);

    % RMS training error
    errM(1,deg) = sqrt(err / size(XTr,1));

    % Squared Error for Test Data
    err = 0;
    PhiTe = designMatrix(XTe,'polynomial',deg);
    err = sum((tTe - PhiTe * w) .^ 2);

    % RMS testing Error
    errM(2,deg) = sqrt(err / size(XTe,1));
end

plot(1:DEG_MAX,errM);
ylabel('Error');
xlabel('Polynomial Degree');
legend('Training','Testing','Location','northwest');
