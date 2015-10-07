% polynomial_regression_1d.m
%
% Solution to Question 4.2.2
% Author: Colin Woodbury <cwoodbur@sfu.ca>

% Make sure workspace is empty.
clear;

% Load the data
[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);
tTr = t(1:100);
tTe = t(101:end);
X = Data(:,8:15);
X = normalizeData(X);
XTr = X(1:100,:);
XTe = X(101:end,:);

errM = [];
COLS = 8;

for col=1:COLS
    PhiTr = designMatrix(XTr(:,col),'polynomial',3);
    w = pinv(PhiTr) * tTr;

    % Squared Error for Training Data
    err = sum((tTr - PhiTr * w) .^ 2);

    % RMS training error
    errM(1,col) = sqrt(err / size(XTr,1));

    % Squared Error for Test Data
    err = 0;
    PhiTe = designMatrix(XTe(:,col),'polynomial',3);
    err = sum((tTe - PhiTe * w) .^ 2);

    % RMS testing Error
    errM(2,col) = sqrt(err / size(XTe,1));
end

bar(8:15,errM');
ylabel('Error');
xlabel('Features');
legend('Training','Testing','Location','northwest');
