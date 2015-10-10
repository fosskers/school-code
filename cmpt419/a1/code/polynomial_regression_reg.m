% polynomial_regression_reg.m
%
% Solution to Question 4.4.1
%

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

lambdas = [0 0.01 0.1 1 10 100 1000 10000];

errM = [];

% PhiTe = designMatrix(XTe,'polynomial',2);

for j=0:9
    [training validation trTa vaTa] = crossValidation(XTr, tTr, j);
    
    PhiTr = designMatrix(training,'polynomial',2);
    PhiVa = designMatrix(validation,'polynomial',2);
    Dot = PhiTr' * PhiTr;

    for i=1:size(lambdas,2)
        lam = lambdas(i);
        w = inv(lam * eye(size(Dot,1)) + Dot) * PhiTr' * trTa;

        % Squared Error for Training Data
        % err = sum((trTa - PhiTr * w) .^ 2) + lam * w' * w;

        % RMS training error
        % errM(1,i) = sqrt(err / size(training,1));

        % Squared Error for Validation Set
        err = sum((vaTa - PhiVa * w) .^ 2) + lam * w' * w;

        % RMS Validation Error
        errM(j+1,i) = sqrt(err / size(validation,1));
    end
end

% Average the errors for each Lambda value
for i=1:size(lambdas,2)
    avgErrs(i) = sum(errM(:,i)) / 10;
end

semilogx(lambdas,avgErrs);
ylabel('Average Error');
xlabel('Lambda Values');
legend('Validation','Location','northwest');
title('Average Validation Error by Lambda Value');
