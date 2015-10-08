% sigmoid_regression.m
%
% Solution to Question 4.3.1

clear;

% Load the data
[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);

X = Data(:,11); %normalizeData(Data(:,11));

ntrain = 100;
X_train = X(1:ntrain);
X_test = X(ntrain+1:end);
t_train = t(1:ntrain);
t_test = t(ntrain+1:end);

% Plot a curve showing learned function.
x_ev = (min(X):3000:max(X))';

Phi = designMatrix(X_train,'sigmoid',[100 10000],2000);
w = pinv(Phi) * t_train;

y_ev = designMatrix(x_ev,'sigmoid',[100 10000],2000) * w;

% Total draining error
% err = sqrt(sum((t_train - Phi * w) .^ 2) / size(X_train,1))
% Total testing error
% PhiTe = designMatrix(X_test,'sigmoid',[100 10000],2000);
% err = sqrt(sum((t_test - PhiTe * w) .^ 2) / size(X_test,1))

% RMS testing Error
% errM(2,deg) = sqrt(err / size(XTe,1));

figure;
plot(x_ev,y_ev,'r.-');  
hold on;
plot(X_train,t_train,'g.');
plot(X_test,t_test,'bo');
hold off;
title(sprintf('Feature %d Data and Fitted Curve with Sigmoid',11));
legend('Fitted Curve','Training','Testing','Location','northeast');
% Make the fonts larger, good for reports.
set(findall(gcf,'type','text'),'FontSize',20)
set(findall(gcf,'type','axes'),'FontSize',20)
