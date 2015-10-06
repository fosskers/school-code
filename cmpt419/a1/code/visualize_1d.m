% X is 1-d
% X_train, X_test, t_train, t_test should all be 1-d, and need to be defined as well.
% You should modify y_ev

[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);
X = Data(:,12);

ntrain = 100;
X_train = X(1:ntrain);
X_test = X(ntrain+1:end);
t_train = t(1:ntrain);
t_test = t(ntrain+1:end);


% Plot a curve showing learned function.
x_ev = (min(X):0.1:max(X))';

% TO DO:: Put your regression estimate here.
y_ev = 100*sin(x_ev);

figure;
plot(x_ev,y_ev,'r.-');  
hold on;
plot(X_train,t_train,'g.');
plot(X_test,t_test,'bo');
hold off;
title(sprintf('Fit with degree %d polynomial',5));
% Make the fonts larger, good for reports.
set(findall(gcf,'type','text'),'FontSize',20)
set(findall(gcf,'type','axes'),'FontSize',20)
