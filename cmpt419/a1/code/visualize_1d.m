% X is 1-d
% X_train, X_test, t_train, t_test should all be 1-d, and need to
% be  defined as well. You should modify y_ev.

[Countries, Features, Data] = loadUnicefData();

% Split into training and testing data
t = Data(:,2);

DEG = 3;

for i=11:13
    X = normalizeData(Data(:,i));

    ntrain = 100;
    X_train = X(1:ntrain);
    X_test = X(ntrain+1:end);
    t_train = t(1:ntrain);
    t_test = t(ntrain+1:end);

    % Plot a curve showing learned function.
    x_ev = (min(X):0.1:max(X))';

    Phi = designMatrix(X_train,'polynomial',DEG);
    w = pinv(Phi) * t_train;

    y_ev = designMatrix(x_ev,'polynomial',DEG) * w;

    figure;
    plot(x_ev,y_ev,'r.-');  
    hold on;
    plot(X_train,t_train,'g.');
    plot(X_test,t_test,'bo');
    hold off;
    title(sprintf('Feature %d Data and Fitted Curve',i));
    legend('Fitted Curve','Training','Testing','Location','northeast');
    % Make the fonts larger, good for reports.
    set(findall(gcf,'type','text'),'FontSize',20)
    set(findall(gcf,'type','axes'),'FontSize',20)
end

