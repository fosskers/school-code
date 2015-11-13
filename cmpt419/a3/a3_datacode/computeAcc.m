function e = computeAcc(X,NN,t)
% Compute classification accuracy for network NN on data X with targets t.
[A,Z] = feedforward(X,NN);

% Take max over output layer, compare to t.
[mvals,minds] = max(Z{end},[],2);
e = length(find(minds == t)) / length(t);
