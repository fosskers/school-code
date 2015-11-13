function Y = transformDigits(X)
% Transform X, a U-by-V-by-N representation of N U-by-V images into a flat vector form.
% Y contains elements of X, but in a N-by-UV shape.
Y = reshape(X, [size(X,1)*size(X,2) size(X,3)]);
Y = Y';
