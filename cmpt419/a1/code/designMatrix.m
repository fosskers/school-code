function Phi = designMatrix(X,basis,varargin)
% Phi = designMatrix(X,basis)
% Phi = designMatrix(X,'polynomial',degree)
% Phi = designMatrix(X,'sigmoid',Mu,s)
%
% Compute the design matrix for input data X
% X is n-by-d
% Mu is k-by-1

if strcmp(basis,'polynomial')
  k = varargin{1};
  % TO DO:: Fill in
  Phi = []
elseif strcmp(basis,'sigmoid')
  Mu = varargin{1};
  s = varargin{2};
  % TO DO:: Fill in
  Phi = []
else
  error('Unknown basis type');
end
