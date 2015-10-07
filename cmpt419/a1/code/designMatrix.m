function Phi = designMatrix(X,basis,varargin)
% Phi = designMatrix(X,basis)
% Phi = designMatrix(X,'polynomial',degree)
% Phi = designMatrix(X,'sigmoid',Mu,s)
%
% Compute the design matrix for input data X
% X is n-by-d
% Mu is k-by-1

if strcmp(basis,'polynomial')
  deg = varargin{1};

  % Initialize as a column vector of 1s. These are the x_0 values
  % given to `phi0 = const 1`.
  Phi(1:size(X,1),1) = 1;
  
  for i=1:size(X,1)
      for j=1:size(X,2)
          Phi(i,j+1) = X(i,j) ^ deg;
      end
  end
elseif strcmp(basis,'sigmoid')
  Mu = varargin{1};
  s = varargin{2};
  % TO DO:: Fill in
  Phi = []
else
  error('Unknown basis type');
end
