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

  for d=1:deg
      PhiTemp = [];

      for i=1:size(X,1)
          for j=1:size(X,2)
              PhiTemp(i,j) = X(i,j) ^ d;
          end
      end

      Phi = [Phi PhiTemp];
  end
elseif strcmp(basis,'sigmoid')
  Mu = varargin{1};
  s = varargin{2};

  % Our data is guaranteed to have only 1 column.
  for i=1:size(X,1)
      Phi(i,1) = 1;  % Bias term
      Phi(i,2) = 1 / (1 + exp((Mu(1) - X(i,1)) / s));
      Phi(i,3) = 1 / (1 + exp((Mu(2) - X(i,1)) / s));
  end
else
  error('Unknown basis type');
end
