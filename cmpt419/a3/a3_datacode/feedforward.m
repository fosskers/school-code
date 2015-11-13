function [A,Z] = feedforward(X,NN)
% Run example X through network in NN
% X is N-by-M
% NN is struct array containing layers of network
% Returns:
%  A, activations of nodes in network (cell array of activations at each layer)
%  Z, outputs at nodes (non-linear function applied to activations)

% Initialize outputs of previous layer as the inputs, plus bias inputs.
Zprev = [X ones(size(X,1),1)];
for l_i=1:length(NN)
  if size(Zprev,2)~=size(NN(l_i).weights,1)
    error('Mismatch in weight matrix size');
  end

  % Compute activations at this layer.
  A_this = Zprev*NN(l_i).weights;
  A{l_i} = A_this;

  if strcmp(NN(l_i).type,'sigmoid')
    Z{l_i} = 1./(1+exp(-A_this));
  elseif strcmp(NN(l_i).type,'softmax')
    % Compute safely: divide all by numerator, compute as 1/(sum (exp(a_j-a_k)))
    K = size(A_this,2);
    for k_i=1:K
      Z{l_i}(:,k_i) = 1./sum(exp(A_this - repmat(A_this(:,k_i),[1 K])),2);
    end
  else
    error('Unknown node type');
  end

  % Store previous activations, plus bias inputs.
  Zprev = [Z{l_i} ones(size(Z{l_i},1),1)];
end


