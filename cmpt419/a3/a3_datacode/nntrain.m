% 
% Train a neural network.
%

% Load data.
% Loads X (28x28x10000): images, t (10000x1): labels
% Note that original labels are integers in 0-9.
load digits10000

K = 10;  % Number of classes.
ETA = 0.1; % Step size for stochastic gradient descent.
MAX_EPOCH = 10; % Maximum number of iterations through the training data.

% Transform digits to 10000x784, remove spatial structure.
Xt = transformDigits(X);
t = t+1; % Encode as 1-10, digit+1.

% Set up training and testing sets.
TRAIN_INDS=1:500;
TEST_INDS=setdiff(1:size(Xt,1),TRAIN_INDS);
TEST_INDS=501:1000;

Xtest=Xt(TEST_INDS,:);
ttest=t(TEST_INDS);

Xtrain=Xt(TRAIN_INDS,:);
ttrain=t(TRAIN_INDS);
[N D] = size(Xtrain);

% Create neural network data structure.
% Simple version, have weight vector per node, all nodes in a layer are same type.
% NN(i).weights is a matrix of weights, each row corresponds to the weights for a node at the next layer.
% Note that bias term is added at the end.
%  I.e. a_k = NN(i).weights(:,k)' * z, where z is the vector of node outputs at the preceding layer.
H = 500;  % Number of hidden nodes.
clear NN;
NN = struct('weights',[],'type','');

NN(1).weights = randn(D+1,H);
%NN(1).weights = rand(D+1,H);
NN(1).type = 'sigmoid';

NN(2).weights = 0.1*randn(H+1,K);
NN(2).type = 'softmax';



% Stochastic gradient descent with back-propagation.
% Training/testing set accuracy
tra_all=[];
tea_all=[];
for epoch=1:MAX_EPOCH
  fprintf('Training neural network epoch %d/%d: ', epoch, MAX_EPOCH);
  tt = clock;
  for x_i=1:N
    [A,Z] = feedforward(Xtrain(x_i,:),NN);

    % Output layer derivative.
    % Assume classification with softmax.
    % Note: code for multiple hidden layers should use a for loop, but the first/last layers are special cases, which is all we have here.
    % TO DO:: fill this in.
    dW2 = zeros(H+1,K);


    % Hidden layer derivative.
    % Backpropagate error from output layer to hidden layer.
    % TO DO:: fill this in.
    dW1 = zeros(D+1,H);


    % Apply the computed gradients in a stochastic gradient descent update.
    NN(2).weights = NN(2).weights - ETA*dW2;
    NN(1).weights = NN(1).weights - ETA*dW1;
  end

  tra_all(epoch) = computeAcc(Xtrain,NN,ttrain);
  tea_all(epoch) = computeAcc(Xtest,NN,ttest);
  fprintf('training accuracy = %.4f, took %.2f seconds\n',tra_all(epoch),etime(clock,tt));

end
fprintf('Final test accuracy = %.4f.\n',tea_all(end));


% Set up a figure for plotting training error.
figure(1);
clf;
plot(tra_all,'bo-');
hold on;
plot(tea_all,'ro-');
hold off;
xlabel('Epoch');
ylabel('Classification accuracy')
title('Training neural network with backpropagation');
legend('Training set','Test set');
axis([1 MAX_EPOCH 0 1])
set(findall(gcf,'type','text'),'FontSize',20)
set(findall(gcf,'type','axes'),'FontSize',20)


% Produce webpage showing predictions.
fprintf('Producing webpage of results... ');
% Get predictions
[A,Z] = feedforward(Xtest,NN);
% Take max over output layer to get predictions.
[mvals,preds] = max(Z{end},[],2);

% -1 to convert back to actual digits.
webpageDisplay(X,TEST_INDS,preds-1,ttest-1);
fprintf('done.\n  TRY OPENING output.html\n');
