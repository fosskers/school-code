% Train a neural network.
% Colin Woodbury - 301238755

% Clear the workbench.
clear;

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
% COLIN: Is there a mistake here?
TEST_INDS=setdiff(1:size(Xt,1),TRAIN_INDS);
TEST_INDS=501:1000;

% Training data
Xtrain=Xt(TRAIN_INDS,:);
ttrain=t(TRAIN_INDS);
[N D] = size(Xtrain);

% Testing data
Xtest=Xt(TEST_INDS,:);
ttest=t(TEST_INDS);

% Create neural network data structure.
% Simple version, have weight vector per node, all nodes in a layer
% are same type.
% NN(i).weights is a matrix of weights, each row corresponds to the
% weights for a node at the next layer.
% Note that bias term is added at the end.
%  I.e. a_k = NN(i).weights(:,k)' * z, where z is the vector of
% node outputs at the preceding layer.
H = 500;  % Number of hidden nodes.
clear NN;
NN = struct('weights',[],'type','');

NN(1).weights = randn(D+1,H);
%NN(1).weights = rand(D+1,H);
%NN(1).weights(1:D+1,1:H) = 0.5;
NN(1).type = 'sigmoid';

NN(2).weights = 0.1*randn(H+1,K);
%NN(2).weights = zeros(H+1,K);
NN(2).type = 'softmax';

% Stochastic gradient descent with back-propagation.
% Training/testing set accuracy
tra_all=[];
tea_all=[];
for epoch=1:MAX_EPOCH
    fprintf('Training neural network epoch %d/%d: ', epoch, MAX_EPOCH);
    tt = clock;

    for x_i=1:N
        %        fprintf('EPOCH = %d, x_i = %d\n',epoch,x_i);
        % A, activations of nodes in network (cell array of
        % activations at each layer)
        % Z, outputs at nodes (non-linear function applied to activations)
        [A,Z] = feedforward(Xtrain(x_i,:),NN);
    
        % Note! Learned from Youtube: derivative of a y_i in the
        % softmax function is: (dy_i/dz_i) = y_i * (1 - y_i). Softmax
        % produces y's based on a special summation of all the input
        % z's in that layer. The sum of the y's is 1.
    
        % Output layer derivative.
        % Assume classification with softmax.
        % Note: code for multiple hidden layers should use a for
        % loop, but the first/last layers are special cases, which
        % is all we have here.
        %
        % (y_k - t_k) * z_j, where j is the previous layer before k.
        dW2 = zeros(H+1,K);
        the_t = zeros(K,1);
        the_t(ttrain(x_i),1) = 1;  % 1-hot encoding.
        z = Z{1};
        z = [z 1];
        dks = [];

        for k=1:size(Z{2},2)
            y_k = Z{2}(1,k);
            t_k = the_t(k,1);
            dks(k,1) = y_k - t_k;
            dW2(:,k) = (dks(k,1) * z)';  % append gradients for this `k`.
        end

        % Hidden layer derivative.
        % Backpropagate error from output layer to hidden layer.
        dW1 = zeros(D+1,H);
        xs = [Xtrain(x_i,:) 1]';

        for j=1:size(Z{1},2)
            dprod = NN(2).weights(j,:) * dks;
            dj = z(1,j) * (1 - z(1,j)) * dprod;
            dW1(:,j) = dj * xs;
        end

        % Stochastic Gradient Descent
        NN(2).weights = NN(2).weights - ETA*dW2;
        NN(1).weights = NN(1).weights - ETA*dW1;
    end
    
    tra_all(epoch) = computeAcc(Xtrain,NN,ttrain);
    tea_all(epoch) = computeAcc(Xtest,NN,ttest);
    fprintf('training accuracy = %.4f, took %.2f seconds\n', ...
            tra_all(epoch), etime(clock,tt));
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
