% do_cv.m
% Colin Woodbury - 301238755

% Clear workspace
clear;

MAX_EPOCH=100;
ETA=0.00001;
NORMALIZE=0;

% Positive class
POS=1;

% Load data.
% Contains `Ftrain` (3000x1373) and `Ltrain` (3000x1)
load('email.mat');

% Number of training examples
Ntrain = size(Ftrain,1);

% Class POS versus the rest.
% This sets up L as +/-1 for the two classes.
% This is the vector of `t` values.
L = (Ltrain == POS) - (Ltrain~= POS);

% Cross Validation offset base
BASE = 300;

% Kernel parameters
K_TYPE = 'gaussian';
K_C = 0;
%K_PARAMS = {1/Ntrain, K_C};
K_PARAMS = {15};

% Running total of validation error
e_all = 0;

for i=1:(Ntrain/BASE)
    fprintf('Training fold %02d... ',i);

    [training, validation, trTa, vaTa] = crossValidation(Ftrain,L, ...
                                                      i-1,BASE);
    % Train kernel perceptron
    % Compute gram matrix
    K = gramMatrix(training,training,K_TYPE,K_PARAMS);
    
    % Run stochastic gradient descent
    % This is a column vector
    Alpha = zeros(size(training,1),1);

    for epoch=1:MAX_EPOCH
        rp = randperm(size(training,1));

        % Train Alpha on the data in a random order.
        for n_i=rp
            % Evaluate perceptron.
            y = sign(Alpha' * K(:,n_i));

            if y == 0
                y = 1;
            end
            
            if y * trTa(n_i) < 0
                Alpha(n_i) = Alpha(n_i) + ETA * trTa(n_i);
            end
        end
    end

    % Training error
    Fn = sign((Alpha') * K)';
    tr_nerr = sum(Fn ~= trTa);

    % Test error
    K_t = gramMatrix(training,validation,K_TYPE,K_PARAMS);
    Fn = sign((Alpha') * K_t)';
    va_nerr = sum(Fn ~= vaTa);

    e_all = e_all + va_nerr;

    fprintf('train error %02d | val error %02d\n', tr_nerr, va_nerr);
end

avg_e = e_all / (Ntrain/BASE);

fprintf('Average validation error: %f (%.3f%%)\n', avg_e, 100 * avg_e / BASE);