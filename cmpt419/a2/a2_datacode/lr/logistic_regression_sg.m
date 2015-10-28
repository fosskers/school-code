% logistic_regression_sg.m
% Colin Woodbury - 301238755

% Clear workspace.
clear;

% Maximum number of iterations.  Continue until this limit, or when error change is below tol.
max_iter=500;
tol = 0.0001;

% Step sizes for gradient descent.
Eta = [0.1 0.05 0.03 0.02 0.01 0.001 0.0001];
%Eta = [ 0.03 ];

% Wait for user when drawing plots.
wait_user = false;

% Get X1, X2
load('data.mat');

% Data matrix, with column of ones at end.
X = [X1; X2];
X = [X ones(size(X,1),1)];
% Target values, 0 for class 1 (datapoints X1), 1 for class 2 (datapoints X2).
t = [zeros(size(X1,1),1); ones(size(X2,1),1)];

% Error values over all iterations.
e_all = [];

% Note: Both `X` and `t` have 200 rows.

% The order in which to check the training data.
indexes = randperm(size(X,1));

% --- ITERATE OVER ALL ETA VALUES --- %
for e_inx=1:size(Eta,2)
    % Initialize w.
    w = [0.1 0 0]';

    % Current eta value.
    eta = Eta(e_inx);
      
    % --- ITERATE OVER EPOCHS --- %
    for iter=1:max_iter
        % Total error for this epoch
        total_e = 0;
        
        % --- ITERATE RANDOMLY OVER TRAINING DATA --- %
        for r_inx=indexes        
            % Compute output using current w on all data X.
            y = sigmoid(w'*X(r_inx,:)')';

            % e is the error, negative log-likelihood (Eqn 4.90)
            e = -sum(t(r_inx) * log(y) + (1-t(r_inx)) * log(1-y));
            
            total_e = total_e + e;
            
            % Gradient of the error, using Eqn 4.91
            % grad_e = sum(repmat(y - t,[1 size(X,2)]) .* X, 1);
            grad_e = (y - t(r_inx)) * X(r_inx,:);

            % Update w, *subtracting* a step in the error derivative since
            % we're minimizing
            w_old = w;
            w = w - eta*grad_e';
        end

        %{
        fprintf('Epoch %d, negative log-likelihood %.4f, w=', ...
                iter, total_e);
        fprintf('%.2f ',w);
        fprintf('\n');
        %}
        
        % Add this error to the end of error vector.
        e_all(e_inx,iter) = total_e;

        % Stop iterating if error doesn't change more than tol.
        if iter > 1
            if abs(total_e - e_all(e_inx,iter-1)) < tol
                fprintf('eta=%.4f converged at epoch %d\n', ...
                        eta, iter);
                break;
            end
        end
    end
end

% Plot error over iterations
figure(3)
set(gca,'FontSize',15);
plot(e_all');
xlabel('Epoch');
ylabel('neg. log likelihood')
title('Minimization using Stochastic Gradient Descent');
legend('eta=0.1','eta=0.05','eta=0.03','eta=0.02','eta=0.01', 'eta=0.001', ...
       'eta=0.0001');
