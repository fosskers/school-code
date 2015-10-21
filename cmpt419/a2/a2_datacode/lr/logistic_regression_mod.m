% logistic_regression_mod.m
% Colin Woodbury - 301238755

% Clear workspace.
clear;

% Maximum number of iterations.  Continue until this limit, or when error change is below tol.
max_iter=500;
tol = 0.0001;

% Step sizes for gradient descent.
Eta = [0.005 0.003 0.001 0.0005 0.0001];

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

for e_inx=1:size(Eta,2)
    % Initialize w.
    w = [0.1 0 0]';

    % Current eta value.
    eta = Eta(e_inx);

    for iter=1:max_iter
        % Compute output using current w on all data X.
        y = sigmoid(w'*X')';
  
        % e is the error, negative log-likelihood (Eqn 4.90)
        e = -sum(t.*log(y) + (1-t).*log(1-y));

        % Add this error to the end of error vector.
        e_all(e_inx,iter) = e;
  
        % Gradient of the error, using Eqn 4.91
        grad_e = sum(repmat(y - t,[1 size(X,2)]) .* X, 1);
  
        % Update w, *subtracting* a step in the error derivative since
        % we're minimizing
        w_old = w;
        w = w - eta*grad_e';
  
        % Print some information.
        %fprintf('iter %d, negative log-likelihood %.4f, w=', iter, e);
        %fprintf('%.2f ',w);
        if wait_user
            % Wait for user input.
            input('Press enter');
        else
            fprintf('\n');
        end
  
        % Stop iterating if error doesn't change more than tol.
        if iter>1
            if abs(e-e_all(e_inx,iter-1))<tol
                break;
            end
        end
    end
end

size(e_all)

% Plot error over iterations
figure(3)
set(gca,'FontSize',15);
plot(e_all');
xlabel('Iteration');
ylabel('neg. log likelihood')
title('Minimization using gradient descent');
legend('mu=0.005','mu=0.003','mu=0.001','mu=0.0005','mu=0.0001');
