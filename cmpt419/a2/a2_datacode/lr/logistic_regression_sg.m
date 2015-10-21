% logistic_regression.m


% Maximum number of iterations.  Continue until this limit, or when error change is below tol.
max_iter=500;
tol = 0.0001;

% Step size for gradient descent.
eta = 0.003;

% Wait for user when drawing plots.
wait_user = false;


% Get X1, X2
load('data.mat');

% Data matrix, with column of ones at end.
X = [X1; X2];
X = [X ones(size(X,1),1)];
% Target values, 0 for class 1 (datapoints X1), 1 for class 2 (datapoints X2).
t = [zeros(size(X1,1),1); ones(size(X2,1),1)];

% Initialize w.
w = [0.1 0 0]';

% Error values over all iterations.
e_all = [];

% Set up the slope-intercept figure
figure(2);
clf;
set(gca,'FontSize',15);
title('Separator in slope-intercept space');
xlabel('slope');
ylabel('intercept');
axis([-5 5 -10 0]);
axis equal;
axis manual;


for iter=1:max_iter
  % Compute output using current w on all data X.
  y = sigmoid(w'*X')';
  
  % e is the error, negative log-likelihood (Eqn 4.90)
  e = -sum(t.*log(y) + (1-t).*log(1-y));

  % Add this error to the end of error vector.
  e_all(end+1) = e;
  
  % Gradient of the error, using Eqn 4.91
  grad_e = sum(repmat(y - t,[1 size(X,2)]) .* X, 1);
  
  % Update w, *subtracting* a step in the error derivative since we're minimizing
  w_old = w;
  w = w - eta*grad_e';
  
  % Plot current separator and data.
  figure(1);
  set(gca,'FontSize',15);
  plot(X1(:,1),X1(:,2),'g.');
  hold on;
  plot(X2(:,1),X2(:,2),'b.');
  drawSep(w);
  hold off;
  title('Separator in data space');
  axis([-5 15 -10 10]);
  axis equal;
  axis manual;
  drawnow;

  
  % Add next step of separator in m-b space.
  figure(2);
  hold on;
  plotMB(w,w_old);
  hold off;
  
  % Print some information.
  fprintf('iter %d, negative log-likelihood %.4f, w=', iter, e);
  fprintf('%.2f ',w);
  if wait_user
    % Wait for user input.
    input('Press enter');
  else
    fprintf('\n');
  end
  
  
  % Stop iterating if error doesn't change more than tol.
  if iter>1
    if abs(e-e_all(iter-1))<tol
      break;
    end
  end
end

% Plot error over iterations
figure(3)
set(gca,'FontSize',15);
plot(e_all,'b-');
xlabel('Iteration');
ylabel('neg. log likelihood')
title('Minimization using gradient descent');


