function drawSep(w)
% drawSep(w)
% Draw a hyperplane w'x
%
% w is [w_1 w_2 bias]'
L = 1000;

% Check for degeneracy, at least one of w(1:2) must be large enough to invert
if abs(w(2)) > eps
  x1 = [-L L];
  x2 = (-[w(3) w(3)] - w(1).*x1)/w(2);
elseif abs(w(1)) > eps
  x2 = [-L L];
  x1 = (-[w(3) w(3)] - w(2).*x2)/w(1);
else
  error('Invalid separator');
end

plot(x1,x2,'r-');

