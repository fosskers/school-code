function plotMB(w,varargin)
% plotMB(w)
% plotMB(w,w_old)
%
% Plot the current separator in slope-intercept space (m-b)
% If two inputs are provided, draw a line between them.

m = -w(1)/w(2);
b = -w(3)/w(2);

if nargin>=2
  w_old = varargin{1};
  m_old = -w_old(1)/(w_old(2)+eps);
  b_old = -w_old(3)/(w_old(2)+eps);
  plot([m_old m],[b_old b],'ko-');
else
  plot(m,b,'ko');
end