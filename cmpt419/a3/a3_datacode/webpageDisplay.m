function webpageDisplay(X,inds,preds,labels)
% Produce a webpage to visualize classification output.
% Creates/overwrites output.html
%
% X: N-by-M-by-C array of C images (size N-by-M)
% inds: K-by-1 array of indices of images on which predictions are made
% preds: K-by-1 array of predictions
% labels: K-by-1 array of ground truth labels


out_file = 'output.html';
base_dir = 'images';

% Spit out images as jpegs.
dd = dir(base_dir);
if length(dd)==1 && ~dd(1).isdir
  error(sprintf('%s exists and is not a directory',basedir));
elseif length(dd)==0
  % Make image directory.
  mkdir(base_dir);
end
for im_i=inds
  imfile = sprintf('%s/img_%06d.jpg',base_dir,im_i);
  imwrite(X(:,:,im_i),imfile);
end


N = length(inds);

% Check for valid input.
assert(N == length(preds),'inds and preds have different numbers of examples');

% Produce simple webpage
fp = fopen(out_file,'w');
fprintf(fp,'<html>\n');
fprintf(fp,'<table>\n');

% Generate a table row for each image.
for e_i=1:N
  % Add a red background colour if incorrectly classified.
  corr_str = '';
  if preds(e_i) ~= labels(e_i)
    corr_str = 'style="background-color:red"';
  end
  fprintf(fp,'<tr>\n');
  fprintf(fp,'  <td><img src="%s/img_%06d.jpg" width=40></td>\n',base_dir,inds(e_i));
  fprintf(fp,'  <td %s>%d</td>\n',corr_str,preds(e_i));
  fprintf(fp,'</tr>\n');
end



fprintf(fp,'</table>\n');
fprintf(fp,'</html>\n');
fclose(fp);
