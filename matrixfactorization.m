fprintf('Loading movie ratings dataset.\n\n');
load ('ex8_movies.mat');
m = mean(Y(1, R(1, :)));
fprintf('mean of movie 1 is: %f / 5\n\n', m);
load ('ex8_movieParams.mat');

%  Useful Values
num_users = size(Y, 2);
num_movies = size(Y, 1);
num_features = 10;

% Set Initial Parameters (Theta, X)
X = randn(num_movies, num_features);
Theta = randn(num_users, num_features)
;
%  Reduce the data set size so that this runs faster
num_users = 4; num_movies = 5; num_features = 3;
X = X(1:num_movies, 1:num_features);
Theta = Theta(1:num_users, 1:num_features);
Y = Y(1:num_movies, 1:num_users);
R = R(1:num_movies, 1:num_users);

xbefore = X;
thetabefore = Theta;

[c1, g] = cofiCostFunc([xbefore ; thetabefore], Y, R, num_users, num_movies, num_features, lambda);
randomthetaandx = c1

initial_parameters = [X(:); Theta(:)];

% Set options for fmincg
options = optimset('GradObj', 'on', 'MaxIter', 100);

% Set Regularization
lambda = 10;
theta = fmincg (@(t)(cofiCostFunc(t, Y, R, num_users, num_movies, num_features, lambda)), initial_parameters, options);


theta2 = fminunc (@(t)(cofiCostFunc(t, Y, R, num_users, num_movies, num_features, lambda)), initial_parameters, options);

theta3 = gradientDescent(@(t)(cofiCostFunc(t, Y, R, num_users, num_movies, num_features, lambda)), initial_parameters);

X2 = reshape(theta(1:num_movies*num_features), num_movies, num_features);
Theta = reshape(theta(num_movies*num_features+1:end), num_users, num_features);

X = reshape(theta2(1:num_movies*num_features), num_movies, num_features);
Theta2 = reshape(theta2(num_movies*num_features+1:end), num_users, num_features);

size(theta3)
X3 = reshape(theta3(1:num_movies*num_features), num_movies, num_features);
Theta3 = reshape(theta3(num_movies*num_features+1:end), num_users, num_features);

[c1, g] = cofiCostFunc([X2(:) ; Theta2(:)], Y, R, num_users, num_movies, num_features, 1.5);
withfmincg = c1

[c2, g] = cofiCostFunc([X(:) ; Theta(:)], Y, R, num_users, num_movies, num_features, 1.5);

withfminunc = c2

[c3, g] = cofiCostFunc([X3(:) ; Theta3(:)], Y, R, num_users, num_movies, num_features, 1.5);

withgraddescent = c3
