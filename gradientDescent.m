function [params] = gradientDescent(f, initial_parameters )
%GRADIENTDESCENTMULTI Performs gradient descent to learn theta
%   theta = GRADIENTDESCENTMULTI(x, y, theta, alpha, num_iters) updates theta by
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
num_iters = 100;
J_history = zeros(num_iters, 1);
alpha = 0.01;
params = initial_parameters;
for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCostMulti) and gradient here.
    %

  [j, grad] = f(params)
  params = params .- alpha * grad;


    % ============================================================

    % Save the cost J in every iteration    
  J_history(iter) = j;

end

end
