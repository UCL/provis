function [b,yhat,b2,yhat2,lo,hi] = TravelModel1(outcome,x1,y1,x2,y2,k,lambda) %#codegen
% compute b = (X'*X) \ (X'*outcome)
% origin      = (x1,y1)
% destination = (x2,y2)
%
% chebyshev polynomials
if nargin<6
  k = [17;5];
end
if nargin<7
    lambda = 0.001;
end
longitude_buffer = 0.1;
latitude_buffer = 0.05;

lo=zeros(4,1);
hi=zeros(4,1);

lo(1)=min(x1)-longitude_buffer;
hi(1)=max(x1)+longitude_buffer;
x = 2*(x1 - lo(1))/(hi(1)-lo(1))-1;
b1 = cheb(x,k(1),0);
  
lo(2)=min(y1)-latitude_buffer;
hi(2)=max(y1)+latitude_buffer;
x = 2*(y1-lo(2))/(hi(2)-lo(2))-1;
b2 = cheb(x,k(1),0);
 
lo(3)=min(x2)-longitude_buffer;
hi(3)=max(x2)+longitude_buffer;
x = 2*(x2-lo(3))/(hi(3)-lo(3))-1;
b3 = cheb(x,k(1),0);

lo(4)=min(y2)-latitude_buffer;
hi(4)=max(y2)+latitude_buffer;
x = 2*(y2-lo(4))/(hi(4)-lo(4))-1;
b4 = cheb(x,k(1),0); 

X = kron(b1.',ones(1,k(2)^3));
X = X .* ...
    kron(ones(1,k(2)),kron(b2.',ones(1,k(2)^2)));
X = X .* ...
    kron(ones(1,k(2)^2),kron(b3.',ones(1,k(2))));
X = X .* ...
      kron(ones(1,k(2)^3),b4.');

b = (X'*X) \ (X'*outcome);
yhat = X*b;

kx = size(X,2);
b2 = (X'*X + lambda*eye(kx)) \ (X'*outcome);
yhat2 = X*b2;
%[b2,FitInfo] = lassoglm(X,outcome,'normal');
