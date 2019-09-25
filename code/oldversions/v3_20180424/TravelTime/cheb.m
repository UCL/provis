function [C,DC,D2C]=cheb(x,n,nodes) %#codegen
% [C,DC,D2C]=cheb(x,n,nodes)
%
% Computes matrix of first n or n+1 Chebyshev polynomials
%  and their 1st derivatives
% C	= (n x N) [T0(x');T1(x');T2(x');...]
%	  (n+1 x N+1) if nodes==1
%     C(i,:) = i'th polynomial evaluated at x
%     C(:,j) = all polynomials evaluated at x(j)
% DC = (n x N)  DC(i,j) = DC_i(x(j)) / dx
% D2C = (n x N) second derivative matrix
% x	= (N x 1) vector of nodes in [-1,1]
% n	= (1 x 1) number of polynomials to calculate
% N	= (1 x 1) number of nodes
% nodes = 1 if using Lobatto-Chebyshev nodes
%	  0 if using Gauss-Chebyshev nodes
%
% Revision history
% 30apr2013  LN  tested gradient and found it is accurate.
N=length(x);
if nodes==0
  C=ones(n,N);

  C(2,:)=x.';
  if nargout>1
    DC = zeros(n,N);
    DC(2,:) = 1;
    if nargout==3
      D2C = zeros(n,N);    
    end
  end

  for j=3:n
    C(j,:)=2*x.'.*C(j-1,:)-C(j-2,:);
    if nargout>1
      DC(j,:) = 2*C(j-1,:)+2*x.'.*DC(j-1,:)-DC(j-2,:);
      if nargout==3
        D2C(j,:) = 4*DC(j-1,:) + 2*(x.').*D2C(j-1,:) - D2C(j-2,:);    
      end
    end
    
  end
  C(1,:)=0.5*C(1,:);
else
  C=ones(n+1,N);
  C(2,:)=x.';
  if nargout>1
    DC = zeros(n+1,N);
    DC(2,:) = 1;
    if nargout==3
      D2C = zeros(n+1,N);
    end
  end
  for j=3:n+1
    C(j,:)=2*x.'.*C(j-1,:)-C(j-2,:);
    if nargout>1
      DC(j,:) = 2*C(j-1,:)+2*x.'.*DC(j-1,:)-DC(j-2,:);
      if nargout==3
        D2C(j,:) = 4*DC(j-1,:)+2*(x.').*D2C(j-1,:)-D2C(j-2,:);    
      end
    end
  end
  i0=[1;n+1];
  C(i0,:)=0.5*C(i0,:);
  if nargout>1
    DC(i0,:) = 0.5*DC(i0,:);
    if nargout==3
      D2C(i0,:) = 0.5*D2C(i0,:);
    end
  end
end
