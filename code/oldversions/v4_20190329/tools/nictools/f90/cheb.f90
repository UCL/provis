subroutine cheb(x,n,c)
  implicit none
  real(8), intent(in)  :: x(:)
  integer, intent(in)  :: n
  real(8), intent(out) :: c(:,:)

  integer :: I1

  nx = size(x,1)
  c = 1.0d0
  c(:,2) = x

  do i1=3,n
    c(:,i1) = 2.0d0 * x * c(:,i1-1)-c(:,i1-2)
  end do
  c(:,1) = 0.5d0 * c(:,1)
end subroutine
