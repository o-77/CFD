program lxf 
    implicit none
   integer::N=100
   real(kind=8)::T=1.0
   real(kind=8)::array_u(1000,1000),exact_u(1000),x(1000)
   real(kind=8)::h,k,pi,cfl=1.0
   integer::i,j,m
   character(len=50)::filename = "D:\mycode\lxf_1\result.dat"

   pi=acos(-1.0d0)
   h=1.0d0/N 
   k=cfl*h/2.0d0
   do j=1,N+1 !缁屾椽妫跨粋缁樻殠
       x(j) = h*(j-1)
       array_u(1,j) = sin(2.0d0*pi*(x(j)))
       exact_u(j) = sin(2.0d0*pi*(x(j)-2.0d0*T))
   end do


   !LxF scheme
   m=T/k
   do i=1,m !閺冨爼妫跨粋缁樻殠
       do j=2,n
        array_u(i+1,j) = (1.0d0/2.0d0)*(1.0d0-2.0d0*k/h)*array_u(i,j+1)+(1.0d0/2.0d0)*(1.0d0+2.0d0*k/h)*array_u(i,j-1)
       end do
       array_u(i+1,1) = (1.0d0/2.0d0)*(1.0d0-2.0d0*k/h)*array_u(i,2)+(1.0d0/2.0d0)*(1.0d0+2.0d0*k/h)*array_u(i,n)
       array_u(i+1,n+1) = (1.0d0/2.0d0)*(1.0d0-2.0d0*k/h)*array_u(i,2)+(1.0d0/2.0d0)*(1.0d0+2.0d0*k/h)*array_u(i,n)
   end do

  open(unit=10,file=filename)
  write(10,*) "variables=x,au,eu"
   do j=1,n+1
   write(10,*) x(j),array_u(m+1,j),exact_u(j)
   end do
   close(10)
   
end program