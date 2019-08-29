Program Biseccion
  implicit none
  Real::a,b,x,f,eps
  Integer:: n,i

  Print*, "Este programa encontrara la soluion a la ecuacion F(x)=x^2-2 mediante el metodo del biseccion."
  do
     print *,"Introduce el intervalo a y b donde crees que se encuentra la solucion"
     read *, a,b
     if(f(a)*f(b)<0) exit
  end do
  
  eps=0.000001
  n=log((b-a)/eps)/log(2.)+1
  Print*, "Para esta ecuacion son necesarios",n,"pasos para encontrar la raiz"
  print *, " i   a   xi   b    f(a)     f(xi)    f(b)"
  do i=1,n
     x=(a+b)/2
     if(f(a)*f(x)<0)then
        b=x
     else
        a=x
     end if
     print *,i,a,x,b,f(a),f(x),f(b) 
  end do
  
End Program Biseccion

Function f(x0)
  real::x0
  real::f
  
  f=x0**2-2
end Function F
