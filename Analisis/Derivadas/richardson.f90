!!El objetivo de este programa es el de aproximar la derivada de una función
!!con el método de Richardson.
!!---------------------------------------------------------------------------

!Declaramos las variables
PROGRAM richardson
  IMPLICIT NONE
  REAL(8)::phi,f,x,h,punto        !!Funciones y valores a utilizar.
  INTEGER::n,m                    !!Contadores.
  REAL(8),DIMENSION(0:10,0:10)::D !!Matriz 10x10 en la cual almacenamos los resultados.

  PRINT*,"Hola, bienvenido al programa de Richardson, donde calcularemos la"
  PRINT*,"derivada de la función 2x2-4x+6 en el punto x=3."

  !Utilizamos el método de Richardson para aproximar la derivada.
  h=0.5
  punto=3.
  DO n=0,10
     D(n,0)=phi(h/(2**n),punto)
  DO m=1,10 
     D(n,m)=(4**m)/(4**m-1)*D(n,m-1)-1/(4**m-1)*D(n-1,m-1)
  END DO
END DO

  !Imprimimos la aproximación.
  PRINT*,D(10,10)
END PROGRAM richardson

!Declaramos la función a la cual aproximaremos la variable.
FUNCTION f(x)
  IMPLICIT NONE
  REAL(8),INTENT(IN)::x
  REAL(8)::f
  f=(2*x**2)-(4*x)-6
END FUNCTION f

!Declaramos una función del método de Diferencias Divididas Centradas
!para aproximar la derivada.
FUNCTION phi(h,x)
  IMPLICIT NONE
  REAL(8),INTENT(IN)::h,x
  REAL(8)::phi,f
  phi=(f(x+h)-f(x-h))/(2*h)
END FUNCTION phi

