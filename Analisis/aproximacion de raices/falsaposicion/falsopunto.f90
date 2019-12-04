!!El objetivo de este programa es el de utilizar el método de falso punto  para
!!aproximar las raìces de una función cuadrática.
!!--------------------------------------------------------------------------

!Declaración de variables
PROGRAM falsopunto
  IMPLICIT NONE
  REAL::a,b,c         !!Limites del intervalo  [a,b ] donde calcularemos la raíz.
  REAL::f,x           !!Función a evaluar (f) junto su argumento (x).
  REAL::pm            !!Punto medio.
  REAL::tol           !!Tolerancia del error.
  REAL::aprox         !!Aproximación resultante.
  REAL::sig           !!Contador para verificar el cambio de signo.
  INTEGER::i          !!Contador.


  PRINT*,"Hola, bienvenido al programa del falso punto , donde se calculará"
  PRINT*,"las raíces de la función cuadrática  2x2 - 4x - 6."


  !Pedimos al usuario ingresar su intervalo.
  PRINT*,"Por favor, ingresa el intervalo donde quieres aproximar la raíz:"
  PRINT*,"VALOR INICIAL:"
  READ*,a
  PRINT*,"VALOR FINAL:"
  READ*,b

  !Ajustamos el intervalo.
  ajust:IF(b<a)THEN
     c=b
     b=a
     a=c
  ELSE
  END IF ajust

  !Verificamos si existen raíces en el intervalo y calculamos el punto medio.
  sig=(f(a)/abs(f(a)))*(f(b)/abs(f(b)))
  raices:IF(sig>0)THEN
     PRINT*,"Lo sentimos, pero en ese intervalo no existe ningúna raíz."
     PRINT*,"Por favor, ingresa un nuevo intervalo."
  ELSE IF(sig<0)THEN
     PRINT*,"Se aproximarán las raíces en el intervalo:"
     PRINT*,"[",a,",",b,"]"
     !Definimos la tolerancia del error:
     PRINT*,"Para saber que tan exacto deseas aproximar tu raíz, define la tolerancia:"
     READ*,tol
     !Definimos el loop donde se calculará la raíz.
     i=0
     aprox = a - (f(a)*(b-a))/(f(b)-f(a))
     puntom:DO WHILE(abs(f(aprox))>tol)     !El loop se detendrá cuando la función evaluada en la función sea una solución.
     checar:IF((f(a)*f(b))<0)THEN
        pm= a - (f(a)*(b-a))/(f(b)-f(a))
        IF((f(a)*f(pm))<0)THEN
           b=pm
           aprox=pm
        ELSE IF((f(b)*f(pm))<0)THEN
           a=pm
           aprox=pm

        ELSE IF(f(b) == 0)THEN
           aprox=b
           EXIT
        ELSE IF(f(a) == 0)THEN
           aprox=a
           EXIT
        ELSE if(.NOT. (f(b)*f(pm))<0)THEN
           EXIT
        END IF
     END IF checar
  END DO puntom
END IF raices


PRINT*,"Tu raíz es, aproximadamente:",aprox


END PROGRAM falsopunto


!Declaramos la función a calcular.
FUNCTION f(x)
IMPLICIT NONE
REAL,INTENT(IN):: x
REAL::f
f=(2*x**2)-(4*x)-6
END FUNCTION f

