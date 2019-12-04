Program Secante
  implicit none
  Real::a,b,x,f,eps
  Integer:: n,i,error
  character(len=25)::nombre_archivo,error_msg

  Print*, "Este programa encontrara la soluion a la ecuacion F(x)=x^2-2 mediante el metodo de la Secante."
  Print*, "Para esto es necesario que me digas el numero máximo de iteraciones que deseas que puedo hacer"
  Read*, n
  Print*, "Al final guardare elresultado en un archivo de texto, ¿Que nombre deseas darle?"
  Read*, nombre_archivo
  Print *,"Introduce dos valores que seran las primeras iteraciones"
  Read *, a,b

  eps=0.000001
  
  open(11,file=nombre_archivo,status="Unknown",Action="Write",Iostat=error,Iomsg=error_msg)
  If(error==0)then
     write(11,*) "i             Xi-1                Xi               Xi+1           f(Xi-1)               f(Xi)             f(Xi+1)"
     do i=0,n
        x=b-(b-a)*f(b)/(f(b)-f(a))
        If (abs(x-b)<eps)then
           Print*, "La raiz se encontró en la iteracion",i
           Print*, "Abre el archivo",nombre_archivo,"para conocer el resultado"
           exit
        End If
        write(11,10)i,a,b,x,f(a),f(b),f(x)
        a=b
        b=x
     End Do

     If(i>n)then
        Print*,"No encontre ninguna raiz con las aproximaciones iniciales que introdujiste, por favor introduce otras diferentes "
     End If
  Else
     Print*, error, error_msg
  End if

   10 Format(I3,X,F18.2,X,F18.2,X,F18.2,X,F18.2,X,F18.2,X,F18.2) 
End Program Secante

Function f(x0)
  real::x0
  real::f
  
  f=x0**2-2
end Function F
