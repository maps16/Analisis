Real Function f (x)
  Implicit none
  Real :: x
  f= Cos(x)-x
  !Funcion a evaluar
End Function f

Program Biseccion
  Implicit none
  Real :: f           !Funcion
  Real :: a, b, r, x     !intervalo y Posible valor de la raiz x=2
  Real :: fa, fb, fr  !Evalucion de los intervalos
  Real :: Er, k !Numero de iteraciones
  Integer :: n, i        !iteraciones
 
  Write (*,*) "Valor de a"
  Read *, a
  Write (*,*) "Valor de b"
  Read *, b
 
  fa=f(a)
  fb=f(b)
  k= log((b-a)/0.0001)/log(2.00)
  Write (*,*) "Se realizaran alrededor de  ", int(k)+1 , "iteraciones"
  If (fa*fb < 0) Then 
     Do while (k>0)
        r=(a+b)/2
        fr= f(r)
        
        If(fa*fr<0)Then 
           b=r
!           fb=fr
           k=k-1
        Else 
           a=r
!           fa=fr
           k= k-1
        End IF
  
     End Do
  End If

 ! Er= (b-a)/(2**n)




  write (*,*) "Valor es r ", r, " con un error de 0.0001"

 ! write (*,*) "Valor es r ", Er


End Program Biseccion
