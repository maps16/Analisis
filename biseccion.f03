Real Function f (x)
  Implicit none
  Real :: x
  f= Cos(x)-x
  !Funcion a evaluar
End Function f

Program Biseccion
  Implicit none
  Real :: f           !Funcion
  Real :: a, b, r     !intervalo y Posible valor de la raiz
  Real :: fa, fb, fr  !Evalucion de los intervalos
  Real *8 :: Er 
  Integer :: n, i        !iteraciones
  Write (*,*) "Valor de a"
  Read *, a
  Write (*,*) "Valor de b"
  Read *, b
  n=100
  fa=f(a)
  fb=f(b)
 
  If (fa*fb < 0) Then 
     Do i=1, n, 1
        r=(a+b)/2
        fr= f(r)
        
        If(fa*fr<0)Then 
        b=r
     Else 
        a=r
           
        End IF
     End Do
  End If

  Er= (b-a)/(2**n)




  write (*,*) "Valor es r ", r

  write (*,*) "Valor es r ", Er


End Program Biseccion
