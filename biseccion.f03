Real Function f (x)
  Implicit none
  Real :: x
  f= x**10 -1
  !Funcion a evaluar
End Function f

Program Biseccion
  Implicit none
  Real :: f           !Funcion
  Real :: a, b, r, x     !intervalo y Posible valor de la raiz x=2
  Real :: fa, fb, fr  !Evalucion de los intervalos
  Real :: Er, k !Numero de iteraciones
  Integer :: n, i        !iteraciones
  
  
  Write (*,*) "Inserte un intervalo [a,b] donde b>a."
  Write (*,*) "Valor de a"
  Read *, a
  Write (*,*) "Valor de b"
  Read *, b
 
  fa=f(a)
  fb=f(b)
!  k= log((b-a)/0.0001)/log(2.00)
!  Write (*,*) fa, " | ", fb       !!!Debug!!!
  If (fa*fb < 0) Then 
     
     k= log((b-a)/0.0001)/log(2.00)
     Write (*,*) "Se realizaran alrededor de  ", int(k)+1 , "iteraciones"
     
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
     
     write (*,*) "Valor de r  es ", r, " con un error aproximado de 0.0001"
  
  Else  
  	Write (*,*) "No se puede realizar por fallar en alguna condición." ! ya que o es la raiz o no se encuentra el intervalo dado o es una raiz compleja."
  End If

 ! Er= (b-a)/(2**n)
 ! write (*,*) "Valor de r  es ", r, " con un error aproximado de 0.0001"
 ! write (*,*) "Valor es r ", Er

End Program Biseccion
