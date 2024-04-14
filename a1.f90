! Programa que cálcula a area de um triângulo
program aula1
    implicit none
    real::base, altura, area
    print *, "Cálculo de areo do triângulo"
    print *,"Informe o valor da base do triângulo:"
    read(*,*) base

    print *,"Informe o valor da altura do triângulo:"
    read(*,*) altura

    area = (base*altura)/2.0

    print *, "O valor da área é: ", area

end program aula1