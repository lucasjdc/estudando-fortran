program monte_carlo_pi
    implicit none
    integer, parameter :: num_samples = 1000000
    real :: x, y
    integer :: inside_circle, i
    real :: pi_estimate

    ! Inicializa o contador de pontos dentro do círculo
    inside_circle = 0

    ! Gera pontos aleatórios e verifica se estão dentro do círculo
    do i = 1, num_samples
        ! Gera coordenadas aleatórias no intervalo [-1, 1]
        call random_number(x)
        call random_number(y)
        x = 2.0 * x - 1.0
        y = 2.0 * y - 1.0

        ! Verifica se o ponto está dentro do círculo (x^2 + y^2 <= 1)
        if (x**2 + y**2 <= 1.0) then
            inside_circle = inside_circle + 1
        end if
    end do

    ! Calcula a proporção de pontos dentro do círculo
    pi_estimate = 4.0 * real(inside_circle) / real(num_samples)

    ! Imprime o valor estimado de pi
    print *, "Estimated value of pi:", pi_estimate
end program monte_carlo_pi