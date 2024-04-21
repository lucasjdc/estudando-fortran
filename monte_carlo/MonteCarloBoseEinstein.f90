program MonteCarloBoseEinstein
    use omp_lib
    implicit none
    integer, parameter :: N = 1000  ! Número de partículas
    real :: L = 10.0                ! Tamanho do sistema
    real :: beta = 1.0              ! Parâmetro beta (inverso da temperatura)
    real :: epsilon = 1.0           ! Parâmetro de interação
    real, dimension(N) :: x, y, z   ! Posições das partículas
    real :: dx, dy, dz              ! Mudanças nas posições
    real :: energy_old, energy_new, prob_accept, final_energy
    integer :: i, j, steps
    real :: random_real

    ! Sub-rotina para inicializar as posições das partículas
    call initialize_positions()

    ! Simulação de Monte Carlo
    !$OMP PARALLEL DO PRIVATE(i, j, dx, dy, dz, random_real, energy_old, energy_new, prob_accept) SHARED(x, y, z)
    do steps = 1, 100000
        ! Escolhe uma partícula aleatória
        call random_number(random_real)
        i = int(random_real * N) + 1

        ! Salva a energia antes da mudança
        energy_old = total_energy()

        ! Faz uma mudança na posição da partícula
        call random_number(random_real)
        dx = 0.1 * (random_real - 0.5)
        call random_number(random_real)
        dy = 0.1 * (random_real - 0.5)
        call random_number(random_real)
        dz = 0.1 * (random_real - 0.5)
        x(i) = x(i) + dx
        y(i) = y(i) + dy
        z(i) = z(i) + dz

        ! Calcula a energia depois da mudança
        energy_new = total_energy()

        ! Calcula a probabilidade de aceitar a mudança
        prob_accept = exp(-beta * (energy_new - energy_old))

        ! Decide se aceita ou não a mudança
        call random_number(random_real)
        if (random_real < prob_accept) then
            ! Aceita a mudança
            ! Não faz nada
        else
            ! Rejeita a mudança e restaura as posições anteriores
            x(i) = x(i) - dx
            y(i) = y(i) - dy
            z(i) = z(i) - dz
        end if
    end do
    !$OMP END PARALLEL DO

    ! Cálculo da energia final do sistema
    final_energy = total_energy()

    ! Impressão dos resultados
    print *, 'Energia final:', final_energy

contains

    ! Sub-rotina para inicializar as posições das partículas
    subroutine initialize_positions()
        integer :: i
        real :: random_real
        do i = 1, N
            call random_number(random_real)
            x(i) = L * random_real
            call random_number(random_real)
            y(i) = L * random_real
            call random_number(random_real)
            z(i) = L * random_real
        end do
    end subroutine initialize_positions

    ! Função para calcular a energia total do sistema
    real function total_energy()
        integer :: i, j
        real :: energy
        energy = 0.0
        do i = 1, N - 1
            do j = i + 1, N
                energy = energy + pair_energy(i, j)
            end do
        end do
    end function total_energy

    ! Função para calcular a energia de um par de partículas
    real function pair_energy(i, j)
        integer :: i, j
        real :: r
        r = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2 + (z(i) - z(j))**2)
        if (r < 1e-12) then
            pair_energy = 0.0
        else
            pair_energy = 4.0 * epsilon * ((1.0 / r)**12 - (1.0 / r)**6)
        end if
    end function pair_energy

end program MonteCarloBoseEinstein
