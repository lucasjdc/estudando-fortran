program ListaLigada
    implicit none

    ! Definição da estrutura do nó da lista
    type Node
        integer :: data
        type(Node), pointer :: next
    end type Node

    ! Declaração de variáveis
    type(Node), pointer :: head, current, newNode
    integer :: value
    character(1) :: opcao

    ! Inicializa a lista como vazia
    head => NULL()

    ! Menu
    do
        print *, "Escolha uma opção:"
        print *, "1. Inserir elemento"
        print *, "2. Remover elemento"
        print *, "3. Visualizar lista"
        print *, "4. Sair"
        read(*, '(A)') opcao

        select case (opcao)
        case ('1')
            ! Inserir elemento
            call inserirElemento(head)
        case ('2')
            ! Remover elemento
            call removerElemento(head)
        case ('3')
            ! Visualizar lista
            call visualizarLista(head)
        case ('4')
            ! Sair do programa
            exit
        case default
            print *, "Opção inválida! Tente novamente."
        end select
    end do

contains

    ! Sub-rotina para inserir um elemento na lista
    subroutine inserirElemento(head)
        type(Node), pointer :: head
        type(Node), pointer :: current, newNode
        integer :: value

        print *, "Digite um valor inteiro para inserir na lista:"
        read(*, *) value

        ! Cria um novo nó
        allocate(newNode)
        newNode%data = value
        newNode%next => NULL()

        ! Adiciona o novo nó à lista
        if (.not. associated(head)) then
            head => newNode
        else
            current => head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => newNode
        end if
    end subroutine inserirElemento

    ! Sub-rotina para remover um elemento da lista
    subroutine removerElemento(head)
        type(Node), pointer :: head
        type(Node), pointer :: current, previous
        integer :: value

        if (.not. associated(head)) then
            print *, "A lista está vazia."
            return
        end if

        print *, "Digite o valor inteiro a ser removido:"
        read(*, *) value

        current => head
        previous => NULL()

        do while (associated(current))
            if (current%data == value) then
                ! Remove o elemento
                if (associated(previous)) then
                    previous%next => current%next
                else
                    head => current%next
                end if
                deallocate(current)
                print *, "Elemento removido com sucesso."
                return
            end if
            previous => current
            current => current%next
        end do

        print *, "Elemento não encontrado na lista."
    end subroutine removerElemento

    ! Sub-rotina para visualizar os elementos da lista
    subroutine visualizarLista(head)
        type(Node), pointer :: head
        type(Node), pointer :: current

        if (.not. associated(head)) then
            print *, "A lista está vazia."
            return
        end if

        print *, "Elementos na lista:"
        current => head
        do while (associated(current))
            print *, current%data
            current => current%next
        end do
    end subroutine visualizarLista

end program ListaLigada
