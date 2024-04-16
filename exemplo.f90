program OpenGL_example
  use, intrinsic :: iso_c_binding

  interface
    subroutine glutInit(argc, argv) bind(c, name='glutInit')
      import :: c_int, c_ptr
      integer(c_int), value :: argc
      type(c_ptr), value :: argv
    end subroutine glutInit
    
    subroutine glutInitDisplayMode(mode) bind(c, name='glutInitDisplayMode')
      import :: c_int
      integer(c_int), value :: mode
    end subroutine glutInitDisplayMode
    
    subroutine glutInitWindowSize(width, height) bind(c, name='glutInitWindowSize')
      import :: c_int
      integer(c_int), value :: width, height
    end subroutine glutInitWindowSize
    
    subroutine glutCreateWindow(title) bind(c, name='glutCreateWindow')
      import :: c_char
      character(kind=c_char), dimension(*) :: title
    end subroutine glutCreateWindow
    
    subroutine glClearColor(red, green, blue, alpha) bind(c, name='glClearColor')
      import :: c_float
      real(c_float), value :: red, green, blue, alpha
    end subroutine glClearColor
    
    subroutine glClear(mask) bind(c, name='glClear')
      import :: c_int
      integer(c_int), value :: mask
    end subroutine glClear
    
    subroutine glBegin(mode) bind(c, name='glBegin')
      import :: c_int
      integer(c_int), value :: mode
    end subroutine glBegin
    
    subroutine glEnd() bind(c, name='glEnd')
    end subroutine glEnd
    
    subroutine glVertex2f(x, y) bind(c, name='glVertex2f')
      import :: c_float
      real(c_float), value :: x, y
    end subroutine glVertex2f
    
    subroutine glMatrixMode(mode) bind(c, name='glMatrixMode')
      import :: c_int
      integer(c_int), value :: mode
    end subroutine glMatrixMode
    
    subroutine glLoadIdentity() bind(c, name='glLoadIdentity')
    end subroutine glLoadIdentity
    
    subroutine glOrtho(left, right, bottom, top, near, far) bind(c, name='glOrtho')
      import :: c_double
      real(c_double), value :: left, right, bottom, top, near, far
    end subroutine glOrtho
    
    subroutine glFlush() bind(c, name='glFlush')
    end subroutine glFlush
    
    subroutine glutDisplayFunc(func) bind(c, name='glutDisplayFunc')
      import :: c_funptr
      type(c_funptr), value :: func
    end subroutine glutDisplayFunc
  end interface
  
  real(c_float) :: angle = 0.0

  ! Função para desenhar o triângulo
  contains
    subroutine drawTriangle()
      call glBegin(4) ! Modo GL_TRIANGLES
  
      ! Definição dos vértices do triângulo
      call glVertex2f(-0.5, -0.5)
      call glVertex2f(0.5, -0.5)
      call glVertex2f(0.0, 0.5)
  
      call glEnd()
    end subroutine drawTriangle

    subroutine drawText(x, y, text)
      real(c_float), intent(in) :: x, y
      character(len=*), intent(in) :: text
      integer(c_int) :: length, i

      length = len_trim(text)
      call glRasterPos2f(x, y)
      do i = 1, length
        call glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ichar(text(i:i)))
      end do
    end subroutine drawText

    ! Função para exibir a cena
    subroutine display()
      call glClear(16640) ! GL_COLOR_BUFFER_BIT
  
      ! Rotação contínua do triângulo
      angle = angle + 0.01
  
      ! Define a matriz de projeção
      call glMatrixMode(5889) ! GL_PROJECTION
      call glLoadIdentity()
      call glOrtho(-1.0d0, 1.0d0, -1.0d0, 1.0d0, -1.0d0, 1.0d0)
  
      ! Define a matriz do modelo de visualização
      call glMatrixMode(5888) ! GL_MODELVIEW
      call glLoadIdentity()
      
      ! Exibe o texto "triângulo"
      call drawText(-0.8, 0.8, "Triângulo")
  
      ! Desenha o triângulo
      call glRotatef(angle, 0.0, 0.0, 1.0)
      call drawTriangle()
  
      call glFlush()
    end subroutine display
  end program OpenGL_example