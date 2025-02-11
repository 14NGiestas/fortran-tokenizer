module tokenizer_token_type
    use iso_fortran_env, only: real64, int64
    implicit none
    private

    type, public :: token_t
        character(:), allocatable :: string
        character(:), allocatable :: type
        class(*), allocatable :: object
            !! Unlimited Polymorphic Token
    contains
        procedure :: write
        generic, public :: write(formatted) => write
    end type

contains

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(token_t), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        ! Produce the formatted output.
        write(unit, '(A)', iostat=iostat) '[ token_t '

        ! Build a representation for the token's object if it is allocated.
        if (allocated(self % object)) then
            select type(it => self % object)
            type is (character(*))
                write(unit,'(A)') "'" // trim(it) // "'"
            type is (real)
                write(unit, '(G0)') it
            type is (complex)
                write(unit, '(G0)') it
            type is (integer)
                write(unit, '(I0)') it
            type is (real(real64))
                write(unit, '(G0)') it
            type is (complex(real64))
                write(unit, '(G0)') it
            type is (integer(int64))
                write(unit, '(I0)') it
            class default
                write(unit,'("[DERIVED TYPE ",I0,"]")') sizeof(it)
            end select
        else
          if (allocated(self % type)) then
              write(unit, '(A)') trim(self % type)
              write(unit, '(" ")')
          end if
          if (allocated(self % string)) then
              write(unit, '(A)') "'"//trim(self % string)//"'"
          end if
        end if
        write(unit, '(A)') ' ]'
    end subroutine

end module
