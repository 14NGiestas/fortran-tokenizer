module tokenizer_token_type
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
        integer, intent(in)  :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (allocated(self % object)) then
            select type(it => self % object)
            type is (character(*))
                write(unit,'("''",g0,"''")') it
            type is (real)
                write(unit,'(g0)') it
            type is (complex)
                write(unit,'(g0)') it
            type is (integer)
                write(unit,'(g0)') it
            class default
                write(unit,'("[DERIVED TYPE ",I0,"]")') sizeof(it)
            end select
        else
            write(unit,'("''",g0,"''")') self % string
        end if
    end subroutine

end module
