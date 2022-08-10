module tokenizer_token_list
    use tokenizer_token_type
    implicit none
    private

    public :: token_t

    type, public :: token_list
        type(token_t), allocatable :: items(:)
        integer, private :: num_items = 0
    contains
        procedure :: get
        procedure :: pop
        procedure :: peek
        procedure :: clear
        procedure :: append
        procedure :: is_empty
        procedure, private :: write
        generic, public :: write(formatted) => write
    end type

    interface size
        module procedure list_size
    end interface
    public :: size

contains

    subroutine append(self, item)
        class(token_list) :: self
        type(token_t) :: item

        if (.not. allocated(self % items)) then
            self % items = [ item ]
        else
            self % items = [ self % items, item ]
        end if

        self % num_items = self % num_items + 1
    end subroutine

    function get(self, idx, info)
        class(token_list) :: self
        type(token_t) :: get
        integer, intent(in) :: idx
        integer, optional :: info

        if (assert(self % is_empty(), info)) then
            get = self % items(idx)
        end if
    end function

    function pop(self, info)
        class(token_list) :: self
        type(token_t) :: pop
        integer, optional :: info

        if (assert(self % is_empty(), info)) then
            pop = self % peek(info)

            self % num_items = self % num_items - 1
            self % items = self % items(:self % num_items)
        end if
    end function

    function peek(self, info)
        class(token_list) :: self
        type(token_t) :: peek
        integer, optional :: info

        if (assert(self % is_empty(), info)) then
            peek = self % items(self % num_items)
        end if
    end function

    logical function is_empty(self)
        class(token_list) :: self
        is_empty = self % num_items == 0
    end function

    integer function list_size(self)
        type(token_list) :: self
        list_size = self % num_items
    end function

    subroutine clear(self)
        class(token_list) :: self
        if (allocated(self % items)) then
            deallocate(self % items)
            allocate(self % items(0))
        end if
    end subroutine

    logical function assert(condition, info)
        logical, intent(in) :: condition
        integer, optional :: info

        assert = .true.
        if (present(info)) then
            info = 0
            if (condition) then
                info = -1
                assert = .false.
            end if
        else
            if (condition) error stop
        end if
    end function

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(token_list), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in)  :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        integer :: i

        write(unit,'("[")')
        if (size(self) > 0) then
            do i=1,size(self)-1
                write(unit,'(DT,", ")') self % items(i)
            end do

            write(unit,'(DT)') self % items(i)
        end if
        write(unit,'("]")')
    end subroutine
end module
