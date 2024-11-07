module tokenizer_token_list
    use tokenizer_token_type
    implicit none
    private

    public :: token_t

    type :: item_t
        type(token_t), pointer :: content
        type(item_t), pointer :: next => null()
        type(item_t), pointer :: prev => null()
    end type

    type, public :: token_list
        type(item_t), pointer :: head => null()
        type(item_t), pointer :: tail => null()
        integer, private :: size = 0
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
        type(item_t), pointer :: new

        if (self % size == 0) then
            allocate(self % head)
            allocate(self % head % content, source=item)
            self % tail => self % head
            self % size = self % size + 1
            return
        end if

        allocate(self % tail % next)
        new => self % tail % next
        allocate(new % content, source=item)
        new % next => null()
        new % prev => self % tail
        self % tail => new
        self % size = self % size + 1
    end subroutine

    function get(self, idx, info)
        class(token_list) :: self
        type(token_t) :: get
        integer, intent(in) :: idx
        integer, optional :: info
        type(item_t), pointer :: it
        integer :: i

        if (assert(self % is_empty(), info)) then
            it => self % head
            do i=1,self % size
                if (i == idx) then
                    get = it % content
                    return
                end if
                it => it % next
            end do
        end if
    end function

    function pop(self, info)
        class(token_list) :: self
        type(token_t) :: pop
        integer, optional :: info
        type(item_t), pointer :: new_tail

        if (assert(self % is_empty(), info)) then
            pop = self % peek(info)
            new_tail => self % tail % prev
            deallocate(self % tail)
            self % tail => new_tail
            self % size = self % size - 1
        end if
    end function

    function peek(self, info)
        class(token_list) :: self
        type(token_t) :: peek
        integer, optional :: info

        if (assert(self % is_empty(), info)) then
            peek = self % tail % content
        end if
    end function

    logical function is_empty(self)
        class(token_list) :: self
        is_empty = self % size == 0
    end function

    integer function list_size(self)
        type(token_list) :: self
        list_size = self % size
    end function

    subroutine clear(self)
        class(token_list) :: self
        integer :: i
        type(item_t), pointer :: it
        it => self % head
        do i=1,self % size
            it => it % next
            deallocate(it % prev)
            nullify(it % prev)
        end do
        nullify(it)
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
        type(item_t), pointer :: it
        it => self % head
        write(unit,'("[")')
        if (size(self) > 0) then
            do i=1,self % size - 1
                write(unit,'(DT,", ")') it % content
                it => it % next
            end do

            write(unit,'(DT)') it % content
        end if
        write(unit,'("]")')
        nullify(it)
    end subroutine
end module
