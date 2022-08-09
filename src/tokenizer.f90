module fortran_tokenizer
    use iso_fortran_env
    use tokenizer_token_list
    use tokenizer_token_type
    implicit none
    private

    public :: token_t, token_list, size

    type, public :: tokenizer_t
        procedure(interface_validate), pointer, nopass :: validate_token => null()
    contains
        procedure :: tokenize
    end type

    abstract interface
        logical function interface_validate(token)
            character(*), intent(in) :: token
        end function
    end interface

contains

    function tokenize(self, input) result(tokens)
        !! Split a input string in tokens
        class(tokenizer_t) :: self
        character(*) :: input
        integer :: start, ending
        logical :: is_valid
        type(token_list) :: tokens
        type(token_t) :: new_token

        start = 0
        do while(start < len(input))
            ! Loop until it starts in a valid token
            start = start + 1
            is_valid = self % validate_token(input(start:start))
            if (.not. is_valid) cycle
            ! Greedly consume tokens until find a invalid one
            do ending = start, len(input)
                is_valid = self % validate_token(input(start:ending))
                if (.not. is_valid) exit
            end do
            ! Every consumed token previously was valid
            ! Push the slice into token list
            new_token % string = input(start:ending-1)
            start = ending-1
            call tokens % append(new_token)
        end do
    end function

end module
