module fortran_tokenizer
    use iso_fortran_env
    use tokenizer_token_list
    use tokenizer_token_type
    implicit none
    private

    public :: token_t, token_list, size

    type, public :: tokenizer_t
        procedure(interface_validate), pointer, nopass :: validator => null()
        procedure(interface_validate), pointer, nopass :: ignore => null()
    contains
        procedure :: tokenize
    end type

    abstract interface
        logical function interface_validate(token)
            character(*), intent(in) :: token
        end function
    end interface

contains

    logical function has_no_space(token) result(is_valid)
        !! If it contains space it is not valid
        character(*), intent(in) :: token
        is_valid = .not. scan(token, " ") > 0
    end function

    logical function do_not_ignore(token) result(condition)
        !! Will not ignore any valid token
        character(*), intent(in) :: token
        condition = .false.
    end function

    function tokenize(self, input, validate, ignore) result(tokens)
        !! Split a input string in tokens
        class(tokenizer_t) :: self
        character(*) :: input
        procedure(interface_validate), optional :: validate
        procedure(interface_validate), optional :: ignore
        integer :: start, ending
        logical :: is_valid
        type(token_list) :: tokens
        type(token_t) :: new_token

        procedure(interface_validate), pointer :: validate_token
        procedure(interface_validate), pointer :: ignore_token

        if (present(validate)) then
            validate_token => validate
        else if (associated(self % validator)) then
            validate_token => self % validator
        else
            validate_token => has_no_space
        end if

        if (present(ignore)) then
            ignore_token => ignore
        else if (associated(self % ignore)) then
            ignore_token => self % ignore
        else
            ignore_token => do_not_ignore
        end if

        start = 0
        do while(start < len(input))
            ! Loop until it starts in a valid token
            start = start + 1
            is_valid = validate_token(input(start:start))
            if (.not. is_valid) cycle
            ! Greedly consume tokens until find a invalid one
            do ending = start, len(input)
                is_valid = validate_token(input(start:ending))
                if (.not. is_valid) exit
            end do
            ! Check if the valid token is ignored
            if (ignore_token(input(start:ending-1))) then
                start = ending
                cycle
            end if
            ! Every consumed token previously was valid
            ! Push the slice into token list
            new_token % string = input(start:ending-1)
            start = ending-1
            call tokens % append(new_token)
        end do
    end function

end module
