program check
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tok
    type(token_list) :: tokens
    type(token_t) :: token
    character(:), allocatable :: expected(:)
    integer :: i

    expected = ["hello", &
                "i    ", &
                "am   ", &
                "a    ", &
                "robot"]
    tokens = tok % tokenize("  hello  i am a robot")
    call assert(size(tokens) == 5)
    do i=1,size(tokens)
        token = tokens % get(i)
        call assert(token % string == expected(i))
    end do

    expected = ["(", "1", "+", "2", ")", "/", "2"]

    tokens = tok % tokenize(" (1 + 2) /2", by_expression)
    call assert(size(tokens) == 7)
    do i=1,size(tokens)
        token = tokens % get(i)
        call assert(token % string == expected(i))
    end do

    tokens = tok % tokenize("(1+2)/2", by_expression)
    call assert(size(tokens) == 7)
    do i=1,size(tokens)
        token = tokens % get(i)
        call assert(token % string == expected(i))
    end do


    block
        integer :: info

        do i=1,size(tokens)
            token % string = expected(i)
            call tokens % append(token)
        end do

        token = tokens % peek()
        call assert(token % string == expected(size(expected)))

        do i=1,size(tokens)
            token = tokens % pop(info)
            call assert(info == 0)
        end do

        token = tokens % pop(info)
        call assert(info == -1)
    end block
contains

    elemental subroutine assert(test)
        logical, intent(in) :: test
        if (.not. test) error stop
    end subroutine

    logical function by_whitespace(token)
        !! If it contains whitespace it is not valid
        character(*), intent(in) :: token
        by_whitespace = .not. scan(token, " ") > 0
    end function

    logical function by_expression(token)
        character(*), intent(in) :: token
        by_expression = by_whitespace(token) &
        .and. any(token == [ &
            "(", ")",           &
            "/", "+", "-", "*", &
            "1", "2"            &
        ])
    end function

end program
