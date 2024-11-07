program main
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tok
    type(token_list) :: tokens

    ! Bind a global validator to reuse the same function multiple times
    tok % validator => by_expression

    tokens = tok % tokenize(" (1 + 2) /2")
    print*, tokens

    tokens = tok % tokenize(" (1 + 2) /2    ")
    print*, tokens

    ! You can use a local validator that will be used instead the global
    tokens = tok % tokenize("  hello  i am a robot", by_whitespace)
    print*, tokens

contains

    logical function by_whitespace(token)
        !! If it contains whitespace it is not valid
        character(*), intent(in) :: token
        by_whitespace = .not. scan(token, " ") > 0
    end function

    logical function by_expression(token)
        character(*), intent(in) :: token
        by_expression = by_whitespace(token)
        by_expression = by_expression &
        .and. any(token == [ &
            "(", ")",           &
            "/", "+", "-", "*", &
            "1", "2"            &
        ])
    end function

end program
