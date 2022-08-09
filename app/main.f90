program main
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tok
    type(token_list) :: tokens

    tok % validate_token => by_whitespace

    tokens = tok % tokenize("  hello  i am a robot")
    print*, tokens

    tok % validate_token => by_expression

    tokens = tok % tokenize(" (1 + 2) /2")
    print*, tokens

    tokens = tok % tokenize(" (1 + 2) /2    ")
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
