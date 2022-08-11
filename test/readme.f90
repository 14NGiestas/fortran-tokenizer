program main
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tokenizer
    type(token_list) :: tokens
    ! Bind the behaviour here
    tokenizer % validate_token => by_whitespace
    ! Perform the tokenization
    tokens = tokenizer % tokenize("hello world")
    ! Show the resultant tokens
    print*, tokens ! ['hello', 'world']
contains
    logical function by_whitespace(token)
        !! The token will be valid if it do not contain " "
        character(*), intent(in) :: token
        by_whitespace = .not. scan(token, " ") > 0
    end function
end program
