program main
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tokenizer
    type(token_list) :: tokens

    ! Perform the tokenization
    tokens = tokenizer % tokenize("hello world")
    print*, tokens ! ['hello', 'world']
end program
