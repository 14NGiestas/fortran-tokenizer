!file: test/bind_global.f90
program main
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tokenizer
    type(token_list) :: tokens

    tokenizer % validator => global_behavior

    ! Perform the tokenization
    tokens = tokenizer % tokenize("hello| world")
    print*, tokens ! ['hello', 'world']

    ! Perform another tokenization
    tokens = tokenizer % tokenize("hello |world")
    print*, tokens ! ['hello', 'world']

    ! Perform the tokenization, using a local behaviour
    tokens = tokenizer % tokenize("hello| |world|token with spaces|", validate=local_behavior)

    ! Show the resultant tokens
    print*, tokens ! ['hello', ' ', 'world', 'token with spaces']

    ! Bind global ignore behavior
    tokenizer % ignore => ignore_tokens

    ! Perform the tokenization, using a local behaviour, but any token containg a space will ignored
    tokens = tokenizer % tokenize("hello| |world|tokens with spaces|", validate=local_behavior)

    ! Show the resultant tokens
    print*, tokens ! ['hello', 'world']

contains

    logical function global_behavior(token) result(is_valid)
        !! The token will be valid if it do not contain " " or "|"
        character(*), intent(in) :: token
        is_valid = .not. scan(token, "| ") > 0
    end function

    logical function local_behavior(token) result(is_valid)
        !! The token will be valid if it do not contain "|"
        character(*), intent(in) :: token
        is_valid = .not. scan(token, "|") > 0
    end function

    logical function ignore_tokens(token) result(is_valid)
        !! The token will be ignored if contains any spaces
        character(*), intent(in) :: token
        is_valid = scan(token, " ") > 0
    end function

end program
