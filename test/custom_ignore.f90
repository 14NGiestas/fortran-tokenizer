!file: test/custom_ignore.f90
program main
    use fortran_tokenizer
    implicit none

    character(*), parameter :: string = "hello | | duck | world duck"

    type(tokenizer_t) :: tokenizer
    type(token_list) :: tokens

    ! Perform the tokenization
    tokens = tokenizer % tokenize(string, validate=by_pipe, ignore=if_is_duck)

    ! Show the resultant tokens
    print "('string: ""',A,'""')", string
    print*, tokens ! ['hello', 'world']

contains

    logical function by_pipe(token) result(is_valid)
        !! Return .false. if token is a pipe ("|")
        character(*), intent(in) :: token
        is_valid = .not. scan(token, "| ") > 0
    end function

    logical function if_is_duck(token) result(is_valid)
        !! Return .true. if token is a duck
        character(*), intent(in) :: token
        is_valid = token == "duck"
    end function

end program
