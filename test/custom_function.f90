!file: test/custom_function.f90
program main
    use fortran_tokenizer
    implicit none

    character, parameter :: TABS = char(9)
    character, parameter :: IDENT = repeat(TABS,4)
    character(*), parameter :: string = TABS//"hello "//IDENT//"world"

    type(tokenizer_t) :: tokenizer
    type(token_list) :: tokens

    ! Perform the tokenization
    tokens = tokenizer % tokenize(string, validate=by_whitespace)

    ! Show the resultant tokens
    print "('string: ""',A,'""')", string
    print*, tokens ! ['hello', 'world']

contains

    logical function by_whitespace(token) result(is_valid)
        !! The token will be valid if it do not contain " " or TABS
        character(*), intent(in) :: token
        is_valid = .not. scan(token, " "//TABS) > 0
    end function

end program
