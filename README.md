# Fortran Tokenizer

This package provides a basic tokenizer and it allow users to easily customize it's behavior.

## Use in your project with fpm

Include this in your `fpm.toml` under the dependencies section:

```toml
[ dependencies ]
fortran-tokenizer = { git = "https://github.com/14NGiestas/fortran-tokenizer.git" }
```

## Basic Usage
### By default, a valid token do not contain spaces

```f90
!file: test/default.f90
program main
    use fortran_tokenizer
    implicit none
    type(tokenizer_t) :: tokenizer
    type(token_list) :: tokens

    ! Perform the tokenization
    tokens = tokenizer % tokenize("hello world")
    print*, tokens ! ['hello', 'world']
end program
```

### Tokenize by custom function

You can override the default behavior by defining a function that receives a `character(*), intent(in) :: token`
and returns `.true.` if the string received by the function is a valid token, `.false` otherwise.

```f90
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
```

you can also pass a function with the same interface that once a token is parsed it can be ignored completely

```f90
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
```


### Bind a global rule

To reuse the same tokenizer rules you can bind global functions

```f90
!file: test/bind_global_function.f90
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
```

## Contributing

### Getting the source

First get the code, by cloning the repo:

```sh
git clone https://github.com/14NGiestas/fortran-tokenizer.git
cd fortran-tokenizer
```

## Building with FPM

This project was designed to be built using the [Fortran Package Manager](https://github.com/fortran-lang/fpm).
Follow the directions on that page to install FPM if you haven't already.

To build and run the program (provided as a example), type:

```sh
fpm run
```

to run the tests, type

```sh
fpm test
```
