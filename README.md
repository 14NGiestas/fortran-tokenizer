# Fortran Tokenizer

This package provides the basics of a tokenizer and it allow the user to easily customize it's behavior.

## Basic Usage

The user must provide a function that receives a `character(*), intent(in) :: token` and returns a boolean that checks if the token is valid or not.

```f90
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

```

## Getting Started

First get the code, by cloning the repo:

```sh
git clone https://github.com/14NGiestas/fortran-tokenizer.git
cd fortran-tokenizer 
```

## FPM

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

to use as a library in your project, include this in your `fpm.toml`

```toml
[ dependencies ]
fortran-tokenizer = { git = "https://github.com/14NGiestas/fortran-tokenizer.git" }
```
