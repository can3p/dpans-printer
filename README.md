
# HTML generator for common lisp spec

The generator is based on [dpans-parser fork](https://github.com/can3p/dpans-parser). The idea
is to generate documentation in a same way as common lisp hyper spec is generated but without
any license restrictions.

## Usage

    # install dpans-parser into some folder where it can be found by quicklisp
    $ roswell install can3p/dpans-printer
    $ dpans-printer
    # documentation should appear in a build folder

## License

All the code except any parts taken from original dpans-parser project and spec itself are in
Public Domain. Enjoy!
